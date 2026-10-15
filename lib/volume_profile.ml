open! Core

let day_ns = Time_ns.Span.to_int_ns Time_ns.Span.day

module Stable = struct
  module V1 = struct
    type t =
      { bucket_ns : int
      ; weights : float array
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

include Stable.V1

let check_bucket bucket =
  let b = Time_ns.Span.to_int_ns bucket in
  if b <= 0 || day_ns mod b <> 0
  then raise_s [%message "Volume_profile: bucket must divide a day" (bucket : Time_ns.Span.t)];
  b
;;

let default_bucket = Time_ns.Span.of_int_sec 300

let uniform ?(bucket = default_bucket) () =
  let b = check_bucket bucket in
  let n = day_ns / b in
  { bucket_ns = b; weights = Array.create ~len:n (1. /. Float.of_int n) }
;;

let of_volumes ~bucket volumes =
  let b = check_bucket bucket in
  let n = day_ns / b in
  if Array.length volumes <> n
  then raise_s [%message "Volume_profile.of_volumes: one entry per bucket" (n : int)];
  let total = Array.sum (module Float) volumes ~f:(fun v -> Float.max v 0.) in
  if Float.(total <= 0.)
  then uniform ~bucket ()
  else (
    (* a floor of 1% of the mean bucket, then renormalise *)
    let floor = total /. Float.of_int n *. 0.01 in
    let floored = Array.map volumes ~f:(fun v -> Float.max (Float.max v 0.) floor) in
    let total = Array.sum (module Float) floored ~f:Fn.id in
    { bucket_ns = b; weights = Array.map floored ~f:(fun v -> v /. total) })
;;

let of_trades ?(bucket = default_bucket) trades =
  let b = check_bucket bucket in
  let n = day_ns / b in
  let volumes = Array.create ~len:n 0. in
  List.iter trades ~f:(fun (tr : Market_data.Trade.t) ->
    let ns = Time_ns.to_int_ns_since_epoch tr.exch_ts in
    let i = (ns mod day_ns + day_ns) mod day_ns / b in
    volumes.(i) <- volumes.(i) +. Market_data.Size.to_float tr.size);
  of_volumes ~bucket volumes
;;

let bucket t = Time_ns.Span.of_int_ns t.bucket_ns
let weights t = Array.copy t.weights

(* Cumulative share of volume from the epoch to [x]: whole days count 1 each. *)
let cumulative t x =
  let days = Int.( / ) (if x >= 0 then x else x - day_ns + 1) day_ns in
  let within = x - (days * day_ns) in
  let i = within / t.bucket_ns in
  let acc = ref 0. in
  for j = 0 to i - 1 do
    acc := !acc +. t.weights.(j)
  done;
  let frac = Float.of_int (within - (i * t.bucket_ns)) /. Float.of_int t.bucket_ns in
  Float.of_int days +. !acc +. (t.weights.(i) *. frac)
;;

let weight_between t ~start_ns ~stop_ns =
  if stop_ns <= start_ns then 0. else cumulative t stop_ns -. cumulative t start_ns
;;

let fraction_done t ~start_ns ~stop_ns ~now_ns =
  if now_ns <= start_ns
  then 0.
  else if now_ns >= stop_ns
  then 1.
  else (
    let total = weight_between t ~start_ns ~stop_ns in
    if Float.(total <= 0.)
    then Float.of_int (now_ns - start_ns) /. Float.of_int (stop_ns - start_ns)
    else Float.clamp_exn (weight_between t ~start_ns ~stop_ns:now_ns /. total) ~min:0. ~max:1.)
;;
