open! Core
module Price = Market_data.Price
module Size = Market_data.Size
module Side = Market_data.Side

(* The largest price a buyer is ever asked to accept, and zero for a seller. Kept well
   inside [max_int] so notional arithmetic on it cannot overflow. *)
let no_limit (side : Side.t) =
  match side with
  | Buy -> Price.of_units (1 lsl 60)
  | Sell -> Price.zero
;;

let[@zero_alloc] limit_from ~reference ~(side : Side.t) ~bps =
  let r = Price.to_units reference in
  let move = (r / 10_000 * bps) + (r mod 10_000 * bps / 10_000) in
  match side with
  | Buy -> Price.of_units (r + move)
  | Sell -> Price.of_units (Int.max 0 (r - move))
;;

(* A taker on [side] takes the opposite side's resting orders: asks for a buy. *)
let[@inline] resting (side : Side.t) : Side.t =
  match side with
  | Buy -> Sell
  | Sell -> Buy
;;

let[@inline] within (side : Side.t) px ~limit =
  match side with
  | Buy -> Price.( <= ) px limit
  | Sell -> Price.( >= ) px limit
;;

let[@zero_alloc] walk book ~side ~size ~limit =
  let r = resting side in
  let n = Book.levels book r in
  let mutable left = Size.to_units size in
  let mutable notional = 0 in
  let mutable i = 0 in
  while left > 0 && i < n && within side (Book.px book r i) ~limit do
    let take = Int.min left (Size.to_units (Book.sz book r i)) in
    notional
    <- notional + Fixed_decimal.mul_units (Price.to_units (Book.px book r i)) take;
    left <- left - take;
    i <- i + 1
  done;
  #(Size.of_units (Size.to_units size - left), notional)
;;

let[@zero_alloc] depth_within book ~side ~bps =
  match Book.mid book, Book.best_bid book, Book.best_ask book with
  | This mid, This _, This _ ->
    let limit = limit_from ~reference:mid ~side ~bps in
    let r = resting side in
    let n = Book.levels book r in
    let mutable total = 0 in
    let mutable i = 0 in
    while i < n && within side (Book.px book r i) ~limit do
      total <- total + Size.to_units (Book.sz book r i);
      i <- i + 1
    done;
    Size.of_units total
  | _ -> Size.zero
;;

let adverse_bps ~(side : Side.t) ~reference px =
  let r = Price.to_float reference in
  if Float.(r <= 0.)
  then 0.
  else (
    let d = (Price.to_float px -. r) /. r *. 10_000. in
    match side with
    | Buy -> d
    | Sell -> -.d)
;;

module Estimate = struct
  type t =
    { filled : Size.t
    ; avg_px : Price.t
    ; worst_px : Price.t
    ; mid : Price.t option
    ; touch_px : Price.t
    ; slippage_bps : float
    ; touch_bps : float
    ; levels : int
    ; exhausted : bool
    }
  [@@deriving sexp, compare, equal]
end

let estimate ?limit book ~side ~size =
  let limit = Option.value limit ~default:(no_limit side) in
  let r = resting side in
  let n = Book.levels book r in
  let reached = ref 0 in
  while !reached < n && within side (Book.px book r !reached) ~limit do
    incr reached
  done;
  let #(filled, notional) = walk book ~side ~size ~limit in
  if Size.is_zero filled || !reached = 0
  then None
  else (
    let avg_px =
      Price.of_units (Fixed_decimal.div_units notional (Size.to_units filled))
    in
    (* the last level actually used: walk the cumulative size again *)
    let levels =
      let rec go i acc =
        if i >= !reached
        then i
        else (
          let acc = acc + Size.to_units (Book.sz book r i) in
          if acc >= Size.to_units filled then i + 1 else go (i + 1) acc)
      in
      go 0 0
    in
    let touch_px = Book.px book r 0 in
    let mid =
      match Book.mid book, Book.best_bid book, Book.best_ask book with
      | This m, This _, This _ -> Some m
      | _ -> None
    in
    let reference = Option.value mid ~default:touch_px in
    Some
      { Estimate.filled
      ; avg_px
      ; worst_px = Book.px book r (levels - 1)
      ; mid
      ; touch_px
      ; slippage_bps = adverse_bps ~side ~reference avg_px
      ; touch_bps = adverse_bps ~side ~reference:touch_px avg_px
      ; levels
      ; exhausted = Size.( < ) filled size
      })
;;
