open! Core
module Price = Market_data.Price
module Size = Market_data.Size
module Side = Market_data.Side

module Stable = struct
  module V1 = struct
    type t =
      { bid_px : Price.Stable.V1.t array
      ; bid_sz : Size.Stable.V1.t array
      ; ask_px : Price.Stable.V1.t array
      ; ask_sz : Size.Stable.V1.t array
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

include Stable.V1

let empty = { bid_px = [||]; bid_sz = [||]; ask_px = [||]; ask_sz = [||] }

let check_side ~name ~better levels =
  List.iter levels ~f:(fun (_, sz) ->
    if not (Size.is_positive sz)
    then raise_s [%message "Book.create: non-positive size" name (sz : Size.t)]);
  ignore
    (List.fold levels ~init:None ~f:(fun prev (px, _) ->
       (match prev with
        | Some p when not (better p px) ->
          raise_s [%message "Book.create: levels not best first" name (px : Price.t)]
        | _ -> ());
       Some px)
     : Price.t option)
;;

let create ~bids ~asks =
  check_side ~name:"bids" ~better:Price.( > ) bids;
  check_side ~name:"asks" ~better:Price.( < ) asks;
  (match bids, asks with
   | (b, _) :: _, (a, _) :: _ when Price.(b >= a) ->
     raise_s [%message "Book.create: crossed" (b : Price.t) (a : Price.t)]
   | _ -> ());
  { bid_px = Array.of_list_map bids ~f:fst
  ; bid_sz = Array.of_list_map bids ~f:snd
  ; ask_px = Array.of_list_map asks ~f:fst
  ; ask_sz = Array.of_list_map asks ~f:snd
  }
;;

let of_levels levels =
  List.filter_map levels ~f:(fun (l : Market_data.L2_update.Level.t) ->
    if Size.is_positive l.size then Some (l.price, l.size) else None)
;;

(* A recorded book can be momentarily crossed (a delta applied before the matching
   removal); keep the side whose top is newer is not knowable here, so drop the crossed
   levels of both sides instead of refusing the snapshot. *)
let uncross bids asks =
  match bids, asks with
  | (b, _) :: _, (a, _) :: _ when Price.(b >= a) ->
    ( List.filter bids ~f:(fun (p, _) -> Price.(p < a))
    , List.filter asks ~f:(fun (p, _) -> Price.(p > b)) )
  | _ -> bids, asks
;;

let of_l2_book ?(depth = 20) book =
  let bids = of_levels (Market_data.L2_book.bids ~depth book) in
  let asks = of_levels (Market_data.L2_book.asks ~depth book) in
  let bids, asks = uncross bids asks in
  create ~bids ~asks
;;

let of_bbo (b : Market_data.Bbo.t) =
  let side px sz = if Size.is_positive sz then [ px, sz ] else [] in
  let bids, asks = uncross (side b.bid_price b.bid_size) (side b.ask_price b.ask_size) in
  create ~bids ~asks
;;

let[@zero_alloc] levels t (side : Side.t) =
  match side with
  | Buy -> Array.length t.bid_px
  | Sell -> Array.length t.ask_px
;;

let[@zero_alloc] px t (side : Side.t) i =
  match side with
  | Buy -> t.bid_px.(i)
  | Sell -> t.ask_px.(i)
;;

let[@zero_alloc] sz t (side : Side.t) i =
  match side with
  | Buy -> t.bid_sz.(i)
  | Sell -> t.ask_sz.(i)
;;

let[@zero_alloc] best_bid t =
  if Array.length t.bid_px = 0 then Null else This (Array.unsafe_get t.bid_px 0)
;;

let[@zero_alloc] best_ask t =
  if Array.length t.ask_px = 0 then Null else This (Array.unsafe_get t.ask_px 0)
;;

let[@zero_alloc] touch t (side : Side.t) =
  match side with
  | Buy -> best_ask t
  | Sell -> best_bid t
;;

let[@zero_alloc] mid t =
  match best_bid t, best_ask t with
  | This b, This a ->
    This (Price.of_units ((Price.to_units b + Price.to_units a) / 2))
  | (This _ as one), Null | Null, (This _ as one) -> one
  | Null, Null -> Null
;;

let[@zero_alloc] size_at t (side : Side.t) target =
  let pxs, szs =
    match side with
    | Buy -> t.bid_px, t.bid_sz
    | Sell -> t.ask_px, t.ask_sz
  in
  let mutable found = Size.zero in
  for i = 0 to Array.length pxs - 1 do
    if Price.equal (Array.unsafe_get pxs i) target then found <- Array.unsafe_get szs i
  done;
  found
;;

let is_empty t = Array.length t.bid_px = 0 && Array.length t.ask_px = 0
