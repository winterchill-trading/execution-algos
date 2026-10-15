open! Core
module Price = Market_data.Price
module Size = Market_data.Size
module Side = Market_data.Side

module Outcome = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Filled
        | Deadline
        | Cancelled
        | Unfilled
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1

  let to_string = function
    | Filled -> "filled"
    | Deadline -> "deadline"
    | Cancelled -> "cancelled"
    | Unfilled -> "unfilled"
  ;;
end

module Stable = struct
  module V1 = struct
    type t =
      { algo : string
      ; side : Side.Stable.V1.t
      ; target : Size.Stable.V1.t
      ; filled : Size.Stable.V1.t
      ; avg_px : Price.Stable.V1.t
      ; arrival_px : Price.Stable.V1.t
      ; end_px : Price.Stable.V1.t
      ; arrival_ns : int
      ; end_ns : int
      ; children : int
      ; maker_filled : Size.Stable.V1.t
      ; fees : float
      ; arrival_slippage_bps : float
      ; shortfall : float
      ; shortfall_bps : float
      ; outcome : Outcome.Stable.V1.t
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

include Stable.V1

let sign (side : Side.t) =
  match side with
  | Buy -> 1.
  | Sell -> -1.
;;

let derive t ~notional =
  let s = sign t.side in
  let a = Price.to_float t.arrival_px in
  let q = Size.to_float t.filled in
  let u = Size.to_float t.target -. q in
  let e = Price.to_float t.end_px in
  let execution = if Float.(q > 0.) then s *. (notional -. (q *. a)) else 0. in
  let opportunity = s *. u *. (e -. a) in
  let shortfall = execution +. opportunity +. t.fees in
  let target_notional = Size.to_float t.target *. a in
  { t with
    arrival_slippage_bps =
      (if Float.(q > 0.) && Float.(a > 0.)
       then s *. ((notional /. q) -. a) /. a *. 10_000.
       else 0.)
  ; shortfall
  ; shortfall_bps =
      (if Float.(target_notional > 0.)
       then shortfall /. target_notional *. 10_000.
       else 0.)
  }
;;

let compute
  ~algo
  ~side
  ~target
  ~filled
  ~notional
  ~maker_filled
  ~fees
  ~arrival_px
  ~end_px
  ~arrival_ns
  ~end_ns
  ~children
  ~outcome
  =
  let avg_px =
    if Size.is_positive filled
    then
      (* [notional] is a float sum of exact products; round to the price unit *)
      Price.of_units (Float.iround_nearest_exn (notional /. Size.to_float filled *. 1e8))
    else Price.zero
  in
  derive
    { algo
    ; side
    ; target
    ; filled
    ; avg_px
    ; arrival_px
    ; end_px
    ; arrival_ns
    ; end_ns
    ; children
    ; maker_filled
    ; fees
    ; arrival_slippage_bps = 0.
    ; shortfall = 0.
    ; shortfall_bps = 0.
    ; outcome
    }
    ~notional
;;

let with_end_px t end_px =
  let notional = Size.to_float t.filled *. Price.to_float t.avg_px in
  derive { t with end_px } ~notional
;;

module Totals = struct
  type t =
    { parents : int
    ; filled_parents : int
    ; target_notional : float
    ; filled_notional : float
    ; fees : float
    ; shortfall : float
    ; slippage_cost : float
    ; children : int
    }
  [@@deriving sexp, compare, equal]

  let empty =
    { parents = 0
    ; filled_parents = 0
    ; target_notional = 0.
    ; filled_notional = 0.
    ; fees = 0.
    ; shortfall = 0.
    ; slippage_cost = 0.
    ; children = 0
    }
  ;;

  let add t (r : Stable.V1.t) =
    let a = Price.to_float r.arrival_px in
    let q = Size.to_float r.filled in
    let filled_notional = q *. Price.to_float r.avg_px in
    { parents = t.parents + 1
    ; filled_parents =
        (t.filled_parents
         +
         match r.outcome with
         | Filled -> 1
         | Deadline | Cancelled | Unfilled -> 0)
    ; target_notional = t.target_notional +. (Size.to_float r.target *. a)
    ; filled_notional = t.filled_notional +. filled_notional
    ; fees = t.fees +. r.fees
    ; shortfall = t.shortfall +. r.shortfall
    ; slippage_cost =
        (t.slippage_cost
         +. if Float.(q > 0.) then sign r.side *. (filled_notional -. (q *. a)) else 0.)
    ; children = t.children + r.children
    }
  ;;

  let of_list = List.fold ~init:empty ~f:add
  let ratio a b = if Float.(b > 0.) then a /. b else 0.
  let shortfall_bps t = ratio t.shortfall t.target_notional *. 10_000.
  let slippage_bps t = ratio t.slippage_cost t.filled_notional *. 10_000.
  let fill_rate t = ratio t.filled_notional t.target_notional
end
