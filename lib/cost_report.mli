(** What executing one parent order cost, measured against its arrival price (the mid when
    the parent started, or its reference price when the book had none).

    With [s = +1] for a buy and [-1] for a sell, [a] the arrival price, [p] the average
    fill price, [e] the price when the parent ended, [q] the filled size and [u] the size
    left unfilled:

    - arrival slippage: [s * (p - a) / a] in basis points (positive is worse);
    - implementation shortfall: [s * q * (p - a) + s * u * (e - a) + fees] in USD: the
      execution cost plus the opportunity cost of what did not fill (the move the follower
      missed), plus fees; also in basis points of [target * a]. *)

open! Core
module Price = Market_data.Price
module Size = Market_data.Size
module Side = Market_data.Side

module Outcome : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Filled
        | Deadline
        | Cancelled
        | Unfilled
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  type t = Stable.V1.t =
    | Filled (** the whole target *)
    | Deadline (** the algorithm's horizon ended first *)
    | Cancelled (** the owner cancelled the parent *)
    | Unfilled
    (** a single IOC (or the final sweep) found too little within the limit, or the rest
        was below the minimum size *)
  [@@deriving bin_io, sexp, compare, equal]

  val to_string : t -> string
end

module Stable : sig
  module V1 : sig
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

type t = Stable.V1.t =
  { algo : string (** [Algo.name] *)
  ; side : Side.t
  ; target : Size.t
  ; filled : Size.t
  ; avg_px : Price.t (** zero when nothing filled *)
  ; arrival_px : Price.t
  ; end_px : Price.t
  ; arrival_ns : int
  ; end_ns : int
  ; children : int (** child orders placed *)
  ; maker_filled : Size.t (** of [filled], what filled while resting *)
  ; fees : float (** USD *)
  ; arrival_slippage_bps : float (** 0 when nothing filled *)
  ; shortfall : float (** USD *)
  ; shortfall_bps : float
  ; outcome : Outcome.t
  }
[@@deriving bin_io, sexp, compare, equal]

(** Computes the derived fields. [notional] is the filled notional in USD (sum of size
    times price). *)
val compute
  :  algo:string
  -> side:Side.t
  -> target:Size.t
  -> filled:Size.t
  -> notional:float
  -> maker_filled:Size.t
  -> fees:float
  -> arrival_px:Price.t
  -> end_px:Price.t
  -> arrival_ns:int
  -> end_ns:int
  -> children:int
  -> outcome:Outcome.t
  -> t

(** The same report with the opportunity cost of the unfilled part valued at [end_px]
    instead (for example the mid some time after the parent ended). *)
val with_end_px : t -> Price.t -> t

(** Sums over many parents. *)
module Totals : sig
  type t =
    { parents : int
    ; filled_parents : int (** [Filled] outcomes *)
    ; target_notional : float (** USD, at arrival prices *)
    ; filled_notional : float (** USD, at fill prices *)
    ; fees : float
    ; shortfall : float
    ; slippage_cost : float (** USD: [s * q * (p - a)] summed *)
    ; children : int
    }
  [@@deriving sexp, compare, equal]

  val empty : t
  val add : t -> Stable.V1.t -> t
  val of_list : Stable.V1.t list -> t

  (** Shortfall in basis points of the target notional. *)
  val shortfall_bps : t -> float

  (** Slippage cost in basis points of the filled notional. *)
  val slippage_bps : t -> float

  (** [filled_notional / target_notional]. *)
  val fill_rate : t -> float
end
