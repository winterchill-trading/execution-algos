(** Impact and slippage of a taker order, estimated by walking the levels of a
    {!Book.t}. The same walk is the paper venue's fill model, so an estimate and a paper
    fill of the same order on the same book agree exactly.

    All results assume the book does not move while the order executes; the
    {!Estimate.t} says when the shown depth ran out ([exhausted]), in which case the real
    cost is higher than the estimate. *)

open! Core
module Price = Market_data.Price
module Size = Market_data.Size
module Side = Market_data.Side

(** The least restrictive limit for a taker on [side]: no price cap. *)
val no_limit : Side.t -> Price.t

(** [limit_from ~reference ~side ~bps] is [reference] moved [bps] basis points against a
    taker on [side] (up for a buy), truncated to the price unit. *)
val limit_from : reference:Price.t -> side:Side.t -> bps:int -> Price.t
[@@zero_alloc]

(** A taker on [side] for [size], at prices no worse than [limit], against the shown
    levels: the size filled and its notional in USD units of [10^-8] (price times size).
    Allocates nothing. *)
val walk : Book.t -> side:Side.t -> size:Size.t -> limit:Price.t -> #(Size.t * int)
[@@zero_alloc]

(** Size a taker on [side] reaches within [bps] of the mid (both sides needed; zero for
    a one-sided or empty book). The "depth" the adaptive algorithm compares a parent
    with. *)
val depth_within : Book.t -> side:Side.t -> bps:int -> Size.t
[@@zero_alloc]

module Estimate : sig
  type t =
    { filled : Size.t
    ; avg_px : Price.t (** volume-weighted price of what fills *)
    ; worst_px : Price.t (** price of the last level reached *)
    ; mid : Price.t option (** the book's mid, when both sides are shown *)
    ; touch_px : Price.t (** best price on the side taken *)
    ; slippage_bps : float
    (** [avg_px] against the mid (the touch for a one-sided book), positive when worse for
        the taker *)
    ; touch_bps : float (** [avg_px] against the touch: the cost of walking the book *)
    ; levels : int (** levels reached *)
    ; exhausted : bool (** the shown depth (within the limit) did not cover [size] *)
    }
  [@@deriving sexp, compare, equal]
end

(** [None] when nothing on the side taken is at or better than [limit] (default
    {!no_limit}). *)
val estimate : ?limit:Price.t -> Book.t -> side:Side.t -> size:Size.t -> Estimate.t option

(** [(price - reference) / reference] in basis points, signed so that positive is worse
    for [side] (paying up on a buy, selling lower on a sell). *)
val adverse_bps : side:Side.t -> reference:Price.t -> Price.t -> float
