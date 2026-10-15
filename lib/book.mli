(** A top-of-book snapshot: the best levels of each side as flat arrays of prices and
    sizes, best first. It is a value (the scheduler stores the arrival book and journals
    ticks), cheap to build from {!Market_data.L2_book} and read without allocating.

    Bids are in descending price order, asks ascending; sizes are positive. {!create}
    checks both. *)

open! Core
module Price = Market_data.Price
module Size = Market_data.Size
module Side = Market_data.Side

module Stable : sig
  module V1 : sig
    type t = private
      { bid_px : Price.Stable.V1.t array
      ; bid_sz : Size.Stable.V1.t array
      ; ask_px : Price.Stable.V1.t array
      ; ask_sz : Size.Stable.V1.t array
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

type t = Stable.V1.t = private
  { bid_px : Price.t array
  ; bid_sz : Size.t array
  ; ask_px : Price.t array
  ; ask_sz : Size.t array
  }
[@@deriving bin_io, sexp, compare, equal]

val empty : t

(** Levels best first. Raises on unsorted prices, a non-positive size, or a crossed book
    (best bid at or above best ask). *)
val create : bids:(Price.t * Size.t) list -> asks:(Price.t * Size.t) list -> t

(** The best [depth] levels (default 20) of each side of an L2 book. *)
val of_l2_book : ?depth:int -> Market_data.L2_book.t -> t

(** One level per side. *)
val of_bbo : Market_data.Bbo.t -> t

(** Number of levels on [side]'s resting orders: [Buy] counts bids, [Sell] asks. *)
val levels : t -> Side.t -> int
[@@zero_alloc]

(** Price and size of level [i] (0 = best) of [side]'s resting orders. *)
val px : t -> Side.t -> int -> Price.t
[@@zero_alloc]

val sz : t -> Side.t -> int -> Size.t [@@zero_alloc]
val best_bid : t -> Price.t or_null [@@zero_alloc]
val best_ask : t -> Price.t or_null [@@zero_alloc]

(** The best price a taker on [side] trades at: the best ask for a buy. *)
val touch : t -> Side.t -> Price.t or_null
[@@zero_alloc]

(** [(bid + ask) / 2] truncated to the price unit; only one side when the other is empty;
    [Null] for an empty book. *)
val mid : t -> Price.t or_null
[@@zero_alloc]

(** Size resting at exactly [px] on [side]'s orders, zero when the price is not shown. *)
val size_at : t -> Side.t -> Price.t -> Size.t
[@@zero_alloc]

val is_empty : t -> bool
