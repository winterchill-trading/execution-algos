(** A paper venue for one instrument: fills child orders against a replayed book and trade
    prints, deterministically. Tests use it to drive the {!Scheduler}; backtests layer
    fees, funding and positions on top.

    Fill model:

    - An IOC child walks the book's levels within its limit ({!Impact.walk}, one fill per
      level) less what earlier IOCs already took from the same book; the next {!set_book}
      restores the shown depth. Then the child is closed.
    - A post-only child that would cross the opposite touch is closed unfilled. Otherwise
      it rests behind the size shown at its price when it arrives ([queue_ahead]); a trade
      at its price first eats the queue, a trade through it fills it at its own price; a
      book that no longer shows that much at the price caps the queue (the idea of
      price-anchor's [Queue_ahead]); a book whose opposite touch reaches its price fills
      it as the maker.
    - Actions take effect [latency] after they are sent, in order.

    Fees are [taker_fee_bps] (IOC) and [maker_fee_bps] (resting) of the fill's notional; a
    negative maker fee is a rebate. *)

open! Core
module Price = Market_data.Price
module Size = Market_data.Size
module Side = Market_data.Side

module Event : sig
  type t =
    | Fill of Fill.t
    | Closed of int
  [@@deriving sexp, compare, equal]
end

type t

(** Defaults: no latency, 4.5 bp taker, 1.5 bp maker (Hyperliquid's base perpetual tier). *)
val create
  :  ?latency:Time_ns.Span.t
  -> ?taker_fee_bps:float
  -> ?maker_fee_bps:float
  -> unit
  -> t

(** Sends actions at [now_ns]; they take effect at [now_ns + latency] (immediately when
    there is no latency). *)
val send : t -> now_ns:int -> Child.Action.t list -> unit

(** Applies actions that are due at [now_ns]. *)
val advance : t -> now_ns:int -> unit

(** The market moves: applies due actions, then the new book (queue caps, crossing fills). *)
val set_book : t -> now_ns:int -> Book.t -> unit

(** A trade print: applies due actions, then fills resting children the trade reaches.
    [aggressor] is the taker's side ([Sell] hits bids). *)
val trade : t -> now_ns:int -> price:Price.t -> size:Size.t -> aggressor:Side.t -> unit

(** Fills and closes since the last call, oldest first. *)
val take_events : t -> Event.t list

val book : t -> Book.t

(** Resting children: the child, what is left, the queue ahead of it. *)
val resting : t -> (Child.t * Size.t * Size.t) list

(** Actions sent and not yet applied. *)
val in_flight : t -> int
