(** The share of a day's traded volume expected in each time-of-day bucket (UTC), for
    VWAP scheduling. Built from recorded {!Market_data.Trade.t}s, or uniform when there is
    no history (a VWAP over a uniform profile is a TWAP).

    Time arguments are nanoseconds since the epoch; a window may span days. *)

open! Core

module Stable : sig
  module V1 : sig
    type t [@@deriving bin_io, sexp, compare, equal]
  end
end

type t = Stable.V1.t [@@deriving bin_io, sexp, compare, equal]

(** Every bucket the same share; [bucket] defaults to five minutes and must divide a
    day. *)
val uniform : ?bucket:Time_ns.Span.t -> unit -> t

(** Volume per bucket from the trades' exchange times, summed over every day they cover
    and normalised. Buckets no trade fell in keep a small floor (1% of the average) so a
    quiet bucket still gets some of the schedule. No trades gives {!uniform}. *)
val of_trades : ?bucket:Time_ns.Span.t -> Market_data.Trade.t list -> t

(** From raw volumes per bucket (one entry per bucket of the day, non-negative). *)
val of_volumes : bucket:Time_ns.Span.t -> float array -> t

val bucket : t -> Time_ns.Span.t

(** Share of the day in bucket [i] (they sum to 1). *)
val weights : t -> float array

(** Expected share of one day's volume traded in [[start_ns, stop_ns)]: 1.0 for a full
    day, interpolated linearly inside a bucket. *)
val weight_between : t -> start_ns:int -> stop_ns:int -> float

(** Expected fraction of the window [[start_ns, stop_ns)] already traded at [now_ns], in
    [0, 1]: 0 at the start, 1 at and after the stop. A window with no expected volume
    falls back to the elapsed time fraction. *)
val fraction_done : t -> start_ns:int -> stop_ns:int -> now_ns:int -> float
