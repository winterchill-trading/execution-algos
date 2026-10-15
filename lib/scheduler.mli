(** The child-order scheduler: a pure state machine that works one {!Parent.t} with one
    {!Algo.t}.

    The caller feeds it {!Event.t}s (time and book ticks, child fills, child closes, a
    cancel) and sends the {!Child.Action.t}s it returns. Nothing here reads a clock or
    holds a mutable field: the same events from the same state give the same actions and
    states, so a journal of events replays exactly. A parent starts on its first [Tick]
    (arrival time, arrival mid and the market volume so far are taken from it).

    Invariants (property-tested in [test/test_invariants.ml]):

    - never more than the target: [filled + working <= size], where [working] is what
      the open children could still fill, whatever the fills and closes;
    - every child is on the lot grid, at least [max lot min_size], and priced no worse
      than [parent.limit_px];
    - with a participation cap (POV, or TWAP/VWAP with [max_participation]),
      [filled + working <= rate * market volume since the start];
    - nothing is placed at or after the horizon ({!Algo.horizon}), except the final IOC
      sweep of [Passive] and [Adaptive]; post-only children are cancelled at the horizon,
      and once the caller has reported their closes the parent is done.

    A tick that has nothing to do returns the same state and no action without allocating
    ({!tick}); see the README for the measured cost per step. *)

open! Core
module Price = Market_data.Price
module Size = Market_data.Size

module Event : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Tick of
            { now_ns : int
            ; book : Book.Stable.V1.t
            ; traded : Size.Stable.V1.t
            }
        | Fill of Fill.Stable.V1.t
        | Closed of int
        | Cancel
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  type t = Stable.V1.t =
    | Tick of
        { now_ns : int (** nanoseconds since the epoch, never decreasing *)
        ; book : Book.t (** the market now *)
        ; traded : Size.t
        (** cumulative market volume of the instrument (any origin, never decreasing):
            only differences matter *)
        }
    | Fill of Fill.t
    | Closed of int
    (** the child is done: an IOC's remainder was cancelled, the order was rejected, or a
        cancel was confirmed. Its unfilled remainder no longer counts as working. *)
    | Cancel (** the owner cancels the parent: post-only children are cancelled *)
  [@@deriving bin_io, sexp, compare, equal]
end

module Working : sig
  type t =
    { child : Child.t
    ; remaining : Size.t (** what can still fill *)
    ; cancel_sent : bool
    }
  [@@deriving bin_io, sexp, compare, equal]
end

module Phase : sig
  type t =
    | Passive
    | Aggressive
  [@@deriving bin_io, sexp, compare, equal]
end

module Status : sig
  type t =
    | Pending (** waiting for the first tick *)
    | Working
    | Done of Cost_report.Outcome.t
  [@@deriving bin_io, sexp, compare, equal]
end

type t [@@deriving bin_io, sexp, compare, equal]

(** A pending parent. [profile] is the VWAP volume profile (default uniform). Raises if
    [algo] is invalid ({!Algo.validate}). *)
val create : ?profile:Volume_profile.t -> Algo.t -> Parent.t -> t

(** One event. *)
val apply : t -> Event.t -> t * Child.Action.t list

(** [apply] for a [Tick]; returns [t] itself (physically) and [[]] when there is nothing
    to do, without allocating. *)
val tick : t -> now_ns:int -> book:Book.t -> traded:Size.t -> #(t * Child.Action.t list)

(** The fills, then one tick: the shape of a periodic driver. *)
val step
  :  t
  -> now_ns:int
  -> book:Book.t
  -> traded:Size.t
  -> fills:Fill.t list
  -> t * Child.Action.t list

(** Folds events from [t]: the actions of each, in order. *)
val replay : t -> Event.t list -> t * Child.Action.t list list

(** {2 Queries} *)

val algo : t -> Algo.t
val parent : t -> Parent.t
val status : t -> Status.t
val phase : t -> Phase.t
val is_done : t -> bool
val filled : t -> Size.t

(** [size - filled]. *)
val remaining : t -> Size.t

(** What open children could still fill. *)
val working_size : t -> Size.t

val working : t -> Working.t list

(** Children placed so far. *)
val children : t -> int

(** Arrival time and price ([None] before the first tick). *)
val arrival : t -> (int * Price.t) option

(** When the horizon ends ([None] before the first tick or for [Immediate]). *)
val deadline_ns : t -> int option

(** The cost report once the parent is done; [end_px] defaults to the mid at the last
    tick that acted (or the arrival price). *)
val report : ?end_px:Price.t -> t -> Cost_report.t option
