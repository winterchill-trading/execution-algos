(** Synthetic books and a driver that runs a {!Execution_algos.Scheduler} against a
    {!Execution_algos.Paper} venue, for tests and the bench. *)

open! Core
open Execution_algos
module Price = Market_data.Price
module Size = Market_data.Size
module Side = Market_data.Side

(** [levels] levels per side, [tick] apart (in price units of 10^-8), each of [size],
    around [mid]: best bid [mid - tick/2] rounded down to the tick, best ask one tick
    above it. *)
val book : ?levels:int -> ?tick:int -> ?size:Size.t -> Price.t -> Book.t

val px : float -> Price.t
val sz : float -> Size.t

module Step : sig
  type t =
    { now_ns : int
    ; book : Book.t
    ; traded : Size.t (** cumulative *)
    ; trades : (Price.t * Size.t * Side.t) list (** prints since the last step *)
    }
end

(** One step every [every_ms] for [n] steps from time 0, mid following [mids] (the last
    one repeats), [volume] traded per step at the touch, alternating sides. *)
val steps
  :  ?levels:int
  -> ?size:Size.t
  -> ?every_ms:int
  -> ?volume:Size.t
  -> n:int
  -> float list
  -> Step.t list

(** Drives the scheduler through the steps; returns the final state and every event it was
    fed with the actions it returned (the journal). *)
val run
  :  ?paper:Paper.t
  -> Scheduler.t
  -> Step.t list
  -> Scheduler.t * (Scheduler.Event.t * Child.Action.t list) list

(** One line per action and fill, for expect tests. *)
val print_journal : (Scheduler.Event.t * Child.Action.t list) list -> unit

val print_report : Scheduler.t -> unit
