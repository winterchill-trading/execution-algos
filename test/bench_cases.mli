(** Scheduler states shared by the allocation test and the bench. *)

open! Core
open Execution_algos

(** A started TWAP between two slices, and the next tick (nothing to do). *)
val twap_idle : unit -> Scheduler.t * int * Book.t * Market_data.Size.t

(** A passive parent resting at the bid, and a tick with the same book. *)
val passive_idle : unit -> Scheduler.t * int * Book.t * Market_data.Size.t

(** A pending TWAP and its first tick (one child placed). *)
val twap_first : unit -> Scheduler.t * int * Book.t * Market_data.Size.t

(** A ten-level book. *)
val book10 : Book.t
