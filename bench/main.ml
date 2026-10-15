(* Cost of one scheduler step and of the book helpers. The state is not threaded between
   runs, so every run does the same work: an idle tick (nothing due), a tick that places a
   child, a whole parent through the paper venue, and the impact walks. *)

open! Core
open Core_bench
open Execution_algos
module C = Execution_algos_test.Bench_cases
module H = Execution_algos_test.Harness

let tick name (s, now_ns, book, traded) =
  Bench.Test.create ~name (fun () ->
    let #(s', actions) = Scheduler.tick s ~now_ns ~book ~traded in
    ignore (Sys.opaque_identity s' : Scheduler.t);
    ignore (Sys.opaque_identity actions : Child.Action.t list))
;;

let whole name algo =
  let steps =
    H.steps ~n:12 (List.init 12 ~f:(fun i -> 100. +. (0.01 *. Float.of_int i)))
  in
  let parent =
    Parent.create
      ~side:Buy
      ~size:(H.sz 3.)
      ~limit_px:(H.px 100.1)
      ~reference_px:(H.px 100.)
      ~lot:(H.sz 0.1)
      ()
  in
  Bench.Test.create ~name (fun () ->
    ignore (Sys.opaque_identity (H.run (Scheduler.create algo parent) steps)))
;;

let () =
  let book = C.book10 in
  let size = H.sz 4.5 in
  Command_unix.run
    (Bench.make_command
       [ tick "tick twap idle" (C.twap_idle ())
       ; tick "tick passive idle" (C.passive_idle ())
       ; tick "tick twap place" (C.twap_first ())
       ; Bench.Test.create ~name:"impact walk 10 levels" (fun () ->
           let #(filled, notional) =
             Impact.walk book ~side:Buy ~size ~limit:(Impact.no_limit Buy)
           in
           ignore (Sys.opaque_identity filled : Market_data.Size.t);
           ignore (Sys.opaque_identity notional : int))
       ; Bench.Test.create ~name:"impact estimate" (fun () ->
           ignore (Sys.opaque_identity (Impact.estimate book ~side:Buy ~size)))
       ; whole
           "parent twap 12 steps"
           (Twap { duration_ms = 9_000; slices = 3; max_participation = None })
       ; whole "parent adaptive 12 steps" Algo.adaptive
       ])
;;
