open! Core
open Execution_algos

let words f =
  (* warm up, then measure one call *)
  f ();
  let before = Gc.minor_words () in
  f ();
  Gc.minor_words () - before
;;

let tick (s, now_ns, book, traded) () =
  let #(s', actions) = Scheduler.tick s ~now_ns ~book ~traded in
  ignore (Sys.opaque_identity s' : Scheduler.t);
  ignore (Sys.opaque_identity actions : Child.Action.t list)
;;

(* Minor words per call. The idle ticks and the book walks are the per-tick cost of a
   parent that is waiting; they must stay at zero. *)
let%expect_test "allocation per call" =
  let book = Bench_cases.book10 in
  let size = Harness.sz 4.5 in
  printf "twap idle tick     %d\n" (words (tick (Bench_cases.twap_idle ())));
  printf "passive idle tick  %d\n" (words (tick (Bench_cases.passive_idle ())));
  printf
    "impact walk        %d\n"
    (words (fun () ->
       let #(filled, notional) =
         Impact.walk book ~side:Buy ~size ~limit:(Impact.no_limit Buy)
       in
       ignore (Sys.opaque_identity filled : Market_data.Size.t);
       ignore (Sys.opaque_identity notional : int)));
  printf
    "depth within       %d\n"
    (words (fun () ->
       ignore
         (Sys.opaque_identity (Impact.depth_within book ~side:Buy ~bps:10)
          : Market_data.Size.t)));
  printf "twap first tick    %d\n" (words (tick (Bench_cases.twap_first ())));
  [%expect
    {|
    twap idle tick     0
    passive idle tick  0
    impact walk        0
    depth within       0
    twap first tick    124
    |}]
;;
