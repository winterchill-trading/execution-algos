open! Core
open Execution_algos
open Harness

(* A buy of 3 lots of 1 against books of 1 per level, 1 traded per second, the mid
   drifting up a cent per second. *)
let parent ?(size = 3.) ?(limit = 100.10) ?(equity = 0.) ?(side = Side.Buy) () =
  Parent.create
    ~side
    ~size:(sz size)
    ~limit_px:(px limit)
    ~reference_px:(px 100.)
    ~lot:(sz 0.1)
    ~min_size:(sz 0.1)
    ~equity
    ()
;;

let flat n = List.init n ~f:(fun _ -> 100.)
let rising n = List.init n ~f:(fun i -> 100. +. (0.01 *. Float.of_int i))

let run_and_print ?steps:s ?(n = 12) ?parent:(p = parent ()) algo =
  let s = Option.value s ~default:(steps ~n (rising n)) in
  let sched, journal = run (Scheduler.create algo p) s in
  print_journal journal;
  print_report sched
;;

let%expect_test "immediate: one IOC at the limit, walks the book" =
  run_and_print Algo.Immediate;
  [%expect
    {|
    t=0.0s
      place #0 Buy 3 @ 100.1 ioc
      fill #0 1 @ 100
      fill #0 1 @ 100.01
      fill #0 1 @ 100.02
    immediate filled: filled 3 of 3 @ 100.01, arrival 99.995, slippage 1.50 bps, shortfall 0.1800 USD (6.00 bps), fees 0.1350, children 1, maker 0
    |}]
;;

let%expect_test "immediate: the limit stops the walk, the rest is left" =
  run_and_print ~parent:(parent ~limit:100.01 ()) Algo.Immediate;
  [%expect
    {|
    t=0.0s
      place #0 Buy 3 @ 100.01 ioc
      fill #0 1 @ 100
      fill #0 1 @ 100.01
    immediate unfilled: filled 2 of 3 @ 100.005, arrival 99.995, slippage 1.00 bps, shortfall 0.1100 USD (3.67 bps), fees 0.0900, children 1, maker 0
    |}]
;;

let%expect_test "twap: three slices of one over 9 s" =
  run_and_print (Twap { duration_ms = 9_000; slices = 3; max_participation = None });
  [%expect
    {|
    t=0.0s
      place #0 Buy 1 @ 100.1 ioc
      fill #0 1 @ 100
    t=3.0s
      place #1 Buy 1 @ 100.1 ioc
      fill #1 1 @ 100.03
    t=6.0s
      place #2 Buy 1 @ 100.1 ioc
      fill #2 1 @ 100.06
    twap filled: filled 3 of 3 @ 100.03, arrival 99.995, slippage 3.50 bps, shortfall 0.2400 USD (8.00 bps), fees 0.1350, children 3, maker 0
    |}]
;;

let%expect_test "twap with a participation cap of 50%" =
  run_and_print (Twap { duration_ms = 9_000; slices = 3; max_participation = Some 0.5 });
  [%expect
    {|
    t=1.0s
      place #0 Buy 0.5 @ 100.1 ioc
      fill #0 0.5 @ 100.01
    t=2.0s
      place #1 Buy 0.5 @ 100.1 ioc
      fill #1 0.5 @ 100.02
    t=3.0s
      place #2 Buy 0.5 @ 100.1 ioc
      fill #2 0.5 @ 100.03
    t=4.0s
      place #3 Buy 0.5 @ 100.1 ioc
      fill #3 0.5 @ 100.04
    t=6.0s
      place #4 Buy 1 @ 100.1 ioc
      fill #4 1 @ 100.06
    twap filled: filled 3 of 3 @ 100.03666667, arrival 99.995, slippage 4.17 bps, shortfall 0.2600 USD (8.67 bps), fees 0.1350, children 5, maker 0
    |}]
;;

let%expect_test "vwap over a uniform profile is a twap" =
  run_and_print (Vwap { duration_ms = 9_000; slices = 3; max_participation = None });
  [%expect
    {|
    t=0.0s
      place #0 Buy 1 @ 100.1 ioc
      fill #0 1 @ 100
    t=3.0s
      place #1 Buy 1 @ 100.1 ioc
      fill #1 1 @ 100.03
    t=6.0s
      place #2 Buy 1 @ 100.1 ioc
      fill #2 1 @ 100.06
    vwap filled: filled 3 of 3 @ 100.03, arrival 99.995, slippage 3.50 bps, shortfall 0.2400 USD (8.00 bps), fees 0.1350, children 3, maker 0
    |}]
;;

let%expect_test "pov: 40% of the market volume, stops at the horizon" =
  run_and_print (Pov { participation = 0.4; max_duration_ms = 6_000 });
  [%expect
    {|
    t=1.0s
      place #0 Buy 0.4 @ 100.1 ioc
      fill #0 0.4 @ 100.01
    t=2.0s
      place #1 Buy 0.4 @ 100.1 ioc
      fill #1 0.4 @ 100.02
    t=3.0s
      place #2 Buy 0.4 @ 100.1 ioc
      fill #2 0.4 @ 100.03
    t=4.0s
      place #3 Buy 0.4 @ 100.1 ioc
      fill #3 0.4 @ 100.04
    t=5.0s
      place #4 Buy 0.4 @ 100.1 ioc
      fill #4 0.4 @ 100.05
    pov deadline: filled 2 of 3 @ 100.03, arrival 99.995, slippage 3.50 bps, shortfall 0.2200 USD (7.33 bps), fees 0.0900, children 5, maker 0
    |}]
;;

let%expect_test "passive in a rising market: re-pegs, then the timeout sweeps" =
  run_and_print (Passive { display = 1.0; timeout_ms = 6_000 });
  [%expect
    {|
    t=0.0s
      place #0 Buy 3 @ 99.99 post-only
    t=1.0s
      cancel #0
    t=2.0s
      place #1 Buy 3 @ 100.01 post-only
    t=3.0s
      cancel #1
    t=4.0s
      place #2 Buy 3 @ 100.03 post-only
    t=5.0s
      cancel #2
    t=6.0s
      place #3 Buy 3 @ 100.1 ioc
      fill #3 1 @ 100.06
      fill #3 1 @ 100.07
      fill #3 1 @ 100.08
    passive filled: filled 3 of 3 @ 100.07, arrival 99.995, slippage 7.50 bps, shortfall 0.3601 USD (12.00 bps), fees 0.1351, children 4, maker 0
    |}]
;;

let%expect_test "iceberg in a rising market" =
  run_and_print ~n:20 (Passive { display = 0.34; timeout_ms = 15_000 });
  [%expect
    {|
    t=0.0s
      place #0 Buy 1 @ 99.99 post-only
    t=1.0s
      cancel #0
      place #1 Buy 1 @ 100 post-only
    t=2.0s
      cancel #1
      place #2 Buy 1 @ 100.01 post-only
    t=3.0s
      cancel #2
      place #3 Buy 1 @ 100.02 post-only
    t=4.0s
      cancel #3
      place #4 Buy 1 @ 100.03 post-only
    t=5.0s
      cancel #4
      place #5 Buy 1 @ 100.04 post-only
    t=6.0s
      cancel #5
      place #6 Buy 1 @ 100.05 post-only
    t=7.0s
      cancel #6
      place #7 Buy 1 @ 100.06 post-only
    t=8.0s
      cancel #7
      place #8 Buy 1 @ 100.07 post-only
    t=9.0s
      cancel #8
      place #9 Buy 1 @ 100.08 post-only
    t=10.0s
      cancel #9
      place #10 Buy 1 @ 100.09 post-only
    t=11.0s
      cancel #10
      place #11 Buy 1 @ 100.1 post-only
    t=15.0s
      cancel #11
      place #12 Buy 2 @ 100.1 ioc
    iceberg unfilled: filled 0 of 3 @ 0, arrival 99.995, slippage 0.00 bps, shortfall 0.4500 USD (15.00 bps), fees 0.0000, children 13, maker 0
    |}]
;;

let%expect_test "adaptive: a small parent works passively" =
  run_and_print
    ~parent:(parent ~size:0.5 ~equity:100_000. ())
    (Adaptive
       { small_ratio = 0.5
       ; depth_bps = 10
       ; max_behind_bps = 50
       ; max_drift_bps = 10
       ; timeout_ms = 8_000
       });
  [%expect
    {|
    t=0.0s
      place #0 Buy 0.5 @ 99.99 post-only
    t=1.0s
      cancel #0
    t=2.0s
      place #1 Buy 0.5 @ 100.01 post-only
    t=3.0s
      cancel #1
    t=4.0s
      place #2 Buy 0.5 @ 100.03 post-only
    t=5.0s
      cancel #2
    t=6.0s
      place #3 Buy 0.5 @ 100.05 post-only
    t=7.0s
      cancel #3
    t=8.0s
      place #4 Buy 0.5 @ 100.1 ioc
      fill #4 0.5 @ 100.08
    adaptive filled: filled 0.5 of 0.5 @ 100.08, arrival 99.995, slippage 8.50 bps, shortfall 0.0650 USD (13.00 bps), fees 0.0225, children 5, maker 0
    |}]
;;

let%expect_test "adaptive: a parent large against the depth takes at once" =
  run_and_print ~parent:(parent ~size:3. ()) Algo.adaptive;
  [%expect
    {|
    t=0.0s
      place #0 Buy 3 @ 100.1 ioc
      fill #0 1 @ 100
      fill #0 1 @ 100.01
      fill #0 1 @ 100.02
    adaptive filled: filled 3 of 3 @ 100.01, arrival 99.995, slippage 1.50 bps, shortfall 0.1800 USD (6.00 bps), fees 0.1350, children 1, maker 0
    |}]
;;

let%expect_test "adaptive: too far behind the leader (equity bound) takes at once" =
  run_and_print ~parent:(parent ~size:0.5 ~equity:5_000. ()) Algo.adaptive;
  [%expect
    {|
    t=0.0s
      place #0 Buy 0.5 @ 100.1 ioc
      fill #0 0.5 @ 100
    adaptive filled: filled 0.5 of 0.5 @ 100, arrival 99.995, slippage 0.50 bps, shortfall 0.0250 USD (5.00 bps), fees 0.0225, children 1, maker 0
    |}]
;;

let%expect_test "adaptive: the mid runs away, the algo switches to taking" =
  let n = 10 in
  let s = steps ~n (List.init n ~f:(fun i -> 100. +. (0.05 *. Float.of_int i))) in
  run_and_print
    ~steps:s
    ~parent:(parent ~size:0.5 ~limit:101. ~equity:100_000. ())
    Algo.adaptive;
  [%expect
    {|
    t=0.0s
      place #0 Buy 0.5 @ 99.99 post-only
    t=1.0s
      cancel #0
    t=2.0s
      place #1 Buy 0.5 @ 101 ioc
      fill #1 0.5 @ 100.1
    adaptive filled: filled 0.5 of 0.5 @ 100.1, arrival 99.995, slippage 10.50 bps, shortfall 0.0750 USD (15.01 bps), fees 0.0225, children 2, maker 0
    |}]
;;

let%expect_test "cancel: a resting child is cancelled and the parent ends" =
  let p = parent () in
  let s = steps ~n:2 [ 100. ] in
  let sched, journal = run (Scheduler.create Algo.passive p) s in
  print_journal journal;
  let sched, actions = Scheduler.apply sched Cancel in
  print_s [%sexp (actions : Child.Action.t list)];
  let sched, _ = Scheduler.apply sched (Closed 0) in
  print_report sched;
  [%expect
    {|
    t=0.0s
      place #0 Buy 3 @ 99.99 post-only
    ((Cancel 0))
    passive cancelled: filled 0 of 3 @ 0, arrival 99.995, slippage 0.00 bps, shortfall 0.0000 USD (0.00 bps), fees 0.0000, children 1, maker 0
    |}]
;;

let%expect_test "a sell mirrors a buy" =
  run_and_print
    ~parent:(parent ~side:Sell ~limit:99.90 ())
    (Twap { duration_ms = 4_000; slices = 2; max_participation = None });
  [%expect
    {|
    t=0.0s
      place #0 Sell 1.5 @ 99.9 ioc
      fill #0 1 @ 99.99
      fill #0 0.5 @ 99.98
    t=2.0s
      place #1 Sell 1.5 @ 99.9 ioc
      fill #1 1 @ 100.01
      fill #1 0.5 @ 100
    twap filled: filled 3 of 3 @ 99.99666667, arrival 99.995, slippage -0.17 bps, shortfall 0.1300 USD (4.33 bps), fees 0.1350, children 2, maker 0
    |}]
;;

let%expect_test "latency: children land later" =
  let p = parent () in
  let s = steps ~n:6 (rising 6) in
  let paper = Paper.create ~latency:(Time_ns.Span.of_int_ms 1500) () in
  let sched, journal =
    run
      ~paper
      (Scheduler.create
         (Twap { duration_ms = 4_000; slices = 2; max_participation = None })
         p)
      s
  in
  print_journal journal;
  print_report sched;
  [%expect
    {|
    t=0.0s
      place #0 Buy 1.5 @ 100.1 ioc
    t=2.0s
      fill #0 1 @ 100.01
      fill #0 0.5 @ 100.02
      place #1 Buy 1.5 @ 100.1 ioc
    t=4.0s
      fill #1 1 @ 100.03
      fill #1 0.5 @ 100.04
    twap filled: filled 3 of 3 @ 100.02333333, arrival 99.995, slippage 2.83 bps, shortfall 0.2200 USD (7.33 bps), fees 0.1350, children 2, maker 0
    |}]
;;

let%expect_test "passive in a flat market: sellers fill it behind the queue" =
  run_and_print
    ~steps:(steps ~n:12 ~volume:(sz 2.) (flat 12))
    (Passive { display = 1.0; timeout_ms = 10_000 });
  [%expect
    {|
    t=0.0s
      place #0 Buy 3 @ 99.99 post-only
    t=2.0s
      fill #0 1 @ 99.99 maker
    t=4.0s
      fill #0 2 @ 99.99 maker
    passive filled: filled 3 of 3 @ 99.99, arrival 99.995, slippage -0.50 bps, shortfall 0.0300 USD (1.00 bps), fees 0.0450, children 1, maker 3
    |}]
;;

let%expect_test "iceberg in a flat market: one clip at a time" =
  run_and_print
    ~steps:(steps ~n:16 ~volume:(sz 2.) (flat 16))
    (Passive { display = 0.34; timeout_ms = 15_000 });
  [%expect
    {|
    t=0.0s
      place #0 Buy 1 @ 99.99 post-only
    t=2.0s
      fill #0 1 @ 99.99 maker
      place #1 Buy 1 @ 99.99 post-only
    t=4.0s
      fill #1 1 @ 99.99 maker
      place #2 Buy 1 @ 99.99 post-only
    t=6.0s
      fill #2 1 @ 99.99 maker
    iceberg filled: filled 3 of 3 @ 99.99, arrival 99.995, slippage -0.50 bps, shortfall 0.0300 USD (1.00 bps), fees 0.0450, children 3, maker 3
    |}]
;;

let%expect_test "adaptive in a flat market: a small parent fills as the maker" =
  run_and_print
    ~steps:(steps ~n:8 ~volume:(sz 2.) (flat 8))
    ~parent:(parent ~size:0.5 ~equity:100_000. ())
    Algo.adaptive;
  [%expect
    {|
    t=0.0s
      place #0 Buy 0.5 @ 99.99 post-only
    t=2.0s
      fill #0 0.5 @ 99.99 maker
    adaptive filled: filled 0.5 of 0.5 @ 99.99, arrival 99.995, slippage -0.50 bps, shortfall 0.0050 USD (1.00 bps), fees 0.0075, children 1, maker 0.5
    |}]
;;
