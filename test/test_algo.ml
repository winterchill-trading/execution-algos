open! Core
open Execution_algos

let%expect_test "short forms round trip" =
  List.iter
    [ "immediate"
    ; "twap"
    ; "twap:2m:4"
    ; "vwap:10m"
    ; "pov:0.05:1m"
    ; "passive:15s"
    ; "iceberg:0.2:45s"
    ; "adaptive"
    ; "adaptive:5s"
    ]
    ~f:(fun s ->
      let t = ok_exn (Algo.of_string s) in
      let back = Algo.to_string t in
      assert ([%equal: Algo.t] t (ok_exn (Algo.of_string back)));
      printf "%-16s -> %-16s %s\n" s back (Algo.name t));
  [%expect
    {|
    immediate        -> immediate        immediate
    twap             -> twap:1m:6        twap
    twap:2m:4        -> twap:2m:4        twap
    vwap:10m         -> vwap:10m:10      vwap
    pov:0.05:1m      -> pov:0.05:1m      pov
    passive:15s      -> passive:15s      passive
    iceberg:0.2:45s  -> iceberg:0.2:45s  iceberg
    adaptive         -> adaptive:20s     adaptive
    adaptive:5s      -> adaptive:5s      adaptive
    |}]
;;

let%expect_test "invalid parameters" =
  List.iter [ "twap:0s"; "pov:1.5"; "iceberg:0"; "nope" ] ~f:(fun s ->
    print_s [%sexp (Algo.of_string s : Algo.t Or_error.t)]);
  [%expect
    {|
    (Error ("invalid algo" (errors ("duration must be positive"))))
    (Error ("invalid algo" (errors ("participation must be in (0, 1]"))))
    (Error ("invalid algo" (errors ("display must be in (0, 1]"))))
    (Error ("unknown algo" nope))
    |}]
;;

let%expect_test "sexp form" =
  print_s [%sexp (Algo.adaptive : Algo.t)];
  [%expect
    {|
    (Adaptive (small_ratio 0.5) (depth_bps 10) (max_behind_bps 50)
     (max_drift_bps 10) (timeout_ms 20000))
    |}]
;;
