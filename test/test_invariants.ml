(* Quickcheck properties of the scheduler over random algos, parents, markets and venue
   latencies, driven through the paper venue (see the Scheduler interface for the
   invariants). *)

open! Core
open Execution_algos
open Harness
module G = Quickcheck.Generator

module Case = struct
  type t =
    { algo : Algo.t
    ; parent : Parent.t
    ; steps : Step.t list
    ; latency_ms : int
    }

  let sexp_of_t t =
    [%message
      ""
        ~algo:(t.algo : Algo.t)
        ~parent:(t.parent : Parent.t)
        ~latency_ms:(t.latency_ms : int)
        ~steps:(List.length t.steps : int)]
  ;;
end

let algo_gen : Algo.t G.t =
  let open G.Let_syntax in
  let ms = Int.gen_incl 1_000 20_000 in
  let rate = Float.gen_incl 0.05 1. in
  G.union
    [ return Algo.Immediate
    ; (let%map duration_ms = ms
       and slices = Int.gen_incl 1 8
       and cap = Option.quickcheck_generator rate in
       Algo.Twap { duration_ms; slices; max_participation = cap })
    ; (let%map duration_ms = ms
       and slices = Int.gen_incl 1 8
       and cap = Option.quickcheck_generator rate in
       Algo.Vwap { duration_ms; slices; max_participation = cap })
    ; (let%map participation = rate
       and max_duration_ms = ms in
       Algo.Pov { participation; max_duration_ms })
    ; (let%map display = Float.gen_incl 0.1 1.
       and timeout_ms = ms in
       Algo.Passive { display; timeout_ms })
    ; (let%map small_ratio = Float.gen_incl 0.1 5.
       and depth_bps = Int.gen_incl 1 50
       and max_behind_bps = Int.gen_incl 1 500
       and max_drift_bps = Int.gen_incl 1 30
       and timeout_ms = ms in
       Algo.Adaptive { small_ratio; depth_bps; max_behind_bps; max_drift_bps; timeout_ms })
    ]
;;

let case_gen : Case.t G.t =
  let open G.Let_syntax in
  let%bind algo = algo_gen in
  let%bind side = G.of_list Side.all in
  let%bind lots = Int.gen_incl 1 50 in
  let%bind min_lots = Int.gen_incl 0 5 in
  let%bind limit_bps = Int.gen_incl 0 60 in
  let%bind equity = G.of_list [ 0.; 1_000.; 100_000. ] in
  let%bind every_ms = Int.gen_incl 200 2_000 in
  let%bind latency_ms = G.of_list [ 0; 0; 150; 1_000; 2_500 ] in
  let horizon_ms =
    Option.value_map (Algo.horizon algo) ~default:0 ~f:(fun h ->
      Int.of_float (Time_ns.Span.to_ms h))
  in
  (* the horizon, then two venue round trips: the cancel of a resting child, then the
     final sweep it releases *)
  let n = ((horizon_ms + (2 * latency_ms)) / every_ms) + 4 in
  let%bind moves = G.list_with_length n (Int.gen_incl (-3) 3) in
  let%bind volumes = G.list_with_length n (Int.gen_incl 0 30) in
  let%bind level_sizes = G.list_with_length n (Int.gen_incl 1 20) in
  let lot = sz 0.1 in
  let reference = px 100. in
  let parent =
    Parent.create
      ~side
      ~size:(Size.of_units (lots * Size.to_units lot))
      ~limit_px:(Impact.limit_from ~reference ~side ~bps:limit_bps)
      ~reference_px:reference
      ~lot
      ~min_size:(Size.of_units (min_lots * Size.to_units lot))
      ~equity
      ()
  in
  let mid = ref 100. in
  let traded = ref Size.zero in
  let steps =
    List.zip_exn (List.zip_exn moves volumes) level_sizes
    |> List.mapi ~f:(fun i ((move, volume), level) ->
      mid := !mid +. (0.01 *. Float.of_int move);
      let book = book ~size:(Size.of_units (level * 10_000_000)) (px !mid) in
      let volume = Size.of_units (volume * 10_000_000) in
      let aggressor : Side.t = if i % 2 = 0 then Sell else Buy in
      let trade_px =
        match aggressor with
        | Sell -> Book.px book Buy 0
        | Buy -> Book.px book Sell 0
      in
      traded := Size.( + ) !traded volume;
      { Step.now_ns = i * every_ms * 1_000_000
      ; book
      ; traded = !traded
      ; trades = (if Size.is_positive volume then [ trade_px, volume, aggressor ] else [])
      })
  in
  return { Case.algo; parent; steps; latency_ms }
;;

let within_limit (parent : Parent.t) (c : Child.t) =
  match parent.side with
  | Buy -> Price.( <= ) c.limit_px parent.limit_px
  | Sell -> Price.( >= ) c.limit_px parent.limit_px
;;

let check (case : Case.t) =
  let paper = Paper.create ~latency:(Time_ns.Span.of_int_ms case.latency_ms) () in
  let sched, journal = run ~paper (Scheduler.create case.algo case.parent) case.steps in
  let p = case.parent in
  let lot = Size.to_units p.lot in
  let min_child = Int.max lot (Size.to_units p.min_size) in
  let traded0 = (List.hd_exn case.steps).traded in
  let rate =
    match case.algo with
    | Pov { participation; _ } -> Some participation
    | Twap { max_participation; _ } | Vwap { max_participation; _ } -> max_participation
    | Immediate | Passive _ | Adaptive _ -> None
  in
  let deadline = ref None in
  let late_places = ref 0 in
  (* replay the journal's events on a fresh scheduler: same actions, same final state *)
  let replayed, actions =
    Scheduler.replay
      (Scheduler.create case.algo case.parent)
      (List.map journal ~f:(fun e -> e.event))
  in
  if not ([%equal: Scheduler.t] replayed sched)
  then raise_s [%message "replay diverged" (case : Case.t)];
  List.iter2_exn journal actions ~f:(fun e a ->
    if not ([%equal: Child.Action.t list] e.actions a)
    then raise_s [%message "replayed actions differ" (case : Case.t)]);
  (* each state along the way *)
  ignore
    (List.fold journal ~init:(Scheduler.create case.algo case.parent) ~f:(fun s e ->
       let s, actions = Scheduler.apply s e.event in
       let filled = Size.to_units (Scheduler.filled s) in
       let working = Size.to_units (Scheduler.working_size s) in
       if filled + working > Size.to_units p.size
       then
         raise_s [%message "over target" (case : Case.t) (filled : int) (working : int)];
       if Option.is_none !deadline then deadline := Scheduler.deadline_ns s;
       (match e.event, rate with
        | Tick { traded; _ }, Some rate ->
          let allowed =
            rate *. Float.of_int (Size.to_units traded - Size.to_units traded0)
          in
          if Float.( > ) (Float.of_int (filled + working)) (allowed +. 1.)
          then
            raise_s
              [%message
                "participation exceeded" (case : Case.t) (filled : int) (allowed : float)]
        | _ -> ());
       List.iter actions ~f:(function
         | Cancel _ -> ()
         | Place c ->
           let u = Size.to_units c.size in
           (* Immediate sends the whole parent whatever its size, like a plain IOC *)
           let min_child =
             match case.algo with
             | Immediate -> lot
             | _ -> min_child
           in
           if u % lot <> 0 || u < min_child || not (within_limit p c)
           then raise_s [%message "bad child" (case : Case.t) (c : Child.t)];
           (match !deadline with
            | Some d when e.now_ns >= d ->
              (match case.algo with
               | (Passive _ | Adaptive _) when not c.post_only -> incr late_places
               | _ ->
                 raise_s
                   [%message "placed after the horizon" (case : Case.t) (c : Child.t)])
            | _ -> ()));
       s)
     : Scheduler.t);
  if !late_places > 1 then raise_s [%message "more than one final sweep" (case : Case.t)];
  (* the market outlives the horizon and the latency: the parent must be done *)
  if not (Scheduler.is_done sched)
  then
    raise_s
      [%message
        "not done after the horizon"
          (case : Case.t)
          ~status:(Scheduler.status sched : Scheduler.Status.t)]
;;

let%expect_test "scheduler invariants" =
  Quickcheck.test ~trials:2_000 ~sexp_of:Case.sexp_of_t case_gen ~f:check;
  [%expect {| |}]
;;

let%expect_test "events round trip through bin_io" =
  Quickcheck.test ~trials:200 ~sexp_of:Case.sexp_of_t case_gen ~f:(fun case ->
    let _, journal = run (Scheduler.create case.algo case.parent) case.steps in
    List.iter journal ~f:(fun e ->
      let s = Binable.to_string (module Scheduler.Event.Stable.V1) e.event in
      let back = Binable.of_string (module Scheduler.Event.Stable.V1) s in
      assert ([%equal: Scheduler.Event.t] back e.event)));
  [%expect {| |}]
;;

(* What the random cases cover: outcomes per algo over the first 1000 cases of the fixed
   seed, so a generator change that stops exercising an algo shows up here. *)
let%expect_test "coverage of the generated cases" =
  let counts = Hashtbl.create (module String) in
  Quickcheck.iter ~trials:1_000 case_gen ~f:(fun case ->
    let paper = Paper.create ~latency:(Time_ns.Span.of_int_ms case.latency_ms) () in
    let sched, _ = run ~paper (Scheduler.create case.algo case.parent) case.steps in
    let outcome =
      match Scheduler.status sched with
      | Done o -> Cost_report.Outcome.to_string o
      | Pending | Working -> "open"
    in
    Hashtbl.incr counts (sprintf "%-9s %s" (Algo.name case.algo) outcome));
  Hashtbl.to_alist counts
  |> List.sort ~compare:[%compare: string * int]
  |> List.iter ~f:(fun (k, n) -> printf "%s %d\n" k n);
  [%expect
    {|
    adaptive  filled 150
    adaptive  unfilled 19
    iceberg   filled 99
    iceberg   unfilled 57
    immediate filled 118
    immediate unfilled 53
    passive   filled 4
    passive   unfilled 2
    pov       deadline 72
    pov       filled 99
    twap      deadline 68
    twap      filled 100
    vwap      deadline 65
    vwap      filled 94
    |}]
;;
