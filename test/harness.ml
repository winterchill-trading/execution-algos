open! Core
open Execution_algos
module Price = Market_data.Price
module Size = Market_data.Size
module Side = Market_data.Side

let px x = (Price.of_float [@alert "-lossy"]) x
let sz x = (Size.of_float [@alert "-lossy"]) x

let book ?(levels = 5) ?(tick = 1_000_000) ?(size = sz 1.) mid =
  let m = Price.to_units mid in
  let best_bid = (m - (tick / 2)) / tick * tick in
  let best_bid =
    if best_bid + tick <= m - (tick / 2) then best_bid + tick else best_bid
  in
  let bids =
    List.init levels ~f:(fun i -> Price.of_units (best_bid - (i * tick)), size)
  in
  let asks =
    List.init levels ~f:(fun i -> Price.of_units (best_bid + ((i + 1) * tick)), size)
  in
  Book.create ~bids ~asks
;;

module Step = struct
  type t =
    { now_ns : int
    ; book : Book.t
    ; traded : Size.t
    ; trades : (Price.t * Size.t * Side.t) list
    }
end

let steps ?levels ?size ?(every_ms = 1000) ?(volume = sz 1.) ~n mids =
  let mids = Array.of_list mids in
  let traded = ref Size.zero in
  List.init n ~f:(fun i ->
    let mid = mids.(Int.min i (Array.length mids - 1)) in
    let book = book ?levels ?size (px mid) in
    let aggressor : Side.t = if i % 2 = 0 then Sell else Buy in
    let trade_px =
      match aggressor with
      | Sell -> Book.px book Buy 0
      | Buy -> Book.px book Sell 0
    in
    let trades =
      if Size.is_positive volume then [ trade_px, volume, aggressor ] else []
    in
    traded := Size.( + ) !traded volume;
    { Step.now_ns = i * every_ms * 1_000_000; book; traded = !traded; trades })
;;

let run ?(paper = Paper.create ()) sched steps =
  let journal = Queue.create () in
  let apply sched event =
    let sched, actions = Scheduler.apply sched event in
    Queue.enqueue journal (event, actions);
    sched, actions
  in
  let feed sched =
    List.fold (Paper.take_events paper) ~init:sched ~f:(fun sched e ->
      let event : Scheduler.Event.t =
        match e with
        | Fill f -> Fill f
        | Closed id -> Closed id
      in
      fst (apply sched event))
  in
  let sched =
    List.fold steps ~init:sched ~f:(fun sched (s : Step.t) ->
      Paper.set_book paper ~now_ns:s.now_ns s.book;
      List.iter s.trades ~f:(fun (price, size, aggressor) ->
        Paper.trade paper ~now_ns:s.now_ns ~price ~size ~aggressor);
      let sched = feed sched in
      let sched, actions =
        apply sched (Tick { now_ns = s.now_ns; book = s.book; traded = s.traded })
      in
      Paper.send paper ~now_ns:s.now_ns actions;
      feed sched)
  in
  sched, Queue.to_list journal
;;

let print_journal journal =
  List.iter journal ~f:(fun ((event : Scheduler.Event.t), actions) ->
    (match event with
     | Tick { now_ns; _ } when not (List.is_empty actions) ->
       printf "t=%.1fs\n" (Float.of_int now_ns /. 1e9)
     | Fill f ->
       printf
         "  fill #%d %s @ %s%s\n"
         f.child
         (Size.to_string f.size)
         (Price.to_string f.price)
         (if f.maker then " maker" else "")
     | Closed _ | Cancel | Tick _ -> ());
    List.iter actions ~f:(fun (a : Child.Action.t) ->
      match a with
      | Place c ->
        printf
          "  place #%d %s %s @ %s%s\n"
          c.id
          (Sexp.to_string (Side.sexp_of_t c.side))
          (Size.to_string c.size)
          (Price.to_string c.limit_px)
          (if c.post_only then " post-only" else " ioc")
      | Cancel id -> printf "  cancel #%d\n" id))
;;

let print_report sched =
  match Scheduler.report sched with
  | None ->
    print_s [%message "not done" ~status:(Scheduler.status sched : Scheduler.Status.t)]
  | Some r ->
    printf
      "%s %s: filled %s of %s @ %s, arrival %s, slippage %.2f bps, shortfall %.4f USD \
       (%.2f bps), fees %.4f, children %d, maker %s\n"
      r.algo
      (Cost_report.Outcome.to_string r.outcome)
      (Size.to_string r.filled)
      (Size.to_string r.target)
      (Price.to_string r.avg_px)
      (Price.to_string r.arrival_px)
      r.arrival_slippage_bps
      r.shortfall
      r.shortfall_bps
      r.fees
      r.children
      (Size.to_string r.maker_filled)
;;
