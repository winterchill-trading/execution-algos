open! Core
open Execution_algos
open Harness

let book10 = book ~levels:10 (px 100.)

let parent () =
  Parent.create
    ~side:Buy
    ~size:(sz 3.)
    ~limit_px:(px 100.1)
    ~reference_px:(px 100.)
    ~lot:(sz 0.1)
    ()
;;

let started algo =
  let s = Scheduler.create algo (parent ()) in
  let s, _ = Scheduler.apply s (Tick { now_ns = 0; book = book10; traded = sz 1. }) in
  (* the venue answers the first child: filled and closed *)
  let s =
    List.fold (Scheduler.working s) ~init:s ~f:(fun s w ->
      if w.child.post_only
      then s
      else (
        let s, _ =
          Scheduler.apply
            s
            (Fill
               { child = w.child.id
               ; size = w.remaining
               ; price = px 100.
               ; fee = 0.
               ; maker = false
               })
        in
        fst (Scheduler.apply s (Closed w.child.id))))
  in
  s
;;

let twap () = Algo.Twap { duration_ms = 60_000; slices = 6; max_participation = None }
let twap_idle () = started (twap ()), 1_000_000_000, book10, sz 1.
let passive_idle () = started Algo.passive, 1_000_000_000, book10, sz 1.
let twap_first () = Scheduler.create (twap ()) (parent ()), 0, book10, sz 1.
