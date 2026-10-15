open! Core
open Execution_algos
open Harness

let%expect_test "book from levels, mid and touch" =
  let b = book ~levels:3 (px 100.) in
  print_s [%sexp (b : Book.t)];
  [%expect
    {|
    ((bid_px (99.99 99.98 99.97)) (bid_sz (1 1 1)) (ask_px (100 100.01 100.02))
     (ask_sz (1 1 1)))
    |}];
  print_s
    [%message
      ""
        ~mid:(Book.mid b : Price.t or_null)
        ~touch_buy:(Book.touch b Buy : Price.t or_null)
        ~at:(Book.size_at b Sell (px 100.01) : Size.t)];
  [%expect {| ((mid (99.995)) (touch_buy (100)) (at 1)) |}]
;;

let%expect_test "create rejects crossed and unsorted books" =
  let show f = print_s [%sexp (Or_error.try_with f : Book.t Or_error.t)] in
  show (fun () -> Book.create ~bids:[ px 100., sz 1. ] ~asks:[ px 100., sz 1. ]);
  show (fun () ->
    Book.create ~bids:[ px 99., sz 1.; px 99.5, sz 1. ] ~asks:[ px 100., sz 1. ]);
  [%expect
    {|
    (Error ("Book.create: crossed" (b 100) (a 100)))
    (Error ("Book.create: levels not best first" bids (px 99.5)))
    |}]
;;

let%expect_test "walk and estimate a buy through three levels" =
  let b = book ~levels:3 (px 100.) in
  let #(filled, notional) =
    Impact.walk b ~side:Buy ~size:(sz 2.5) ~limit:(Impact.no_limit Buy)
  in
  printf
    "%s %s\n"
    (Size.to_string filled)
    (Exchange_core.Usd.to_string (Exchange_core.Usd.of_units notional));
  [%expect {| 2.5 250.02 |}];
  print_s [%sexp (Impact.estimate b ~side:Buy ~size:(sz 2.5) : Impact.Estimate.t option)];
  [%expect
    {|
    (((filled 2.5) (avg_px 100.008) (worst_px 100.02) (mid (99.995))
      (touch_px 100) (slippage_bps 1.3000650032492642)
      (touch_bps 0.79999999999955662) (levels 3) (exhausted false)))
    |}];
  (* a limit at the second level stops the walk there *)
  print_s
    [%sexp
      (Impact.estimate b ~side:Buy ~size:(sz 5.) ~limit:(px 100.01)
       |> Option.map ~f:(fun e -> e.filled, e.exhausted)
       : (Size.t * bool) option)];
  [%expect {| ((2 true)) |}];
  print_s [%sexp (Impact.depth_within b ~side:Sell ~bps:2 : Size.t)];
  [%expect {| 2 |}]
;;

let%expect_test "limit_from moves against the taker" =
  print_s
    [%message
      ""
        ~buy:(Impact.limit_from ~reference:(px 100.) ~side:Buy ~bps:20 : Price.t)
        ~sell:(Impact.limit_from ~reference:(px 100.) ~side:Sell ~bps:20 : Price.t)];
  [%expect {| ((buy 100.2) (sell 99.8)) |}]
;;

let%expect_test "volume profile" =
  let hour = 3_600_000_000_000 in
  let u = Volume_profile.uniform () in
  printf
    "%.4f %.4f\n"
    (Volume_profile.weight_between u ~start_ns:0 ~stop_ns:hour)
    (Volume_profile.fraction_done u ~start_ns:0 ~stop_ns:hour ~now_ns:(hour / 4));
  [%expect {| 0.0417 0.2500 |}];
  (* all the volume in the first half hour of the day: a VWAP over the first hour is
     almost done after 30 minutes *)
  let trades =
    List.init 6 ~f:(fun i ->
      { Market_data.Trade.instrument = Market_data.Instrument.of_string "hyperliquid:BTC"
      ; exch_ts = Time_ns.of_int_ns_since_epoch (i * 300_000_000_000)
      ; recv_ts = Time_ns.epoch
      ; price = px 100.
      ; size = sz 1.
      ; side = Buy
      ; trade_id = Int.to_string i
      })
  in
  let p = Volume_profile.of_trades trades in
  printf
    "%.4f %.4f\n"
    (Volume_profile.fraction_done p ~start_ns:0 ~stop_ns:hour ~now_ns:(hour / 2))
    (Volume_profile.fraction_done p ~start_ns:0 ~stop_ns:hour ~now_ns:(hour / 4));
  [%expect {| 0.9998 0.4999 |}]
;;
