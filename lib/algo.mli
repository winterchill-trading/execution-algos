(** Which execution algorithm works a parent order, and its parameters. Durations are in
    milliseconds so the sexp form is readable and stable.

    - [Immediate]: one IOC child for the whole parent at the parent's limit; what does not
      fill is left (the caller re-plans). The behaviour of a plain IOC order.
    - [Twap]: [slices] equal IOC children at evenly spaced times over [duration_ms]; a
      slice that did not fill is carried into the next one.
    - [Vwap]: like [Twap], but slice [k] brings the cumulative quantity to the share of
      the window's expected volume ({!Volume_profile}) traded by the end of the slice.
    - [Pov]: IOC children keeping the parent's cumulative quantity at [participation]
      times the market volume traded since the parent started; stops at
      [max_duration_ms].
    - [Passive]: one post-only child at a time, pegged to the best price on the parent's
      own side (re-pegged when the touch moves), showing at most [display] of the parent
      (an iceberg below 1.0); after [timeout_ms] the rest goes out as one IOC.
    - [Adaptive]: the copy algorithm. When the parent is small against the depth within
      [depth_bps] of the mid ([size <= small_ratio * depth]) and the follower is not far
      behind ([remaining notional <= max_behind_bps] of its equity), it works passively;
      otherwise, or once the mid moved [max_drift_bps] against it, or after [timeout_ms],
      it takes liquidity with IOC children until done or until the timeout.

    [max_participation] (TWAP, VWAP) caps the cumulative quantity at that share of the
    market volume since the start, as POV does. *)

open! Core

module Stable : sig
  module V1 : sig
    type t =
      | Immediate
      | Twap of
          { duration_ms : int
          ; slices : int
          ; max_participation : float option
          }
      | Vwap of
          { duration_ms : int
          ; slices : int
          ; max_participation : float option
          }
      | Pov of
          { participation : float
          ; max_duration_ms : int
          }
      | Passive of
          { display : float
          ; timeout_ms : int
          }
      | Adaptive of
          { small_ratio : float
          ; depth_bps : int
          ; max_behind_bps : int
          ; max_drift_bps : int
          ; timeout_ms : int
          }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

type t = Stable.V1.t =
  | Immediate
  | Twap of
      { duration_ms : int
      ; slices : int
      ; max_participation : float option
      }
  | Vwap of
      { duration_ms : int
      ; slices : int
      ; max_participation : float option
      }
  | Pov of
      { participation : float
      ; max_duration_ms : int
      }
  | Passive of
      { display : float
      ; timeout_ms : int
      }
  | Adaptive of
      { small_ratio : float
      ; depth_bps : int
      ; max_behind_bps : int
      ; max_drift_bps : int
      ; timeout_ms : int
      }
[@@deriving bin_io, sexp, compare, equal]

(** Defaults: TWAP 60 s in 6 slices, VWAP 5 min in 10, POV 10% for at most 5 min, passive
    30 s showing everything, iceberg showing a quarter, adaptive with ratio 0.5 within
    10 bps, 50 bps behind, 10 bps drift, 20 s timeout. *)
val twap : t

val vwap : t
val pov : t
val passive : t
val iceberg : t
val adaptive : t

(** ["immediate"], ["twap"], ["vwap"], ["pov"], ["passive"], ["iceberg"], ["adaptive"]. *)
val name : t -> string

(** Parses a name with optional parameters separated by [':'], spans as
    {!Time_ns.Span.of_string} reads them:
    ["immediate"], ["twap[:DURATION[:SLICES]]"], ["vwap[:DURATION[:SLICES]]"],
    ["pov[:RATE[:MAX_DURATION]]"], ["passive[:TIMEOUT]"],
    ["iceberg[:DISPLAY[:TIMEOUT]]"], ["adaptive[:TIMEOUT]"], or a sexp of [t]. *)
val of_string : string -> t Or_error.t

(** The short form {!of_string} reads back. *)
val to_string : t -> string

(** Positive durations and slices, rates and display in (0, 1], ratios positive. *)
val validate : t -> unit Or_error.t

(** When the parent must be finished, from its start; [None] for [Immediate]. *)
val horizon : t -> Time_ns.Span.t option

(** {!of_string} and {!to_string} for [Command] flags. *)
val arg_type : t Command.Arg_type.t
