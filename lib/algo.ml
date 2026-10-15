open! Core

module Stable = struct
  module V1 = struct
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

include Stable.V1

let twap = Twap { duration_ms = 60_000; slices = 6; max_participation = None }
let vwap = Vwap { duration_ms = 300_000; slices = 10; max_participation = None }
let pov = Pov { participation = 0.1; max_duration_ms = 300_000 }
let passive = Passive { display = 1.0; timeout_ms = 30_000 }
let iceberg = Passive { display = 0.25; timeout_ms = 30_000 }

let adaptive =
  Adaptive
    { small_ratio = 0.5
    ; depth_bps = 10
    ; max_behind_bps = 50
    ; max_drift_bps = 10
    ; timeout_ms = 20_000
    }
;;

let name = function
  | Immediate -> "immediate"
  | Twap _ -> "twap"
  | Vwap _ -> "vwap"
  | Pov _ -> "pov"
  | Passive { display; _ } when Float.(display < 1.) -> "iceberg"
  | Passive _ -> "passive"
  | Adaptive _ -> "adaptive"
;;

let ms_of_span s =
  let open Or_error.Let_syntax in
  let%bind span = Or_error.try_with (fun () -> Time_ns.Span.of_string s) in
  Ok (Int.of_float (Time_ns.Span.to_ms span))
;;

let span_of_ms ms =
  if ms mod 60_000 = 0
  then sprintf "%dm" (ms / 60_000)
  else if ms mod 1000 = 0
  then sprintf "%ds" (ms / 1000)
  else sprintf "%dms" ms
;;

let float_arg s = Or_error.try_with (fun () -> Float.of_string s)
let int_arg s = Or_error.try_with (fun () -> Int.of_string s)

let validate t =
  let errors =
    let pos name x = Option.some_if (x <= 0) (name ^ " must be positive") in
    let frac name x =
      Option.some_if
        (Float.is_nan x || Float.(x <= 0.) || Float.(x > 1.))
        (name ^ " must be in (0, 1]")
    in
    match t with
    | Immediate -> []
    | Twap { duration_ms; slices; max_participation }
    | Vwap { duration_ms; slices; max_participation } ->
      [ pos "duration" duration_ms
      ; pos "slices" slices
      ; Option.bind max_participation ~f:(frac "max_participation")
      ]
    | Pov { participation; max_duration_ms } ->
      [ frac "participation" participation; pos "max_duration" max_duration_ms ]
    | Passive { display; timeout_ms } ->
      [ frac "display" display; pos "timeout" timeout_ms ]
    | Adaptive { small_ratio; depth_bps; max_behind_bps; max_drift_bps; timeout_ms } ->
      [ Option.some_if
          (Float.is_nan small_ratio || Float.(small_ratio <= 0.))
          "small_ratio must be positive"
      ; pos "depth_bps" depth_bps
      ; pos "max_behind_bps" max_behind_bps
      ; pos "max_drift_bps" max_drift_bps
      ; pos "timeout" timeout_ms
      ]
  in
  match List.filter_opt errors with
  | [] -> Ok ()
  | errors -> Or_error.error_s [%message "invalid algo" (errors : string list)]
;;

let of_short s =
  let open Or_error.Let_syntax in
  match String.split (String.lowercase (String.strip s)) ~on:':' with
  | [ "immediate" ] | [ "ioc" ] -> Ok Immediate
  | "twap" :: rest | "vwap" :: rest ->
    let%bind duration_ms, slices =
      match rest with
      | [] -> Ok (None, None)
      | [ d ] ->
        let%map d = ms_of_span d in
        Some d, None
      | [ d; n ] ->
        let%bind d = ms_of_span d in
        let%map n = int_arg n in
        Some d, Some n
      | _ -> Or_error.error_string "too many parameters"
    in
    let is_twap = String.is_prefix (String.lowercase (String.strip s)) ~prefix:"twap" in
    let default = if is_twap then twap else vwap in
    let d0, n0 =
      match default with
      | Twap { duration_ms; slices; _ } | Vwap { duration_ms; slices; _ } ->
        duration_ms, slices
      | _ -> assert false
    in
    let duration_ms = Option.value duration_ms ~default:d0 in
    let slices = Option.value slices ~default:n0 in
    Ok
      (if is_twap
       then Twap { duration_ms; slices; max_participation = None }
       else Vwap { duration_ms; slices; max_participation = None })
  | "pov" :: rest ->
    let%bind participation, max_duration_ms =
      match rest with
      | [] -> Ok (0.1, 300_000)
      | [ r ] ->
        let%map r = float_arg r in
        r, 300_000
      | [ r; d ] ->
        let%bind r = float_arg r in
        let%map d = ms_of_span d in
        r, d
      | _ -> Or_error.error_string "too many parameters"
    in
    Ok (Pov { participation; max_duration_ms })
  | [ "passive" ] -> Ok passive
  | [ "passive"; d ] ->
    let%map timeout_ms = ms_of_span d in
    Passive { display = 1.0; timeout_ms }
  | [ "iceberg" ] -> Ok iceberg
  | [ "iceberg"; f ] ->
    let%map display = float_arg f in
    Passive { display; timeout_ms = 30_000 }
  | [ "iceberg"; f; d ] ->
    let%bind display = float_arg f in
    let%map timeout_ms = ms_of_span d in
    Passive { display; timeout_ms }
  | [ "adaptive" ] -> Ok adaptive
  | [ "adaptive"; d ] ->
    let%map timeout_ms = ms_of_span d in
    (match adaptive with
     | Adaptive a -> Adaptive { a with timeout_ms }
     | _ -> assert false)
  | _ -> Or_error.error_s [%message "unknown algo" s]
;;

let of_string s =
  let open Or_error.Let_syntax in
  let%bind t =
    if String.is_prefix (String.lstrip s) ~prefix:"("
    then Or_error.try_with (fun () -> t_of_sexp (Sexp.of_string s))
    else of_short s
  in
  let%map () = validate t in
  t
;;

let to_string = function
  | Immediate -> "immediate"
  | Twap { duration_ms; slices; max_participation = None } ->
    sprintf "twap:%s:%d" (span_of_ms duration_ms) slices
  | Vwap { duration_ms; slices; max_participation = None } ->
    sprintf "vwap:%s:%d" (span_of_ms duration_ms) slices
  | Pov { participation; max_duration_ms } ->
    sprintf "pov:%g:%s" participation (span_of_ms max_duration_ms)
  | Passive { display; timeout_ms } when Float.(display >= 1.) ->
    sprintf "passive:%s" (span_of_ms timeout_ms)
  | Passive { display; timeout_ms } ->
    sprintf "iceberg:%g:%s" display (span_of_ms timeout_ms)
  | (Twap _ | Vwap _ | Adaptive _) as t ->
    (match t with
     | Adaptive a when [%equal: t] (Adaptive { a with timeout_ms = 20_000 }) adaptive ->
       sprintf "adaptive:%s" (span_of_ms a.timeout_ms)
     | _ -> Sexp.to_string (sexp_of_t t))
;;

let horizon t =
  let ms x = Some (Time_ns.Span.of_int_ms x) in
  match t with
  | Immediate -> None
  | Twap { duration_ms; _ } | Vwap { duration_ms; _ } -> ms duration_ms
  | Pov { max_duration_ms; _ } -> ms max_duration_ms
  | Passive { timeout_ms; _ } | Adaptive { timeout_ms; _ } -> ms timeout_ms
;;

let arg_type = Command.Arg_type.create (fun s -> ok_exn (of_string s))
