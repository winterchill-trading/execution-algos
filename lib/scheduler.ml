open! Core
module Price = Market_data.Price
module Size = Market_data.Size
module Side = Market_data.Side

module Event = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Tick of
            { now_ns : int
            ; book : Book.Stable.V1.t
            ; traded : Size.Stable.V1.t
            }
        | Fill of Fill.Stable.V1.t
        | Closed of int
        | Cancel
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1
end

module Working = struct
  type t =
    { child : Child.Stable.V1.t
    ; remaining : Size.Stable.V1.t
    ; cancel_sent : bool
    }
  [@@deriving bin_io, sexp, compare, equal]
end

module Phase = struct
  type t =
    | Passive
    | Aggressive
  [@@deriving bin_io, sexp, compare, equal]
end

module Status = struct
  type t =
    | Pending
    | Working
    | Done of Cost_report.Outcome.Stable.V1.t
  [@@deriving bin_io, sexp, compare, equal]
end

type t =
  { algo : Algo.Stable.V1.t
  ; parent : Parent.Stable.V1.t
  ; profile : Volume_profile.Stable.V1.t
  ; status : Status.t
  ; phase : Phase.t
  ; start_ns : int
  ; deadline_ns : int (** [max_int] for [Immediate] *)
  ; expired : bool (** a tick at or after the deadline was seen *)
  ; cancelling : bool
  ; sweeping : bool (** the final IOC of [Passive]/[Adaptive] was sent *)
  ; arrival_px : Price.Stable.V1.t
  ; last_px : Price.Stable.V1.t (** mid at the last tick that acted *)
  ; last_ns : int
  ; traded_start : Size.Stable.V1.t
  ; traded_seen : Size.Stable.V1.t (** market volume at the last tick that acted *)
  ; filled : Size.Stable.V1.t
  ; notional : float
  ; maker_filled : Size.Stable.V1.t
  ; fees : float
  ; working : Working.t list
  ; next_id : int
  ; next_slice : int
  }
[@@deriving bin_io, sexp, compare, equal]

let create ?profile algo (parent : Parent.t) =
  ok_exn (Algo.validate algo);
  { algo
  ; parent
  ; profile = Option.value_or_thunk profile ~default:(fun () -> Volume_profile.uniform ())
  ; status = Pending
  ; phase = Aggressive
  ; start_ns = 0
  ; deadline_ns = Int.max_value
  ; expired = false
  ; cancelling = false
  ; sweeping = false
  ; arrival_px = parent.reference_px
  ; last_px = parent.reference_px
  ; last_ns = 0
  ; traded_start = Size.zero
  ; traded_seen = Size.zero
  ; filled = Size.zero
  ; notional = 0.
  ; maker_filled = Size.zero
  ; fees = 0.
  ; working = []
  ; next_id = 0
  ; next_slice = 0
  }
;;

(* {2 Arithmetic on units} *)

let rec working_units = function
  | [] -> 0
  | (w : Working.t) :: rest -> Size.to_units w.remaining + working_units rest
;;

let[@inline] free_units t =
  Size.to_units t.parent.size - Size.to_units t.filled - working_units t.working
;;

let[@inline] lot_units t = Size.to_units t.parent.lot
let[@inline] round_lot t u = if u <= 0 then 0 else u / lot_units t * lot_units t

let[@inline] min_child_units t =
  Int.max (lot_units t) (round_lot t (Size.to_units t.parent.min_size + lot_units t - 1))
;;

(* The cumulative quantity a participation [rate] allows given the market volume since the
   start, on the lot grid. *)
let[@inline] allowed_units t ~rate ~traded =
  let vol = Size.to_units traded - Size.to_units t.traded_start in
  if vol <= 0 then 0 else round_lot t (Float.iround_down_exn (rate *. Float.of_int vol))
;;

let participation (algo : Algo.t) =
  match algo with
  | Twap { max_participation; _ } | Vwap { max_participation; _ } -> max_participation
  | Pov { participation; _ } -> Some participation
  | Immediate | Passive _ | Adaptive _ -> None
;;

let slices (algo : Algo.t) =
  match algo with
  | Twap { slices; _ } | Vwap { slices; _ } -> slices
  | Immediate | Pov _ | Passive _ | Adaptive _ -> 1
;;

let[@inline] slice_time t k =
  t.start_ns + ((t.deadline_ns - t.start_ns) / slices t.algo * k)
;;

(* The price a post-only child pegs to: the best price on the parent's own side, never
   past the limit. [Null] while that side is empty. *)
let[@inline] peg t book : Price.t or_null =
  match t.parent.side with
  | Buy ->
    (match Book.best_bid book with
     | This b -> This (Price.min b t.parent.limit_px)
     | Null -> Null)
  | Sell ->
    (match Book.best_ask book with
     | This a -> This (Price.max a t.parent.limit_px)
     | Null -> Null)
;;

let[@inline] touch_within_limit t book =
  match Book.touch book t.parent.side with
  | This px ->
    (match t.parent.side with
     | Buy -> Price.( <= ) px t.parent.limit_px
     | Sell -> Price.( >= ) px t.parent.limit_px)
  | Null -> false
;;

let[@inline] drift_bps t book =
  match Book.mid book with
  | Null -> 0
  | This mid ->
    let a = Price.to_units t.arrival_px in
    if a <= 0
    then 0
    else (
      let d = (Price.to_units mid - a) * 10_000 / a in
      match t.parent.side with
      | Buy -> d
      | Sell -> -d)
;;

let rec has_live_resting = function
  | [] -> false
  | (w : Working.t) :: rest ->
    (w.child.post_only && not w.cancel_sent) || has_live_resting rest
;;

let rec live_off_peg ~px = function
  | [] -> false
  | (w : Working.t) :: rest ->
    (w.child.post_only && (not w.cancel_sent) && not (Price.equal w.child.limit_px px))
    || live_off_peg ~px rest
;;

(* {2 Idle detection: no allocation} *)

let needs_tick t ~now_ns ~book ~traded =
  match t.status with
  | Done _ -> false
  | Pending -> true
  | Working ->
    if t.cancelling
    then false
    else if now_ns >= t.deadline_ns
    then not t.expired
    else (
      let free = free_units t in
      match t.algo with
      | Immediate -> false
      | Twap { max_participation; _ } | Vwap { max_participation; _ } ->
        now_ns >= slice_time t t.next_slice
        || (Option.is_some max_participation
            && free >= min_child_units t
            && Size.( > ) traded t.traded_seen)
      | Pov { participation; _ } ->
        let allowed = allowed_units t ~rate:participation ~traded in
        allowed - Size.to_units t.filled - working_units t.working >= min_child_units t
        && free >= min_child_units t
      | Passive _ | Adaptive _ ->
        (match t.phase with
         | Aggressive -> free >= min_child_units t && touch_within_limit t book
         | Passive ->
           let off_peg =
             match peg t book with
             | This px ->
               live_off_peg ~px t.working
               || ((not (has_live_resting t.working)) && free >= min_child_units t)
             | Null -> false
           in
           off_peg
           ||
             (match t.algo with
             | Adaptive { max_drift_bps; _ } -> drift_bps t book >= max_drift_bps
             | _ -> false)))
;;

(* {2 Transitions} *)

let place t ~size ~limit_px ~post_only =
  let child = { Child.id = t.next_id; side = t.parent.side; size; limit_px; post_only } in
  ( { t with
      next_id = t.next_id + 1
    ; working = t.working @ [ { Working.child; remaining = size; cancel_sent = false } ]
    }
  , Child.Action.Place child )
;;

let cancel_resting t =
  let actions = ref [] in
  let working =
    List.map t.working ~f:(fun (w : Working.t) ->
      if w.child.post_only && not w.cancel_sent
      then (
        actions := Child.Action.Cancel w.child.id :: !actions;
        { w with cancel_sent = true })
      else w)
  in
  { t with working }, List.rev !actions
;;

let finish t outcome = { t with status = Done outcome }

(* After any event: is the parent done? *)
let settle t =
  match t.status with
  | Pending | Done _ -> t
  | Working ->
    let full = Size.equal t.filled t.parent.size in
    if not (List.is_empty t.working)
    then t
    else if full
    then finish t Filled
    else if t.cancelling
    then finish t Cancelled
    else (
      match t.algo with
      | Immediate -> if t.next_id > 0 then finish t Unfilled else t
      | (Passive _ | Adaptive _) when t.sweeping -> finish t Unfilled
      | _ when t.expired -> finish t Deadline
      | _ -> t)
;;

let ioc t ~units =
  place t ~size:(Size.of_units units) ~limit_px:t.parent.limit_px ~post_only:false
;;

let start t ~now_ns ~book ~traded =
  let arrival_px =
    match Book.mid book with
    | This m -> m
    | Null -> t.parent.reference_px
  in
  let deadline_ns =
    match Algo.horizon t.algo with
    | None -> Int.max_value
    | Some h -> now_ns + Time_ns.Span.to_int_ns h
  in
  let phase : Phase.t =
    match t.algo with
    | Passive _ -> Passive
    | Adaptive { small_ratio; depth_bps; max_behind_bps; _ } ->
      let depth = Impact.depth_within book ~side:t.parent.side ~bps:depth_bps in
      let small =
        Float.( <= ) (Size.to_float t.parent.size) (small_ratio *. Size.to_float depth)
      in
      let behind_ok =
        Float.(t.parent.equity <= 0.)
        || Float.( <= )
             (Size.to_float t.parent.size
              *. Price.to_float arrival_px
              /. t.parent.equity
              *. 10_000.)
             (Float.of_int max_behind_bps)
      in
      if small && behind_ok then Passive else Aggressive
    | Immediate | Twap _ | Vwap _ | Pov _ -> Aggressive
  in
  { t with
    status = Working
  ; start_ns = now_ns
  ; deadline_ns
  ; arrival_px
  ; last_px = arrival_px
  ; last_ns = now_ns
  ; traded_start = traded
  ; traded_seen = traded
  ; phase
  }
;;

let act t ~now_ns ~book ~traded =
  let t =
    { t with
      last_ns = now_ns
    ; traded_seen = traded
    ; last_px =
        (match Book.mid book with
         | This m -> m
         | Null -> t.last_px)
    }
  in
  let min_child = min_child_units t in
  if now_ns >= t.deadline_ns
  then (
    (* the horizon: cancel what rests; Passive and Adaptive sweep the rest once *)
    let t = { t with expired = true } in
    let t, cancels = cancel_resting t in
    match t.algo with
    | (Passive _ | Adaptive _) when not t.sweeping ->
      let free = round_lot t (free_units t) in
      let t = { t with sweeping = true; phase = Aggressive } in
      if free >= min_child
      then (
        let t, a = ioc t ~units:free in
        t, cancels @ [ a ])
      else t, cancels
    | _ -> t, cancels)
  else (
    let free = round_lot t (free_units t) in
    let filled = Size.to_units t.filled in
    let working = working_units t.working in
    match t.algo with
    | Immediate ->
      if t.next_id = 0 && free >= lot_units t
      then (
        let t, a = ioc t ~units:free in
        t, [ a ])
      else t, []
    | Twap _ | Vwap _ ->
      let n = slices t.algo in
      if now_ns >= slice_time t t.next_slice
      then (
        (* the slice containing [now]; slices missed by a late tick are merged *)
        let duration = t.deadline_ns - t.start_ns in
        let k = Int.min (n - 1) ((now_ns - t.start_ns) / Int.max 1 (duration / n)) in
        let frac =
          if k >= n - 1
          then 1.
          else (
            match t.algo with
            | Vwap _ ->
              Volume_profile.fraction_done
                t.profile
                ~start_ns:t.start_ns
                ~stop_ns:t.deadline_ns
                ~now_ns:(slice_time t (k + 1))
            | _ -> Float.of_int (k + 1) /. Float.of_int n)
        in
        let t = { t with next_slice = k + 1 } in
        let target =
          if k >= n - 1
          then Size.to_units t.parent.size
          else
            round_lot
              t
              (Float.iround_down_exn (frac *. Size.to_float t.parent.size *. 1e8))
        in
        let target =
          match participation t.algo with
          | None -> target
          | Some rate -> Int.min target (allowed_units t ~rate ~traded)
        in
        let want = Int.min free (round_lot t (target - filled - working)) in
        if want >= min_child && touch_within_limit t book
        then (
          let t, a = ioc t ~units:want in
          t, [ a ])
        else t, [])
      else (
        (* between slices: a participation cap may have held a slice back *)
        match participation t.algo with
        | None -> t, []
        | Some rate ->
          let k = t.next_slice - 1 in
          let target =
            if k < 0
            then 0
            else if k >= n - 1
            then Size.to_units t.parent.size
            else (
              let frac =
                match t.algo with
                | Vwap _ ->
                  Volume_profile.fraction_done
                    t.profile
                    ~start_ns:t.start_ns
                    ~stop_ns:t.deadline_ns
                    ~now_ns:(slice_time t (k + 1))
                | _ -> Float.of_int (k + 1) /. Float.of_int n
              in
              round_lot
                t
                (Float.iround_down_exn (frac *. Size.to_float t.parent.size *. 1e8)))
          in
          let target = Int.min target (allowed_units t ~rate ~traded) in
          let want = Int.min free (round_lot t (target - filled - working)) in
          if want >= min_child && touch_within_limit t book
          then (
            let t, a = ioc t ~units:want in
            t, [ a ])
          else t, [])
    | Pov { participation; _ } ->
      let allowed = allowed_units t ~rate:participation ~traded in
      let want = Int.min free (round_lot t (allowed - filled - working)) in
      if want >= min_child && touch_within_limit t book
      then (
        let t, a = ioc t ~units:want in
        t, [ a ])
      else t, []
    | Passive _ | Adaptive _ ->
      let t =
        match t.algo, t.phase with
        | Adaptive { max_drift_bps; _ }, Passive when drift_bps t book >= max_drift_bps ->
          { t with phase = Aggressive }
        | _ -> t
      in
      (match t.phase with
       | Aggressive ->
         let t, cancels = cancel_resting t in
         let free = round_lot t (free_units t) in
         if free >= min_child && touch_within_limit t book
         then (
           let t, a = ioc t ~units:free in
           t, cancels @ [ a ])
         else t, cancels
       | Passive ->
         (match peg t book with
          | Null -> t, []
          | This px ->
            let cancels = ref [] in
            let working =
              List.map t.working ~f:(fun (w : Working.t) ->
                if w.child.post_only
                   && (not w.cancel_sent)
                   && not (Price.equal w.child.limit_px px)
                then (
                  cancels := Child.Action.Cancel w.child.id :: !cancels;
                  { w with cancel_sent = true })
                else w)
            in
            let t = { t with working } in
            let cancels = List.rev !cancels in
            let free = round_lot t (free_units t) in
            if (not (has_live_resting t.working)) && free >= min_child
            then (
              let display =
                match t.algo with
                | Passive { display; _ } -> display
                | _ -> 1.
              in
              let clip =
                Int.max
                  min_child
                  (round_lot
                     t
                     (Float.iround_down_exn
                        (display *. Float.of_int (Size.to_units t.parent.size))))
              in
              let t, a =
                place
                  t
                  ~size:(Size.of_units (Int.min free clip))
                  ~limit_px:px
                  ~post_only:true
              in
              t, cancels @ [ a ])
            else t, cancels)))
;;

let tick t ~now_ns ~book ~traded =
  if not (needs_tick t ~now_ns ~book ~traded)
  then #(t, [])
  else (
    let t =
      match t.status with
      | Pending -> start t ~now_ns ~book ~traded
      | Working | Done _ -> t
    in
    let t, actions = act t ~now_ns ~book ~traded in
    #(settle t, actions))
;;

let on_fill t (f : Fill.t) =
  match List.findi t.working ~f:(fun _ (w : Working.t) -> w.child.id = f.child) with
  | None -> t
  | Some (i, w) ->
    let size = Size.min f.size w.remaining in
    let remaining = Size.( - ) w.remaining size in
    let working =
      if Size.is_zero remaining
      then List.filteri t.working ~f:(fun j _ -> j <> i)
      else List.mapi t.working ~f:(fun j x -> if j = i then { w with remaining } else x)
    in
    settle
      { t with
        working
      ; filled = Size.( + ) t.filled size
      ; notional = t.notional +. (Size.to_float size *. Price.to_float f.price)
      ; maker_filled =
          (if f.maker then Size.( + ) t.maker_filled size else t.maker_filled)
      ; fees =
          (* a partial fill clamped to what was working pays its share of the fee *)
          (t.fees
           +.
           if Size.equal size f.size
           then f.fee
           else f.fee *. Size.to_float size /. Size.to_float f.size)
      }
;;

let on_closed t id =
  settle
    { t with
      working = List.filter t.working ~f:(fun (w : Working.t) -> w.child.id <> id)
    }
;;

let on_cancel t =
  match t.status with
  | Done _ -> t, []
  | Pending -> finish { t with cancelling = true } Cancelled, []
  | Working ->
    let t, actions = cancel_resting { t with cancelling = true } in
    settle t, actions
;;

let apply t (event : Event.t) =
  match event with
  | Tick { now_ns; book; traded } ->
    let #(t, actions) = tick t ~now_ns ~book ~traded in
    t, actions
  | Fill f -> on_fill t f, []
  | Closed id -> on_closed t id, []
  | Cancel -> on_cancel t
;;

let step t ~now_ns ~book ~traded ~fills =
  let t = List.fold fills ~init:t ~f:(fun t f -> fst (apply t (Fill f))) in
  apply t (Tick { now_ns; book; traded })
;;

let replay t events =
  let t, actions =
    List.fold events ~init:(t, []) ~f:(fun (t, acc) e ->
      let t, a = apply t e in
      t, a :: acc)
  in
  t, List.rev actions
;;

let algo t = t.algo
let parent t = t.parent
let status t = t.status
let phase t = t.phase

let is_done t =
  match t.status with
  | Done _ -> true
  | Pending | Working -> false
;;

let filled t = t.filled
let remaining t = Size.( - ) t.parent.size t.filled
let working_size t = Size.of_units (working_units t.working)
let working t = t.working
let children t = t.next_id

let arrival t =
  match t.status with
  | Pending -> None
  | Working | Done _ -> Some (t.start_ns, t.arrival_px)
;;

let deadline_ns t =
  match t.status, t.algo with
  | Pending, _ | _, Immediate -> None
  | (Working | Done _), _ -> Some t.deadline_ns
;;

let report ?end_px t =
  match t.status with
  | Pending | Working -> None
  | Done outcome ->
    Some
      (Cost_report.compute
         ~algo:(Algo.name t.algo)
         ~side:t.parent.side
         ~target:t.parent.size
         ~filled:t.filled
         ~notional:t.notional
         ~maker_filled:t.maker_filled
         ~fees:t.fees
         ~arrival_px:t.arrival_px
         ~end_px:(Option.value end_px ~default:t.last_px)
         ~arrival_ns:t.start_ns
         ~end_ns:t.last_ns
         ~children:t.next_id
         ~outcome)
;;
