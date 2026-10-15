open! Core
module Price = Market_data.Price
module Size = Market_data.Size
module Side = Market_data.Side

module Event = struct
  type t =
    | Fill of Fill.t
    | Closed of int
  [@@deriving sexp, compare, equal]
end

type resting =
  { child : Child.t
  ; mutable left : int
  ; mutable queue : int
  }

type t =
  { latency_ns : int
  ; taker_fee_bps : float
  ; maker_fee_bps : float
  ; mutable book : Book.t
  ; mutable used_bids : int array (** taken by IOCs from this book, per level *)
  ; mutable used_asks : int array
  ; mutable resting : resting list
  ; pending : (int * Child.Action.t) Queue.t
  ; events : Event.t Queue.t
  }

let create ?(latency = Time_ns.Span.zero) ?(taker_fee_bps = 4.5) ?(maker_fee_bps = 1.5) ()
  =
  { latency_ns = Time_ns.Span.to_int_ns latency
  ; taker_fee_bps
  ; maker_fee_bps
  ; book = Book.empty
  ; used_bids = [||]
  ; used_asks = [||]
  ; resting = []
  ; pending = Queue.create ()
  ; events = Queue.create ()
  }
;;

let fee bps ~size ~price = Size.to_float size *. Price.to_float price *. bps /. 10_000.

let emit_fill t ~(child : Child.t) ~units ~price ~maker =
  let size = Size.of_units units in
  Queue.enqueue
    t.events
    (Fill
       { child = child.id
       ; size
       ; price
       ; fee = fee (if maker then t.maker_fee_bps else t.taker_fee_bps) ~size ~price
       ; maker
       })
;;

let crosses (side : Side.t) px ~limit =
  match side with
  | Buy -> Price.( <= ) px limit
  | Sell -> Price.( >= ) px limit
;;

let execute_ioc t (child : Child.t) =
  let r : Side.t = Side.flip child.side in
  let used =
    match r with
    | Buy -> t.used_bids
    | Sell -> t.used_asks
  in
  let n = Book.levels t.book r in
  let left = ref (Size.to_units child.size) in
  let i = ref 0 in
  while !left > 0 && !i < n && crosses child.side (Book.px t.book r !i) ~limit:child.limit_px do
    let avail = Size.to_units (Book.sz t.book r !i) - used.(!i) in
    let take = Int.min !left avail in
    if take > 0
    then (
      used.(!i) <- used.(!i) + take;
      left := !left - take;
      emit_fill t ~child ~units:take ~price:(Book.px t.book r !i) ~maker:false);
    incr i
  done;
  Queue.enqueue t.events (Closed child.id)
;;

let rest t (child : Child.t) =
  let would_cross =
    match Book.touch t.book child.side with
    | This px -> crosses child.side px ~limit:child.limit_px
    | Null -> false
  in
  if would_cross
  then Queue.enqueue t.events (Closed child.id)
  else
    t.resting
    <- t.resting
       @ [ { child
           ; left = Size.to_units child.size
           ; queue = Size.to_units (Book.size_at t.book child.side child.limit_px)
           }
         ]
;;

let cancel t id =
  match List.find t.resting ~f:(fun r -> r.child.id = id) with
  | None -> ()
  | Some _ ->
    t.resting <- List.filter t.resting ~f:(fun r -> r.child.id <> id);
    Queue.enqueue t.events (Closed id)
;;

let apply_action t (action : Child.Action.t) =
  match action with
  | Place child when child.post_only -> rest t child
  | Place child -> execute_ioc t child
  | Cancel id -> cancel t id
;;

let advance t ~now_ns =
  while
    (not (Queue.is_empty t.pending)) && fst (Queue.peek_exn t.pending) <= now_ns
  do
    let _, action = Queue.dequeue_exn t.pending in
    apply_action t action
  done
;;

let send t ~now_ns actions =
  List.iter actions ~f:(fun a -> Queue.enqueue t.pending (now_ns + t.latency_ns, a));
  if t.latency_ns = 0 then advance t ~now_ns
;;

let drop_done t =
  let finished, live = List.partition_tf t.resting ~f:(fun r -> r.left <= 0) in
  t.resting <- live;
  List.iter finished ~f:(fun r -> Queue.enqueue t.events (Closed r.child.id))
;;

let set_book t ~now_ns book =
  advance t ~now_ns;
  t.book <- book;
  t.used_bids <- Array.create ~len:(Book.levels book Buy) 0;
  t.used_asks <- Array.create ~len:(Book.levels book Sell) 0;
  List.iter t.resting ~f:(fun r ->
    let side = r.child.side in
    let px = r.child.limit_px in
    (* the queue cannot be longer than what the book now shows at the price, if the
       price is inside the shown range *)
    let n = Book.levels book side in
    let inside =
      n > 0
      &&
      match side with
      | Buy -> Price.( >= ) px (Book.px book side (n - 1))
      | Sell -> Price.( <= ) px (Book.px book side (n - 1))
    in
    if inside
    then r.queue <- Int.min r.queue (Size.to_units (Book.size_at book side px));
    (* the opposite side reached the price: those sellers traded with the queue first,
       then with us *)
    let opp = Side.flip side in
    let m = Book.levels book opp in
    let reach = ref 0 in
    for i = 0 to m - 1 do
      if crosses side (Book.px book opp i) ~limit:px
      then reach := !reach + Size.to_units (Book.sz book opp i)
    done;
    if !reach > 0
    then (
      let past_queue = Int.max 0 (!reach - r.queue) in
      r.queue <- Int.max 0 (r.queue - !reach);
      let take = Int.min r.left past_queue in
      if take > 0
      then (
        r.left <- r.left - take;
        emit_fill t ~child:r.child ~units:take ~price:px ~maker:true)));
  drop_done t
;;

let trade t ~now_ns ~price ~size ~(aggressor : Side.t) =
  advance t ~now_ns;
  let left = ref (Size.to_units size) in
  List.iter t.resting ~f:(fun r ->
    if !left > 0 && not (Side.equal r.child.side aggressor)
    then (
      let px = r.child.limit_px in
      let through =
        match r.child.side with
        | Buy -> Price.( < ) price px
        | Sell -> Price.( > ) price px
      in
      if through
      then (
        let take = Int.min r.left !left in
        r.left <- r.left - take;
        left := !left - take;
        if take > 0 then emit_fill t ~child:r.child ~units:take ~price:px ~maker:true)
      else if Price.equal price px
      then (
        let past = Int.max 0 (!left - r.queue) in
        r.queue <- Int.max 0 (r.queue - !left);
        let take = Int.min r.left past in
        r.left <- r.left - take;
        left := !left - take;
        if take > 0 then emit_fill t ~child:r.child ~units:take ~price:px ~maker:true)));
  drop_done t
;;

let take_events t =
  let events = Queue.to_list t.events in
  Queue.clear t.events;
  events
;;

let book t = t.book

let resting t =
  List.map t.resting ~f:(fun r ->
    r.child, Size.of_units r.left, Size.of_units r.queue)
;;

let in_flight t = Queue.length t.pending
