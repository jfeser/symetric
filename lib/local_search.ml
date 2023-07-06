open Std

module type Subst_intf = sig
  type t [@@deriving sexp]
  type k
  type v

  val empty : t
  val set : t -> k -> v -> t
  val get : t -> k -> v option
end

module type Term_set_intf = sig
  type t [@@deriving compare, sexp]
  type op

  val heads : t -> (op * t list) Iter.t
end

let tabu (type state) ?(max_tabu = 10) ~neighbors state start k =
  let (module State : Base.Hashable.Key with type t = state) = state in
  let seen = Hash_queue.create @@ Base.Hashable.of_key state in
  let rec loop current =
    let m_next =
      Iter.find_pred (fun c -> not (Hash_queue.mem seen c)) (neighbors current)
    in
    match m_next with
    | Some next ->
        (* print_s [%message (current : State.t) (next : State.t)]; *)
        Hash_queue.enqueue_back_exn seen next ();
        if Hash_queue.length seen > max_tabu then Hash_queue.drop_front seen;
        k next;
        loop next
    | None -> ()
  in
  Hash_queue.enqueue_back_exn seen start ();
  loop start

module type Value_intf = sig
  type t [@@deriving compare, hash, sexp]
end

let rewrite_all unnormalize t k =
  let rec rewrite_all t k =
    unnormalize t |> List.iter ~f:k;
    let (Program.Apply (op, args)) = t in
    List.iteri args ~f:(fun i t' ->
        rewrite_all t' (fun p ->
            k @@ Program.Apply (op, List.take args ~n:i @ (p :: List.drop args (i + 1)))))
  in
  rewrite_all t k

let of_unnormalize_tabu (type op value) ?(max_tabu = 1000) ?(random = false)
    ~target_distance (module Op : Value_intf with type t = op)
    (module Value : Value_intf with type t = value) unnormalize eval start =
  let module State = struct
    type t = { program : Op.t Program.t; value : Value.t }
    [@@deriving compare, hash, sexp]
  end in
  let neighbors (t : State.t) =
    let cmp = [%compare: float * _] in

    let choices =
      rewrite_all unnormalize t.program
      |> Iter.map (fun p ->
             let value = eval p in
             (-.target_distance value, State.{ program = p; value }))
      |> Iter.to_list
    in

    let n_sample = List.length choices / 2 in
    let n_sample = if n_sample > 0 then n_sample else List.length choices in
    let ret =
      if random then List.permute choices |> Iter.of_list
      else
        Iter.of_list choices |> Iter.sample n_sample |> Iter.of_array
        |> Iter.top_k ~compare:[%compare: float * _] (max_tabu + 1)
        |> Iter.map (fun (d, x) -> (-.d, x))
        |> Iter.sort ~cmp
    in
    Iter.map Tuple.T2.get2 ret
  in

  tabu ~max_tabu ~neighbors (module State) { program = start; value = eval start }
  |> Iter.map (fun s -> s.State.program)
