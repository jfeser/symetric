open Std
module P = Program

module Type = struct
  type t = Int | Tower [@@deriving compare, equal, hash, sexp, yojson]

  let default = Int
  let output = Tower
  let pp fmt x = Sexp.pp fmt @@ [%sexp_of: t] x
end

module Op = struct
  type t =
    | Loop
    | Move_s of int
    | Move_p of int
    | Move_l
    | Move_r
    | Drop_v
    | Drop_h
    | Embed
    | Int of int
    | Seq
  [@@deriving compare, equal, hash, sexp, yojson]

  let default = Int (-1)
  let cost _ = 1

  let ret_type : _ -> Type.t = function
    | Loop | Move_s _ | Move_p _ | Drop_v | Drop_h | Embed | Seq -> Tower
    | Int _ -> Int
    | _ -> assert false

  let args_type : _ -> Type.t list = function
    | Embed -> [ Tower ]
    | Seq -> [ Tower; Tower ]
    | Move_s _ -> [ Tower ]
    | Move_p _ -> [ Tower ]
    | Loop -> [ Int; Tower ]
    | Drop_v | Drop_h | Int _ -> []
    | _ -> assert false

  let arity op = List.length @@ args_type op
  let is_commutative _ = false
  let pp fmt x = Sexp.pp_hum fmt @@ [%sexp_of: t] x
end

module Value = struct
  module Block = struct
    module T = struct
      type t = { x : int; y : int; kind : int } [@@deriving compare, equal, hash, sexp]
    end

    include T
    include Comparator.Make (T)
  end

  module State = struct
    type t = { hand : int; tops : Small_int_array.t; blocks : Set.M(Block).t }
    [@@deriving compare, equal, hash, sexp]

    let default dim =
      {
        hand = 0;
        tops = Small_int_array.init (dim + 2) ~f:(fun _ -> 0);
        blocks = Set.empty (module Block);
      }
  end

  type transition = {
    prog : Op.t Program.t;
    func : State.t -> State.t; [@ignore]
    summary : State.t list; [@ignore]
  }
  [@@deriving compare, equal, hash, sexp]

  type t = Int of int | Trans of transition [@@deriving compare, equal, hash, sexp]

  let default = Int (-1)

  module Ctx = struct
    type t = { summary_states : State.t list; dim : int } [@@deriving sexp]

    let create ?(dim = 32)
        ?(summary_states =
          [
            State.default dim;
            (* { (State.default dim) with hand = dim / 2 }; *)
            (* { (State.default dim) with hand = dim - 2 }; *)
          ]) () =
      { summary_states; dim }
  end

  let pp (ctx : Ctx.t) fmt (s : State.t) =
    let b = s.blocks in
    let is_h b x y = Set.mem b Block.{ x; y; kind = 0 } in
    let is_v b x y = Set.mem b Block.{ x; y; kind = 1 } in
    let is_on b x y =
      is_h b x y
      || is_h b (x - 1) y
      || is_h b (x - 2) y
      || is_v b x y
      || is_v b x (y - 1)
      || is_v b x (y - 2)
    in

    Fmt.pf fmt "@[<v>";
    Fmt.pf fmt "%a@," Sexp.pp_hum ([%sexp_of: State.t] s);
    for i = 0 to ctx.dim - 1 do
      if i = s.hand then Fmt.pf fmt "v" else Fmt.pf fmt " "
    done;
    Fmt.pf fmt "@,";
    for j = ctx.dim - 1 downto 0 do
      for i = 0 to ctx.dim - 1 do
        if is_on b i j then Fmt.pf fmt "█" else Fmt.pf fmt "."
      done;
      Fmt.pf fmt "@,"
    done;
    Fmt.pf fmt "@]"

  let empty = Set.empty (module Int)

  let eval_unmemoized (ctx : Ctx.t) (op : Op.t) args : t =
    let fail () = raise_s [%message "unexpected arguments" (op : Op.t) (args : t list)] in
    let prog =
      let arg_progs =
        List.map args ~f:(function Trans t -> t.prog | Int x -> Apply (Op.Int x, []))
      in
      P.Apply (op, arg_progs)
    in
    let trans func =
      Trans
        {
          func;
          summary =
            (let s0 = State.default ctx.dim in
             let s1 = func s0 in
             let s2 = func s1 in
             let s3 = func s2 in
             [ s1; s2; s3 ]);
          prog;
        }
    in
    match (op, args) with
    | Int _, _ :: _ -> fail ()
    | Int x, [] -> Int x
    | (Drop_h | Drop_v), _ :: _ -> fail ()
    | Drop_h, [] ->
        let func (s : State.t) : State.t =
          let v_pos =
            Small_int_array.(
              max (get s.tops s.hand)
              @@ max (get s.tops (s.hand + 1)) (get s.tops (s.hand + 2)))
          in
          if v_pos >= ctx.dim then s
          else
            let blocks = Set.add s.blocks { x = s.hand; y = v_pos; kind = 0 } in
            let tops =
              Small_int_array.set_many s.tops
                (Iter.int_range ~start:s.hand ~stop:(min (ctx.dim - 1) (s.hand + 2))
                |> Iter.map (fun i -> (i, v_pos + 1)))
            in
            { s with blocks; tops }
        in
        trans func
    | Drop_v, [] ->
        let func (s : State.t) : State.t =
          let v_pos = Small_int_array.get s.tops s.hand in
          if v_pos >= ctx.dim then s
          else
            let blocks = Set.add s.blocks { x = s.hand; y = v_pos; kind = 1 } in
            let tops = Small_int_array.set s.tops s.hand (v_pos + 3) in
            { s with blocks; tops }
        in
        trans func
    | Embed, ([] | [ Int _ ] | _ :: _ :: _) -> fail ()
    | Embed, [ Trans p ] -> trans (fun s -> { (p.func s) with hand = s.hand })
    | Seq, ([] | [ _ ] | [ Int _; _ ] | [ _; Int _ ] | _ :: _ :: _ :: _) -> fail ()
    | Seq, [ Trans t; Trans p ] -> trans (fun s -> t.func s |> p.func)
    | (Move_s _ | Move_p _), ([] | [ Int _ ] | _ :: _ :: _) -> fail ()
    | Move_s x, [ Trans t ] ->
        trans (fun s -> { (t.func s) with hand = min (ctx.dim - 2) (max 0 (s.hand + x)) })
    | Move_p x, [ Trans t ] ->
        trans (fun s -> t.func { s with hand = min (ctx.dim - 2) (max 0 (s.hand + x)) })
    | Loop, ([] | [ _ ] | [ Trans _; _ ] | [ _; Int _ ] | _ :: _ :: _ :: _) -> fail ()
    | Loop, [ Int x; Trans p ] ->
        let func (s : State.t) : State.t =
          let rec loop x' s = if x' >= x then s else loop (x' + 1) (p.func s) in
          loop 0 s
        in
        trans func
    | _ -> assert false

  let mk_eval_memoized () =
    let module Key = struct
      module T = struct
        type nonrec t = Op.t * t list [@@deriving compare, hash, sexp]
      end

      include T
      include Comparable.Make (T)
    end in
    let tbl = Hashtbl.create (module Key) in
    let find_or_eval (ctx : Ctx.t) op args =
      match Hashtbl.find tbl (op, args) with
      | Some v -> v
      | None ->
          let v = eval_unmemoized ctx op args in
          Hashtbl.set tbl ~key:(op, args) ~data:v;
          v
    in
    find_or_eval

  let eval = eval_unmemoized
  let is_error _ = false

  (* let emd (ctx : Ctx.t) s s' = *)
  (*   let emd, _ = *)
  (*     Iter.int_range ~start:0 ~stop:(ctx.dim - 1) *)
  (*     |> Iter.fold *)
  (*          (fun (emd_tot, emd_prev) i -> *)
  (*            let p = Bool.to_int (Set.mem s i) in *)
  (*            let q = Bool.to_int (Set.mem s' i) in *)
  (*            let emd = p + emd_prev - q in *)
  (*            (emd_tot + abs emd, emd)) *)
  (*          (0, 0) *)
  (*   in *)
  (*   emd *)

  let emd _ s s' =
    let _, _, emd =
      Set.merge_to_sequence ~order:`Increasing s s'
      |> Sequence.fold ~init:(0, 0, 0) ~f:(fun (emd_prev, prev_bucket, emd_tot) ->
           function
           | Set.Merge_to_sequence_element.Left bucket ->
               let emd = 1 + emd_prev in
               let emd_tot =
                 emd_tot + abs ((bucket - prev_bucket) * emd_prev) + abs emd
               in
               (emd, bucket, emd_tot)
           | Right bucket ->
               let emd = emd_prev - 1 in
               let emd_tot =
                 emd_tot + abs ((bucket - prev_bucket) * emd_prev) + abs emd
               in
               (emd, bucket, emd_tot)
           | Both (bucket, _) ->
               let emd = emd_prev in
               let emd_tot =
                 emd_tot + abs ((bucket - prev_bucket) * emd_prev) + abs emd
               in
               (emd, bucket, emd_tot))
    in
    emd

  let blocks_distance ?(add_remove_penalty = 5) _ctx s s' =
    Map.fold_symmetric_diff s s' ~data_equal:[%equal: Set.M(Int).t * Set.M(Int).t] ~init:0
      ~f:(fun d (_, rows) ->
        let l1_h, l1_v, emd_h, emd_v =
          match rows with
          | `Left (h_row, v_row) | `Right (h_row, v_row) ->
              (Set.length h_row, Set.length v_row, 0, 0)
          | `Unequal ((h_row, v_row), (h_row', v_row')) ->
              let l1_h = abs (Set.length h_row - Set.length h_row') in
              let l1_v = abs (Set.length v_row - Set.length v_row') in
              let emd_h = emd _ctx h_row h_row' in
              let emd_v = emd _ctx v_row v_row' in
              (l1_h, l1_v, emd_h, emd_v)
        in
        d + ((l1_h + l1_v) * add_remove_penalty) + emd_h + emd_v)

  let row_distance s s' =
    let add_penalty = 1 and remove_penalty = 5 in
    if Set.is_empty s then Set.length s' * remove_penalty
    else if Set.is_empty s' then Set.length s * add_penalty
    else
      (* let min = Set.min_elt_exn s in *)
      (* let min' = Set.min_elt_exn s' in *)
      (* let diff = min' - min in *)
      (* let s' = Set.map (module Int) ~f:(fun x -> x - diff) s' in *)
      (* diff *)
      (* + *)
      (add_penalty * Set.length (Set.diff s s'))
      + (remove_penalty * Set.length (Set.diff s' s))

  let blocks_distance _ctx s s' =
    let n = Float.of_int @@ Set.length (Set.inter s s') in
    let d = Float.of_int @@ Set.length (Set.union s s') in
    if Float.(d = 0.) then 0. else 100. *. (1. -. (n /. d))

  let distance ctx v v' =
    match (v, v') with
    | Int x, Int x' -> if x = x' then 0. else Float.infinity
    | Trans x, Trans x' ->
        if [%equal: transition] x x' then 0.
        else
          (* if *)
          (*   List.for_all2_exn x.summary x'.summary ~f:(fun s s' -> *)
          (*       Set.is_empty s.blocks && Set.is_empty s'.blocks) *)
          (* then *)
          (*   if List.exists2_exn x.summary x.summary ~f:(fun s s' -> s.hand <> s'.hand) then *)
          (*     1.0 *)
          (*   else 0. *)
          (* else *)
          let dist =
            List.map2_exn x.summary x'.summary ~f:(fun s s' ->
                blocks_distance ctx s.blocks s'.blocks
                (* +. Float.of_int (abs (s.hand - s'.hand)) *))
            |> Iter.of_list |> Iter.mean
          in
          (* Fmt.epr "Distance (d=%f):@,%a@,%a@." dist (pp ctx) *)
          (*   (List.hd_exn x.summary).blocks (pp ctx) (List.hd_exn x'.summary).blocks; *)
          Option.value ~default:0. dist
    | _, _ -> Float.infinity

  let blocks_distance _ctx s s' =
    Set.length (Set.diff s s') + (5 * Set.length (Set.diff s' s))

  let shift_down =
    Set.filter_map
      (module Block)
      ~f:(fun (b : Block.t) -> if b.y > 0 then Some { b with y = b.y - 1 } else None)

  let shift_right (ctx : Ctx.t) bs =
    if
      (not (Set.is_empty bs))
      && Set.for_all bs ~f:(fun (b : Block.t) -> b.x < ctx.dim - 1)
    then Some (Set.map (module Block) bs ~f:(fun (b : Block.t) -> { b with x = b.x + 1 }))
    else None

  let shift_up (ctx : Ctx.t) bs =
    if
      (not (Set.is_empty bs))
      && Set.for_all bs ~f:(fun (b : Block.t) -> b.y < ctx.dim - 1)
    then Some (Set.map (module Block) bs ~f:(fun (b : Block.t) -> { b with y = b.y + 1 }))
    else None

  let motif_count ctx b b' =
    let rec rshift ct b' =
      let ct = ct + if Set.is_subset b' ~of_:b then 1 else 0 in
      match shift_right ctx b' with Some b' -> rshift ct b' | None -> ct
    in
    let rec ushift ct b' =
      let ct = rshift ct b' in
      match shift_up ctx b' with Some b' -> ushift ct b' | None -> ct
    in
    ushift 0 b'

  let rec iter_shifts b f =
    f b;
    let b' = shift_down b in
    if not (Set.is_empty b') then iter_shifts b' f

  let target_distance ctx t = function
    | Int _ -> 1.
    | Trans x ->
        let b = (List.hd_exn x.summary).blocks in
        let b_len = Float.of_int (Set.length b) in
        1.
        /. ((Float.of_int @@ motif_count ctx t (List.hd_exn x.summary).blocks)
           *. Float.(1.5 ** b_len))
end

let int x = P.apply (Op.Int x)
let serialize = [%sexp_of: Op.t P.t]

let rec parse_op : Sexp.t -> Op.t Program.t = function
  | List (Atom "for" :: Atom i :: p) ->
      Apply (Loop, [ int (Int.of_string i); parse (Sexp.List p) ])
  | List (Atom "embed" :: p) -> Apply (Embed, [ parse (List p) ])
  | List [ Atom "l"; Atom i ] -> Apply (Move_l, [ int (Int.of_string i) ])
  | List [ Atom "r"; Atom i ] -> Apply (Move_r, [ int (Int.of_string i) ])
  | Atom "h" -> Apply (Drop_h, [])
  | Atom "v" -> Apply (Drop_v, [])
  | s -> raise_s [%message "unexpected operator" (s : Sexp.t)]

and parse = function
  | List [ x ] -> parse_op x
  | List (x :: xs) -> Apply (Seq, [ parse_op x; parse (List xs) ])
  | List [] | Atom _ -> failwith "unexpected empty list"

let rec desugar : Op.t P.t -> Op.t P.t = function
  | Apply (Seq, [ Apply (Move_l, [ Apply (Int i, []) ]); p ]) ->
      Apply (Move_p (-i), [ desugar p ])
  | Apply (Seq, [ Apply (Move_r, [ Apply (Int i, []) ]); p ]) ->
      Apply (Move_p i, [ desugar p ])
  | Apply (Seq, [ p; Apply (Move_l, [ Apply (Int i, []) ]) ]) ->
      Apply (Move_s (-i), [ desugar p ])
  | Apply (Seq, [ p; Apply (Move_r, [ Apply (Int i, []) ]) ]) ->
      Apply (Move_s i, [ desugar p ])
  | Apply (Move_l, _) | Apply (Move_r, _) -> assert false
  | Apply (op, args) -> Apply (op, List.map ~f:desugar args)

let parse s = parse s |> desugar

let%expect_test "" =
  let bridge =
    parse @@ Sexp.of_string "((for 3 (for 3 v (r 4) v (l 4)) (r 2) h (r 4)))"
  in
  let bridge_top = parse @@ Sexp.of_string "((r 2) (for 3 h (r 6)))" in
  let bridge_pillar = parse @@ Sexp.of_string "((for 3 (embed v (r 4) v)) (r 2) h)" in
  let bridge_pillar_part = parse @@ Sexp.of_string "((for 3 (embed v (r 4))))" in
  let ctx = Value.Ctx.create () in
  let target =
    match P.eval (Value.eval ctx) bridge with
    | Trans t -> (List.hd_exn t.summary).blocks
    | _ -> assert false
  in
  print_s
    [%message
      (Value.target_distance ctx target (P.eval (Value.eval ctx) bridge_top) : float)];
  print_s
    [%message
      (Value.distance ctx
         (P.eval (Value.eval ctx) bridge_pillar)
         (P.eval (Value.eval ctx) bridge_pillar_part)
        : float)];
  print_s
    [%message
      (Value.distance ctx
         (P.eval (Value.eval ctx) bridge_pillar)
         (P.eval (Value.eval ctx) bridge_top)
        : float)];
  print_s
    [%message
      (Value.target_distance ctx target (P.eval (Value.eval ctx) bridge_pillar) : float)];
  print_s
    [%message
      (Value.target_distance ctx target (P.eval (Value.eval ctx) bridge_pillar_part)
        : float)]