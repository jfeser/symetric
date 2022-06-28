open Std
module P = Program

module Type = struct
  type t = Int | Tower [@@deriving compare, equal, hash, sexp, yojson]

  let default = Int
  let output = Tower
  let pp fmt x = Sexp.pp fmt @@ [%sexp_of: t] x
end

module Op = struct
  type t = Loop | Move_l | Move_r | Drop_v | Drop_h | Embed | Int of int | Nop
  [@@deriving compare, equal, hash, sexp, yojson]

  let default = Int (-1)
  let cost _ = 1

  let ret_type : _ -> Type.t = function
    | Loop | Move_l | Move_r | Drop_v | Drop_h | Embed | Nop -> Tower
    | Int _ -> Int

  let args_type : _ -> Type.t list = function
    | Drop_v | Drop_h -> [ Tower ]
    | Embed -> [ Tower; Tower ]
    | Move_l | Move_r -> [ Tower; Int ]
    | Loop -> [ Tower; Int; Tower ]
    | Nop | Int _ -> []

  let arity op = List.length @@ args_type op
  let is_commutative _ = false
  let pp fmt x = Sexp.pp_hum fmt @@ [%sexp_of: t] x
end

module Value = struct
  module State = struct
    type t = { hand : int; tops : Small_int_array.t; blocks : Bitarray.t }
    [@@deriving compare, equal, hash, sexp]

    let default dim =
      {
        hand = 1;
        tops = Small_int_array.init dim ~f:(fun _ -> 0);
        blocks = Bitarray.create (dim * dim) false;
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

    let create ?(dim = 32) ?(summary_states = [ State.default dim ]) () =
      { summary_states; dim }
  end

  let pp (ctx : Ctx.t) fmt x =
    Fmt.pf fmt "@[<v>";
    for j = ctx.dim - 1 downto 0 do
      for i = 0 to ctx.dim - 1 do
        if Bitarray.get x (i + (j * ctx.dim)) then Fmt.pf fmt "█" else Fmt.pf fmt "."
      done;
      Fmt.pf fmt "@,"
    done;
    Fmt.pf fmt "@]"

  let eval_unmemoized (ctx : Ctx.t) (op : Op.t) args : t =
    let fail () = raise_s [%message "unexpected arguments" (op : Op.t) (args : t list)] in
    let prog =
      let arg_progs =
        List.map args ~f:(function Trans t -> t.prog | Int x -> Apply (Op.Int x, []))
      in
      P.Apply (op, arg_progs)
    in
    let apply_step step t =
      let func s = step @@ t.func s in
      Trans { prog; func; summary = List.map ~f:step t.summary }
    in
    match (op, args) with
    | (Int _ | Nop), _ :: _ -> fail ()
    | Int x, [] -> Int x
    | Nop, [] -> Trans { func = Fun.id; summary = ctx.summary_states; prog }
    | (Drop_h | Drop_v), ([] | [ Int _ ] | _ :: _ :: _) -> fail ()
    | Drop_h, [ Trans t ] ->
        let step (s : State.t) : State.t =
          let v_pos =
            Small_int_array.(
              max (get s.tops (s.hand - 1))
              @@ max (get s.tops s.hand) (get s.tops (s.hand + 1)))
          in
          if v_pos >= ctx.dim then s
          else
            let h_pos = Iter.int_range ~start:(s.hand - 1) ~stop:(s.hand + 1) in
            let blocks =
              Bitarray.set_many s.blocks
                (Iter.map (fun i -> ((v_pos * ctx.dim) + i, true)) h_pos)
            in
            let tops =
              Small_int_array.set_many s.tops (Iter.map (fun i -> (i, v_pos + 1)) h_pos)
            in
            { s with blocks; tops }
        in
        apply_step step t
    | Drop_v, [ Trans t ] ->
        let step (s : State.t) : State.t =
          let v_min = Small_int_array.get s.tops s.hand in
          if v_min >= ctx.dim then s
          else
            let v_max = min (v_min + 2) (ctx.dim - 1) in
            let blocks =
              Bitarray.set_many s.blocks
                (Iter.int_range ~start:v_min ~stop:v_max
                |> Iter.map (fun j -> (s.hand + (j * ctx.dim), true)))
            in
            let tops = Small_int_array.set s.tops s.hand v_max in
            { s with blocks; tops }
        in
        apply_step step t
    | Embed, ([] | [ _ ] | [ Int _; _ ] | [ _; Int _ ] | _ :: _ :: _ :: _) -> fail ()
    | Embed, [ Trans t; Trans p ] ->
        let step (s : State.t) : State.t = { (p.func s) with hand = s.hand } in
        apply_step step t
    | (Move_l | Move_r), ([] | [ _ ] | [ Int _; _ ] | [ _; Trans _ ] | _ :: _ :: _ :: _)
      ->
        fail ()
    | Move_l, [ Trans t; Int x ] ->
        let step (s : State.t) : State.t = { s with hand = max 1 (s.hand - x) } in
        apply_step step t
    | Move_r, [ Trans t; Int x ] ->
        let step (s : State.t) : State.t =
          { s with hand = min (ctx.dim - 2) (s.hand + x) }
        in
        apply_step step t
    | ( Loop,
        ( []
        | [ _ ]
        | [ _; _ ]
        | [ Int _; _; _ ]
        | [ _; Trans _; _ ]
        | [ _; _; Int _ ]
        | _ :: _ :: _ :: _ :: _ ) ) ->
        fail ()
    | Loop, [ Trans t; Int x; Trans p ] ->
        let step (s : State.t) : State.t =
          let rec loop x' s = if x' >= x then s else loop (x' + 1) (p.func s) in
          loop 0 s
        in
        apply_step step t

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

  let distance v v' =
    match (v, v') with
    | Int x, Int x' -> if x = x' then 0. else 1.
    | Trans x, Trans x' ->
        if [%equal: transition] x x' then 0.
        else
          List.map2_exn x.summary x'.summary ~f:(fun s s' ->
              Bitarray.jaccard_distance s.blocks s'.blocks)
          |> List.sum (module Float) ~f:Fun.id
    | _, _ -> 1.

  let target_distance t = function
    | Int _ -> 1.
    | Trans x -> Bitarray.jaccard_distance t (List.hd_exn x.summary).blocks
end

let nop = P.apply Op.Nop
let int x = P.apply (Op.Int x)

(* let serialize (Apply (op, args)) = *)
(*   match (op, args) with *)
(*   | Loop, [ prec; Apply (Int x, []); p ] -> *)
(*       serialize prec @ [ List [ Atom "for"; Atom (Int.to_string x); serialize p ] ] *)
(*   | Embed, [ prec; p ] -> serialize prec @ [ List [ Atom "embed"; serialize p ] ] *)
(*   | Move_l, [ prec; p ] -> serialize prec @ [ List [ Atom "embed"; serialize p ] ] *)

let rec parse_op prec : Sexp.t -> Op.t Program.t = function
  | List [ Atom "for"; Atom i; p ] ->
      Apply (Loop, [ prec; int (Int.of_string i); parse p ])
  | List [ Atom "embed"; p ] -> Apply (Embed, [ prec; parse p ])
  | List [ Atom "l"; Atom i ] -> Apply (Move_l, [ prec; int (Int.of_string i) ])
  | List [ Atom "r"; Atom i ] -> Apply (Move_r, [ prec; int (Int.of_string i) ])
  | Atom "h" -> Apply (Drop_h, [ prec ])
  | Atom "v" -> Apply (Drop_v, [ prec ])
  | s -> raise_s [%message "unexpected operator" (s : Sexp.t)]

and parse = function
  | Atom _ as s -> parse_op nop s
  | List xs -> List.fold_left xs ~init:nop ~f:parse_op
