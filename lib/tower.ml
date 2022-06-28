open Std
module P = Program

module Type = struct
  type t = Int | Tower [@@deriving compare, equal, hash, sexp, yojson]

  let default = Int
  let output = Tower
  let pp fmt x = Sexp.pp fmt @@ [%sexp_of: t] x
end

module Op = struct
  type t = Loop | Move | Reverse | Drop_v | Drop_h | Embed | Int of int | Nop
  [@@deriving compare, equal, hash, sexp, yojson]

  let default = Int (-1)
  let cost _ = 1

  let ret_type : _ -> Type.t = function
    | Loop | Move | Reverse | Drop_v | Drop_h | Embed | Nop -> Tower
    | Int _ -> Int

  let args_type : _ -> Type.t list = function
    | Reverse | Drop_v | Drop_h -> [ Tower ]
    | Embed -> [ Tower; Tower ]
    | Move -> [ Tower; Int ]
    | Loop -> [ Tower; Int; Tower ]
    | Nop | Int _ -> []

  let arity op = List.length @@ args_type op
  let is_commutative _ = false
  let pp fmt x = Sexp.pp_hum fmt @@ [%sexp_of: t] x
end

module Value = struct
  module Dir = struct
    type t = Left | Right [@@deriving compare, equal, hash, sexp]

    let swap = function Left -> Right | Right -> Left
  end

  module State = struct
    type t = {
      hand : int;
      dir : Dir.t;
      tops : Small_int_array.t;
      blocks : Bitarray.Blocked_matrix.t;
    }
    [@@deriving compare, equal, hash, sexp]
  end

  type transition = { func : State.t -> State.t; [@ignore] summary : State.t list }
  [@@deriving compare, equal, hash, sexp]

  type t = Int of int | Trans of transition [@@deriving compare, equal, hash, sexp]

  let default = Int (-1)

  module Ctx = struct
    type t = { summary_states : State.t list; max : int } [@@deriving sexp]

    let create ?(summary_states = []) () = { summary_states; max = 64 }
  end

  let eval_unmemoized (ctx : Ctx.t) (op : Op.t) args : t =
    let fail () = raise_s [%message "unexpected arguments" (op : Op.t) (args : t list)] in
    let apply_step step t =
      let func s = step @@ t.func s in
      Trans { func; summary = List.map ~f:step t.summary }
    in
    match (op, args) with
    | (Int _ | Nop), _ :: _ -> fail ()
    | Int x, [] -> Int x
    | Nop, [] -> Trans { func = Fun.id; summary = ctx.summary_states }
    | (Reverse | Drop_h | Drop_v), ([] | [ Int _ ] | _ :: _ :: _) -> fail ()
    | Reverse, [ Trans t ] ->
        let step (s : State.t) : State.t = { s with dir = Dir.swap s.dir } in
        apply_step step t
    | Embed, ([] | [ _ ] | [ Int _; _ ] | [ _; Int _ ] | _ :: _ :: _ :: _) -> fail ()
    | Embed, [ Trans t; Trans p ] ->
        let step (s : State.t) : State.t =
          { (p.func s) with hand = s.hand; dir = s.dir }
        in
        apply_step step t
    | Move, ([] | [ _ ] | [ Int _; _ ] | [ _; Trans _ ] | _ :: _ :: _ :: _) -> fail ()
    | Move, [ Trans t; Int x ] ->
        let step (s : State.t) : State.t =
          let s' = t.func s in
          match s'.dir with
          | Right -> { s' with hand = min ctx.max (s'.hand + x); dir = s.dir }
          | Left -> { s' with hand = min ctx.max (s'.hand + x); dir = s.dir }
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
          let rec loop x' s = if x >= x' then s else loop (x' + 1) (p.func s) in
          loop 0 s
        in
        apply_step step t
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
end

let serialize = [%sexp_of: Op.t Program.t]
let parse = [%of_sexp: Op.t Program.t]
