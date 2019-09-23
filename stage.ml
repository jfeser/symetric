open! Core
open Ppx_stage

module Lift = struct
  include Lift

  let t2 l1 l2 (x, y) =
    let x = l1 x in
    let y = l2 y in
    [%code [%e x], [%e y]]
end

let rec of_list = function
  | [] -> [%code ()]
  | x :: xs ->
      [%code
        [%e x];
        [%e of_list xs]]

[%%code
module T = struct
  open Core

  include Hashtbl.Make (struct
    type t = string * int [@@deriving compare, hash, sexp]
  end)
end
[@code]]

[%%code
module S = struct
  open Core
  include Hash_set.Make (Int)
end
[@code]]

module type INT_CACHE = Sigs.CACHE with type value = int

module Simple_cache : INT_CACHE = struct
  type t = int list T.t code

  type value = int

  let empty k =
    [%code
      let tbl = T.create () in
      [%e k (tbl : t)]]

  let put ~sym ~size tbl v =
    let key = Lift.(t2 string int) (sym, size) in
    [%code T.add_multi ~key:[%e key] ~data:[%e v] [%e tbl]]

  let iter ~sym ~size ~f tbl =
    let key = Lift.(t2 string int) (sym, size) in
    [%code
      if T.mem [%e tbl] [%e key] then
        T.find_exn [%e tbl] [%e key] |> List.iter (fun v -> [%e f v])]

  let print_size tbl =
    [%code print_endline (string_of_int (T.length [%e tbl]))]
end

module Better_cache : INT_CACHE = struct
  type t = S.t array code

  type value = int

  let empty k =
    [%code
      let tbl = Array.init 100 (fun _ -> S.create ()) in
      [%e k (tbl : t)]]

  let put ~sym:_ ~size tbl v =
    let key = Lift.int size in
    [%code Core.Hash_set.add [%e tbl].([%e key]) [%e v]]

  let iter ~sym:_ ~size ~f tbl =
    let key = Lift.int size in
    [%code Core.Hash_set.iter [%e tbl].([%e key]) ~f:(fun v -> [%e f v])]

  let print_size tbl =
    [%code print_endline (string_of_int (Array.length [%e tbl]))]
end

module Best_cache : INT_CACHE = struct
  type t = S.t code * int list ref array code

  type value = int

  let empty k =
    [%code
      let s = S.create () in
      let a = Array.init 100 (fun _ -> ref []) in
      [%e k (s, a)]]

  let put ~sym:_ ~size (s, a) v =
    let key = Lift.int size in
    [%code
      let s = [%e s] in
      let a = [%e a] in
      let v = [%e v] in
      let key = [%e key] in
      if not (Core.Hash_set.mem s v) then (
        Core.Hash_set.add s v;
        a.(key) := v :: !(a.(key)) )]

  let iter ~sym:_ ~size ~f (_, a) =
    let key = Lift.int size in
    [%code List.iter (fun v -> [%e f v]) !([%e a].([%e key]))]

  let print_size (s, _) =
    [%code print_endline (string_of_int (Core.Hash_set.length [%e s]))]
end

module Arith : Sigs.LANG with type value = int = struct
  type value = int

  open Grammar.Term

  let grammar =
    [
      ("A", Id "zero");
      ("A", Id "one");
      ("A", App ("plus", [ Id "A"; Id "A" ]));
      ("A", App ("mul", [ Id "A"; Id "A" ]));
    ]

  let rec eval ctx = function
    | Grammar.Term.Id "zero" -> [%code 0]
    | Id "one" -> [%code 1]
    | Id x -> Map.find_exn ctx x
    | App ("neg", [ e1 ]) -> [%code -[%e eval ctx e1]]
    | App ("plus", [ e1; e2 ]) -> [%code [%e eval ctx e1] + [%e eval ctx e2]]
    | App ("mul", [ e1; e2 ]) -> [%code [%e eval ctx e1] * [%e eval ctx e2]]
    | _ -> assert false
end

[%%code
module S_L = struct
  open Core
  include Hash_set.Make (Values.Int_list)
end
[@code]]

module Better_cache_L = struct
  type t = S_L.t array code * S_L.t array code

  type value = Values.Int_list.t

  let empty k =
    [%code
      let tbl_l = Array.init 100 (fun _ -> S_L.create ()) in
      let tbl_a = Array.init 100 (fun _ -> S_L.create ()) in
      [%e k (tbl_l, tbl_a)]]

  let put ~sym ~size (tbl_l, tbl_a) v =
    let key = Lift.int size in
    if sym = "L" then [%code Core.Hash_set.add [%e tbl_l].([%e key]) [%e v]]
    else if sym = "A" then
      [%code Core.Hash_set.add [%e tbl_a].([%e key]) [%e v]]
    else assert false

  let iter ~sym ~size ~f (tbl_l, tbl_a) =
    let key = Lift.int size in
    if sym = "L" then
      [%code Core.Hash_set.iter [%e tbl_l].([%e key]) ~f:(fun v -> [%e f v])]
    else if sym = "A" then
      [%code Core.Hash_set.iter [%e tbl_a].([%e key]) ~f:(fun v -> [%e f v])]
    else assert false

  let print_size _tbl = [%code ()]
end

module Arith_list : Sigs.LANG with type value = Values.Int_list.t = struct
  type value = Values.Int_list.t

  open Grammar.Term

  let grammar =
    [
      ("L", Id "nil");
      ("L", App ("cons", [ Id "A"; Id "L" ]));
      ("A", Id "zero");
      ("A", Id "one");
      ("A", App ("plus", [ Id "A"; Id "A" ]));
      ("A", App ("mul", [ Id "A"; Id "A" ]));
    ]

  let rec eval ctx = function
    | Grammar.Term.Id "zero" -> [%code Values.Int_list.A 0]
    | Id "one" -> [%code Values.Int_list.A 1]
    | Id "nil" -> [%code Values.Int_list.L []]
    | Id x -> Map.find_exn ctx x
    | App ("cons", [ e; e' ]) ->
        [%code
          let (Values.Int_list.A x) = [%e eval ctx e] in
          let (Values.Int_list.L xs) = [%e eval ctx e'] in
          Values.Int_list.L (x :: xs)]
    | App ("plus", [ e; e' ]) ->
        [%code
          let (Values.Int_list.A x) = [%e eval ctx e] in
          let (Values.Int_list.A x') = [%e eval ctx e'] in
          Values.Int_list.A (x + x')]
    | App ("mul", [ e; e' ]) ->
        [%code
          let (Values.Int_list.A x) = [%e eval ctx e] in
          let (Values.Int_list.A x') = [%e eval ctx e'] in
          Values.Int_list.A (x * x')]
    | _ -> assert false
end
