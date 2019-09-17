open! Core
open! Core_bench.Std
open Ppx_stage

(* Grammar:
   expr = 0 | 1 | expr + expr | expr * expr | - expr *)

type expr =
  | Zero
  | One
  | Add of expr * expr
  | Mul of expr * expr
  | Neg of expr
[@@deriving sexp]

let rec eval = function
  | Zero -> 2
  | One -> 3
  | Add (x, y) -> eval x + eval y
  | Mul (x, y) -> eval x * eval y
  | Neg x -> -eval x

let rec enum_simple n =
  if n = 0 then []
  else if n = 1 then [ Zero; One ]
  else
    List.map (enum_simple (n - 1)) ~f:(fun e -> Neg e)
    @ ( Combinat.Partition.to_list (n - 1, 2)
      |> List.concat_map ~f:(fun a ->
             let e_l = enum_simple a.{0} in
             let e_r = enum_simple a.{1} in
             List.concat_map e_l ~f:(fun e1 ->
                 List.concat_map e_r ~f:(fun e2 ->
                     [ Add (e1, e2); Mul (e1, e2) ]))) )

let enum_simple_distinct n =
  let enum n =
    if n = 0 then []
    else if n = 1 then [ Zero; One ]
    else
      List.map (enum_simple (n - 1)) ~f:(fun e -> Neg e)
      @ ( Combinat.Partition.to_list (n - 1, 2)
        |> List.concat_map ~f:(fun a ->
               let e_l = enum_simple a.{0} in
               let e_r = enum_simple a.{1} in
               List.concat_map e_l ~f:(fun e1 ->
                   List.concat_map e_r ~f:(fun e2 ->
                       [ Add (e1, e2); Mul (e1, e2) ]))) )
  in
  let seen = Hash_set.create (module Int) () in
  enum n
  |> List.filter_map ~f:(fun e ->
         let v = eval e in
         if Hash_set.mem seen v then None
         else (
           Hash_set.add seen v;
           Some (v, e) ))

let enum_cache n =
  let tbl = Array.create ~len:30 [] in
  let peak = ref (-1) in
  let rec enum_cache_a n =
    if n <= !peak then tbl.(n)
    else
      let vs = enum_cache_b n in
      peak := n;
      tbl.(n) <- vs;
      vs
  and enum_cache_b n =
    if n = 0 then []
    else if n = 1 then [ Zero; One ]
    else
      List.map (enum_cache_a (n - 1)) ~f:(fun e -> Neg e)
      @ ( Combinat.Partition.to_list (n - 1, 2)
        |> List.concat_map ~f:(fun a ->
               let e_l = enum_cache_a a.{0} in
               let e_r = enum_cache_a a.{1} in
               List.concat_map e_l ~f:(fun e1 ->
                   List.concat_map e_r ~f:(fun e2 ->
                       [ Add (e1, e2); Mul (e1, e2) ]))) )
  in
  enum_cache_a n

let enum_value_cache n =
  let zero = 0 in
  let one = 1 in
  let neg x = -x in
  let add = ( + ) in
  let mul = ( * ) in
  let seen = Hash_set.create (module Int) () in
  let tbl = Array.create ~len:30 [] in
  let peak = ref (-1) in
  let rec enum_cache_a n =
    if n <= !peak then tbl.(n)
    else
      let vs = enum_cache_b n in
      peak := n;
      tbl.(n) <- vs;
      vs
  and enum_cache_b n =
    (* print_s ([%sexp_of: Hash_set.M(Int).t] seen); *)
    if n = 0 then []
    else if n = 1 then
      [ zero; one ]
      |> List.filter ~f:(fun v ->
             if Hash_set.mem seen v then false
             else (
               Hash_set.add seen v;
               true ))
    else
      List.map (enum_cache_a (n - 1)) ~f:neg
      @ ( Combinat.Partition.to_list (n - 1, 2)
        |> List.concat_map ~f:(fun a ->
               let e_l = enum_cache_a a.{0} in
               let e_r = enum_cache_a a.{1} in
               List.concat_map e_l ~f:(fun e1 ->
                   List.concat_map e_r ~f:(fun e2 -> [ add e1 e2; mul e1 e2 ])))
        )
      |> List.filter ~f:(fun v ->
             if Hash_set.mem seen v then false
             else (
               Hash_set.add seen v;
               true ))
  in
  enum_cache_a n

let enum_custom_heap n =
  let heap = Bigarray.(Array1.create int c_layout) 1_000_000 in
  let ptr = ref 1 in
  let zero = 0 in
  let one = 1 in
  let add x y =
    (* printf "Add(%d, %d)\n" x y; *)
    let p = !ptr in
    heap.{p} <- ((x land 0x0000FFFF) lsl 16) land (y land 0x0000FFFF);
    incr ptr;
    p
  in
  let mul x y =
    (* printf "Mul(%d, %d)\n" x y; *)
    let p = !ptr in
    heap.{p} <- ((x land 0x0000FFFF) lsl 16) land (y land 0x0000FFFF);
    incr ptr;
    p
  in
  let neg x =
    (* printf "Neg(%d)\n" x; *)
    let p = !ptr in
    heap.{p} <- x land 0x0000FFFF;
    incr ptr;
    p
  in
  let tbl = Array.create ~len:30 [] in
  let peak = ref (-1) in
  let rec enum_cache_a n =
    if n <= !peak then tbl.(n)
    else
      let vs = enum_cache_b n in
      peak := n;
      tbl.(n) <- vs;
      vs
  and enum_cache_b n =
    if n = 0 then []
    else if n = 1 then [ zero; one ]
    else
      List.map (enum_cache_a (n - 1)) ~f:neg
      @ ( Combinat.Partition.to_list (n - 1, 2)
        |> List.concat_map ~f:(fun a ->
               let e_l = enum_cache_a a.{0} in
               let e_r = enum_cache_a a.{1} in
               List.concat_map e_l ~f:(fun e1 ->
                   List.concat_map e_r ~f:(fun e2 -> [ add e1 e2; mul e1 e2 ])))
        )
  in
  enum_cache_a n

(* |> ignore;
   * for i = 0 to 1_000_000 - 1 do
   *   printf "Heap %d = %d\n" i heap.{i}
   * done *)

let enum_dfs n =
  let n_distinct = ref 0 in
  let rec enum n stack =
    if List.length stack = 1 then incr n_distinct;
    if n <= 0 then ()
    else (
      (match stack with v :: vs -> enum (n - 1) (-v :: vs) | _ -> ());
      ( match stack with
      | v :: v' :: vs ->
          enum (n - 1) ((v + v') :: vs);
          enum (n - 1) ((v * v') :: vs)
      | _ -> () );
      enum (n - 1) (0 :: stack);
      enum (n - 1) (1 :: stack) )
  in
  enum n [];
  !n_distinct

let enum_dfs_distinct n =
  let n_distinct = ref 0 in
  let seen = Hash_set.create (module Int) () in
  let rec enum n stack =
    if List.length stack = 1 then incr n_distinct;
    match stack with
    | [ v ] ->
        if Hash_set.mem seen v then ()
        else (
          Hash_set.add seen v;
          cont n stack )
    | _ -> cont n stack
  and cont n stack =
    if n <= 0 then ()
    else (
      (match stack with v :: vs -> enum (n - 1) (-v :: vs) | _ -> ());
      ( match stack with
      | v :: v' :: vs ->
          enum (n - 1) ((v + v') :: vs);
          enum (n - 1) ((v * v') :: vs)
      | _ -> () );
      enum (n - 1) (0 :: stack);
      enum (n - 1) (1 :: stack) )
  in
  enum n [];
  !n_distinct

(* let () = printf "%d\n" (enum_simple 13 |> List.length)
 * 
 * let () =
 *   print_s (enum_simple_distinct 13 |> [%sexp_of: (int * expr) list]);
 *   printf "%d\n" (enum_simple_distinct 13 |> List.length)
 * 
 * let () = printf "%d\n" (enum_value_cache 13 |> List.length)
 * 
 * let () = printf "%d\n" (enum_dfs 13)
 * 
 * let () = printf "%d\n" (enum_dfs_distinct 13) *)

module Term = struct
  type t = Id of string | App of string * t list

  let rec size = function
    | Id _ -> 1
    | App (_, ts) -> 1 + List.sum (module Int) ~f:size ts
end

module Grammar = struct
  type t = (string * Term.t) list

  open Term

  let rule_size (_, t) = Term.size t

  let num_holes (lhs, rhs) =
    let rec num = function
      | Id v -> if String.(v = lhs) then 1 else 0
      | App (_, ts) -> List.sum (module Int) ~f:num ts
    in
    num rhs

  let non_terminals g =
    List.map g ~f:(fun (x, _) -> x)
    |> List.dedup_and_sort ~compare:[%compare: string]
end

module type CACHE = sig
  open Ppx_stage

  type t

  val empty : (t -> 'a code) -> 'a code

  val put : sym:string -> size:int -> t -> int code -> unit code

  val iter :
    sym:string -> size:int -> f:(int code -> unit code) -> t -> unit code
end

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

module Lift = struct
  include Lift

  let t2 l1 l2 (x, y) =
    let x = l1 x in
    let y = l2 y in
    [%code [%e x], [%e y]]
end

module Simple_cache : CACHE = struct
  type t = int list T.t code

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
end

module Better_cache : CACHE = struct
  type t = S.t array code

  let empty k =
    [%code
      let tbl = Array.init 20 (fun _ -> S.create ()) in
      [%e k (tbl : t)]]

  let put ~sym:_ ~size tbl v =
    let key = Lift.int size in
    [%code Core.Hash_set.add [%e tbl].([%e key]) [%e v]]

  let iter ~sym:_ ~size ~f tbl =
    let key = Lift.int size in
    [%code Core.Hash_set.iter [%e tbl].([%e key]) ~f:(fun v -> [%e f v])]
end

let rec arith_eval ctx = function
  | Term.Id "zero" -> [%code 2]
  | Id "one" -> [%code 3]
  | Id x -> Map.find_exn ctx x
  | App ("neg", [ e1 ]) -> [%code -[%e arith_eval ctx e1]]
  | App ("plus", [ e1; e2 ]) ->
      [%code [%e arith_eval ctx e1] + [%e arith_eval ctx e2]]
  | App ("mul", [ e1; e2 ]) ->
      [%code [%e arith_eval ctx e1] * [%e arith_eval ctx e2]]
  | _ -> assert false

let eval : int code Map.M(String).t -> Term.t -> int code = arith_eval

let rec of_list = function
  | [] -> [%code ()]
  | x :: xs ->
      [%code
        [%e x];
        [%e of_list xs]]

let gen_enum g max_cost =
  let module G = Grammar in
  let module C = Better_cache in
  let nt = G.non_terminals g in
  let enum tbl cost =
    List.filter g ~f:(fun rule -> G.rule_size rule <= cost)
    |> List.concat_map ~f:(fun rule ->
           let fresh =
             let x = ref 0 in
             fun () ->
               incr x;
               !x
           in
           let holes = ref [] in
           let rec rename = function
             | Term.Id v ->
                 if List.mem nt v ~equal:[%compare.equal: string] then (
                   let v' = sprintf "%s%d" v (fresh ()) in
                   holes := (v, v') :: !holes;
                   Term.Id v' )
                 else Id v
             | App (f, ts) -> App (f, List.map ts ~f:rename)
           in
           let lhs, rhs = rule in
           let rhs = rename rhs in
           let n_holes = List.length !holes in
           if n_holes = 0 && G.rule_size rule = cost then
             [
               C.put ~sym:lhs ~size:cost tbl
                 (eval (Map.empty (module String)) rhs);
             ]
           else
             Combinat.Partition.fold
               ( cost - G.rule_size rule + List.length !holes,
                 List.length !holes )
               ~init:[]
               ~f:(fun code costs ->
                 let loop =
                   let put_all ctx =
                     (C.put ~sym:lhs ~size:cost tbl)
                       (eval (Map.of_alist_exn (module String) ctx) rhs)
                   in
                   List.foldi !holes ~init:put_all
                     ~f:(fun i put_all (sym, name) ->
                       let put_all ctx =
                         C.iter ~sym ~size:costs.{i}
                           ~f:(fun v -> put_all ((name, v) :: ctx))
                           tbl
                       in
                       put_all)
                 in
                 loop [] :: code))
    |> of_list
  in
  C.empty (fun tbl -> List.init max_cost ~f:(enum tbl) |> of_list)

let arith =
  let open Term in
  [
    ("A", Id "zero");
    ("A", Id "one");
    ("A", App ("neg", [ Id "A" ]));
    ("A", App ("plus", [ Id "A"; Id "A" ]));
    ("A", App ("mul", [ Id "A"; Id "A" ]));
  ]

let synth = gen_enum arith 14

(* let () = run synth
 * 
 * let () = printf "Num distinct: %d\n" (enum_simple_distinct 13 |> List.length) *)

(* let () = print Format.std_formatter synth *)

let () =
  Bench.(
    make_command
      [
        Test.create ~name:"enum_simple" (fun () -> ignore (enum_simple 13));
        Test.create ~name:"enum_value_cache" (fun () -> ignore (enum_cache 13));
        Test.create ~name:"enum_cache" (fun () -> ignore (enum_cache 13));
        Test.create ~name:"enum_custom_heap" (fun () ->
            ignore (enum_custom_heap 13));
        Test.create ~name:"enum_dfs" (fun () -> ignore (enum_dfs 13));
        Test.create ~name:"enum_staged" (fun () -> ignore (run synth));
      ])
  |> Command.run
