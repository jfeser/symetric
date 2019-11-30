open! Core

type expr = Zero | One | Add of expr * expr | Mul of expr * expr | Neg of expr
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
  let seen = Hash_set.create (module Int) in
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
  let seen = Hash_set.create (module Int) in
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
  let seen = Hash_set.create (module Int) in
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
