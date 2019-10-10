open! Core
open! Utils
open Utils.Collections

let for_all a f =
  let rec loop i =
    if i >= Bigarray.Array1.dim a then true else f a.{i} && loop (i + 1)
  in
  loop 0

let to_list a = List.init (Bigarray.Array1.dim a) ~f:(fun i -> a.{i})

module Make
    (S : Sigs.CODE)
    (L : Sigs.LANG with type 'a code = 'a S.t)
    (C : Sigs.CACHE with type value = L.value and type 'a code = 'a S.t) =
struct
  open C
  module G = Grammar

  let rec of_list = function
    | [] -> S.unit
    | [ l ] -> l
    | l :: ls -> S.seq l (of_list ls)

  let rec case pred default = function
    | [] -> default
    | (v, k) :: bs -> S.ite (pred v) k (case pred default bs)

  let rec seq_many = function [] -> S.unit | x :: xs -> S.seq x (seq_many xs)

  let rec reconstruct tbl sym (costs' : int32 array code) target =
    let fresh = Fresh.create () in
    let func =
      S.func
        (sprintf "reconstruct_%s" sym)
        (Func (S.Array.mk_type Int, Unit))
        (fun (costs : int32 array code) ->
          G.rhs L.grammar sym
          |> List.map ~f:(G.with_holes ~fresh L.grammar)
          |> List.group_by (module Int) (fun (_, hs) -> List.length hs)
          |> List.map ~f:(fun (n_holes, rhss) ->
                 let case = S.int n_holes in
                 let costs =
                   List.init n_holes ~f:(fun i -> S.(costs.(int i)))
                 in
                 let code : unit S.t =
                   List.map rhss ~f:(fun (term, holes) ->
                       let hole_costs = List.zip_exn costs holes in
                       let check ctx =
                         let ectx = Map.map ctx ~f:(fun (v, _) -> v) in
                         let v = L.eval ectx term in
                         S.ite
                           L.(v = target)
                           (seq_many
                              ( S.print
                                  ( [%sexp_of: G.Term.t] term
                                  |> Sexp.to_string_hum )
                              :: List.map hole_costs
                                   ~f:(fun (_, (sym, name)) ->
                                     let target, costs =
                                       Map.find_exn ctx name
                                     in
                                     reconstruct tbl sym costs target) ))
                           S.unit
                       in
                       let check =
                         List.fold_left hole_costs ~init:check
                           ~f:(fun check (cost, (sym, name)) ->
                             let check ctx =
                               C.iter ~sym ~size:cost
                                 ~f:(fun v ->
                                   check (Map.add_exn ctx ~key:name ~data:v))
                                 tbl
                             in
                             check)
                       in
                       check (Map.empty (module String)))
                   |> of_list
                 in
                 (case, code))
          |> case (fun size -> S.(Array.length costs = size)) S.unit)
    in
    S.apply func costs'

  let enumerate max_cost (target : L.value) =
    let int_array = S.(Array.mk_type Int) in
    let enum tbl made cost =
      List.filter L.grammar ~f:(fun rule -> G.rule_size rule <= cost)
      |> List.concat_map ~f:(fun rule ->
             let fresh = Fresh.create () in
             let lhs, rhs = rule in
             let rhs, holes = G.with_holes ~fresh L.grammar rhs in
             let n_holes = List.length holes in
             if n_holes = 0 && G.rule_size rule = cost then
               [
                 L.eval (Map.empty (module String)) rhs
                 |> put ~sym:lhs ~size:cost
                      ~sizes:(S.Array.const int_array [||])
                      tbl;
               ]
             else
               Combinat.Partition.fold
                 ( cost - G.rule_size rule + List.length holes,
                   List.length holes )
                 ~init:[]
                 ~f:(fun code costs ->
                   if for_all costs (Set.mem made) then
                     let loop =
                       let put_all ctx =
                         let v =
                           L.eval (Map.of_alist_exn (module String) ctx) rhs
                         in
                         let sizes =
                           S.Array.const int_array
                             ( to_list costs |> List.map ~f:S.int
                             |> List.to_array )
                         in
                         S.ite
                           L.(v = target)
                           (reconstruct tbl lhs sizes v)
                           (put ~sym:lhs ~size:cost ~sizes tbl v)
                       in
                       List.foldi holes ~init:put_all
                         ~f:(fun i put_all (sym, name) ->
                           let put_all ctx =
                             C.iter ~sym
                               ~size:(S.int costs.{i})
                               ~f:(fun (v, _) -> put_all ((name, v) :: ctx))
                               tbl
                           in
                           put_all)
                     in
                     loop [] :: code
                   else code))
    in
    C.empty (fun tbl ->
        let _, loops =
          List.init max_cost ~f:(fun c -> c)
          |> List.fold_left
               ~init:(Set.empty (module Int), [])
               ~f:(fun (made, loops) cost ->
                 let loops' = enum tbl made cost in
                 if List.length loops' > 0 then
                   (Set.add made cost, loops @ loops')
                 else (made, loops))
        in
        of_list loops)
end
