open! Core

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
  open L
  open C
  module G = Grammar

  let rec of_list = function
    | [] -> S.unit
    | [ l ] -> l
    | l :: ls -> S.seq l (of_list ls)

  let enumerate max_cost (target : L.value) =
    let nt = G.non_terminals grammar in
    let enum tbl made cost =
      List.filter grammar ~f:(fun rule -> G.rule_size rule <= cost)
      |> List.concat_map ~f:(fun rule ->
             let fresh =
               let x = ref 0 in
               fun () ->
                 incr x;
                 !x
             in
             let holes = ref [] in
             let rec rename = function
               | G.Term.Id v ->
                   if List.mem nt v ~equal:[%compare.equal: string] then (
                     let v' = sprintf "%s%d" v (fresh ()) in
                     holes := (v, v') :: !holes;
                     G.Term.Id v' )
                   else Id v
               | App (f, ts) -> App (f, List.map ts ~f:rename)
             in
             let lhs, rhs = rule in
             let rhs = rename rhs in
             let n_holes = List.length !holes in
             if n_holes = 0 && G.rule_size rule = cost then
               [
                 eval (Map.empty (module String)) rhs
                 |> put ~sym:lhs ~size:cost ~sizes:[] tbl;
               ]
             else
               Combinat.Partition.fold
                 ( cost - G.rule_size rule + List.length !holes,
                   List.length !holes )
                 ~init:[]
                 ~f:(fun code costs ->
                   if for_all costs (Set.mem made) then
                     let loop =
                       let put_all ctx =
                         eval (Map.of_alist_exn (module String) ctx) rhs
                         |> put ~sym:lhs ~size:cost ~sizes:(to_list costs) tbl
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
                     loop [] :: code
                   else code))
    in
    C.empty target (fun tbl ->
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
