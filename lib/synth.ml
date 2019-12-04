open! Core
module Fresh = Utils.Fresh
open Utils.Collections

let for_all a f =
  let rec loop i =
    if i >= Bigarray.Array1.dim a then true else f a.{i} && loop (i + 1)
  in
  loop 0

let to_list a = List.init (Bigarray.Array1.dim a) ~f:(fun i -> a.{i})

module Make
    (S : Sigs.CODE)
    (L : Sigs.LANG with type 'a code = 'a S.t and type type_ = S.ctype)
    (C : Sigs.CACHE with type value = L.value and type 'a code = 'a S.t) =
struct
  open C
  module G = Grammar

  let debug = false

  let debug_print msg = if debug then S.print msg else S.unit

  let rec of_list = function
    | [] -> S.unit
    | [ l ] -> l
    | l :: ls -> S.seq l (of_list ls)

  let rec case pred default = function
    | [] -> default
    | (v, k) :: bs -> S.ite (pred v) k (case pred default bs)

  let rec seq_many = function [] -> S.unit | x :: xs -> S.seq x (seq_many xs)

  let rec let_many f = function
    | [] -> f []
    | [ x ] -> S.let_ x (fun x -> f [ x ])
    | x :: xs -> S.let_ x (fun x -> let_many (fun xs -> f (x :: xs)) xs)

  let costs_t = S.Array.mk_type Int

  let rec reconstruct tbl sym costs inputs output =
    let fresh = Fresh.create () in
    let args_t = S.Tuple.mk_type costs_t (L.type_of output) in
    let func_t = S.Func (args_t, Unit) in
    let func_name = sprintf "reconstruct_%s" sym in
    Log.debug (fun m ->
        m "Building %s :: %s." func_name
          ([%sexp_of: S.ctype] func_t |> Sexp.to_string));
    let body costs target =
      G.rhs L.grammar sym
      |> List.map ~f:(G.with_holes ~fresh L.grammar)
      |> List.group_by (module Int) (fun (_, hs) -> List.length hs)
      |> List.map ~f:(fun (n_holes, rhss) ->
             let case = S.int n_holes in
             let costs = List.init n_holes ~f:(fun i -> S.(costs.(int i))) in
             let code : unit S.t =
               let_many
                 (fun costs ->
                   List.map rhss ~f:(fun (term, holes) ->
                       let hole_costs = List.zip_exn costs holes in
                       let check ctx =
                         let ectx =
                           Map.map ctx ~f:(fun (v, _) -> v)
                           |> Map.merge_exn inputs
                         in
                         let v = L.eval ectx term in
                         S.ite (L.eq v target)
                           (seq_many
                              ( S.print
                                  ( [%sexp_of: G.Term.t] term
                                  |> Sexp.to_string_hum )
                              :: List.map hole_costs ~f:(fun (_, (sym, name)) ->
                                     let target, costs =
                                       Map.find_exn ctx name
                                     in
                                     reconstruct tbl sym costs inputs target) ))
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
                   |> of_list)
                 costs
             in
             (case, code))
      |> case (fun size -> S.(Array.length costs = size)) S.unit
    in
    let func =
      S.func func_name func_t (fun tup ->
          S.let_ (S.Tuple.fst tup) (fun costs ->
              S.let_ (S.Tuple.snd tup) (fun target -> body costs target)))
    in
    S.apply func (S.Tuple.create costs (L.code output))

  let put_all tbl ctx inputs output cost costs lhs rhs =
    let open S in
    let v =
      L.eval (Map.of_alist_exn (module String) ctx |> Map.merge_exn inputs) rhs
    in
    let costs =
      Array.const costs_t (costs |> List.map ~f:int |> List.to_array)
    in
    let insert_code =
      seq_many
        [
          debug_print
            (sprintf "Inserting (%s -> %s) cost %d" lhs (G.Term.to_string rhs)
               cost);
          put ~sym:lhs ~size:cost ~sizes:costs tbl v;
        ]
    in
    match L.(v = output) with
    | Some eq ->
        let recon_code =
          seq_many
            [
              print "Starting reconstruction";
              reconstruct tbl lhs costs inputs output;
              exit;
            ]
        in
        ite eq recon_code insert_code
    | None -> insert_code

  let put_leaf_rule tbl inputs output cost (lhs, rhs) =
    [ put_all tbl [] inputs output cost [] lhs rhs ]

  let put_rule tbl inputs output cost holes made ((lhs, rhs) as rule) =
    Combinat.Partition.fold
      (cost - G.rule_size rule + List.length holes, List.length holes)
      ~init:[]
      ~f:(fun code costs ->
        if for_all costs (Set.mem made) then (
          Log.debug (fun m ->
              m "Enumerating (%s -> %s) at cost %d" lhs (G.Term.to_string rhs)
                cost);
          let loop =
            let put_all ctx =
              put_all tbl ctx inputs output cost (to_list costs) lhs rhs
            in
            List.foldi holes ~init:put_all ~f:(fun i put_all (sym, name) ->
                let put_all ctx =
                  C.iter ~sym
                    ~size:(S.int costs.{i})
                    ~f:(fun (v, _) -> put_all ((name, v) :: ctx))
                    tbl
                in
                put_all)
          in
          loop [] :: code )
        else (
          Log.debug (fun m ->
              m "Ignoring (%s -> %s) at cost %d" lhs (G.Term.to_string rhs) cost);
          code ))

  let enumerate_rule tbl made cost inputs output ((lhs, rhs) as rule) =
    let fresh = Fresh.create () in
    let rhs, holes = G.with_holes ~fresh L.grammar rhs in
    let n_holes = List.length holes in
    if n_holes = 0 && G.rule_size rule = cost then (
      Log.debug (fun m ->
          m "Enumerating (%s -> %s) at cost %d" lhs (G.Term.to_string rhs) cost);
      put_leaf_rule tbl inputs output cost rule )
    else put_rule tbl inputs output cost holes made (lhs, rhs)

  let enumerate_cost tbl made cost inputs output =
    List.filter L.grammar ~f:(fun rule -> G.rule_size rule <= cost)
    |> List.concat_map ~f:(enumerate_rule tbl made cost inputs output)

  let enumerate max_cost inputs (output : L.value) =
    C.empty (fun tbl ->
        let _, loops =
          List.init max_cost ~f:(fun c -> c)
          |> List.fold_left
               ~init:(Set.empty (module Int), [])
               ~f:(fun (made, loops) cost ->
                 let loops' =
                   enumerate_cost tbl made cost inputs output
                   @ [ S.print (sprintf "Completed %d/%d" cost max_cost) ]
                 in
                 if List.length loops' > 0 then
                   (Set.add made cost, loops @ loops')
                 else (made, loops))
        in
        of_list loops)
end
