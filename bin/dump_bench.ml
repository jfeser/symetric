open! Core
open Staged_synth

let pp_iters_fixed =
  let pp fmt n = Format.fprintf fmt "%10Ld" n in
  (pp, 10)

let iters f =
  let pp, width = pp_iters_fixed in
  f ~width pp

let no_total_counter ?message ?pp ?width ?(sampling_interval = 1) () =
  let open Progress in
  let open Segment in
  let box =
    match width with
    | Some width -> box_fixed width
    | None -> box_winsize ~fallback:80
  in
  list
    ((Option.map message ~f:const |> Option.to_list)
    @ (Option.map pp ~f:(fun f -> f of_pp) |> Option.to_list))
  |> box |> periodic sampling_interval |> accumulator Int64.( + ) 0L
  |> make ~init:0L

let bar msg = no_total_counter ~message:msg ~pp:iters ()

let tbar msg total = Progress.counter ~total ~message:msg ~pp:iters ()

let sequence_progress bar =
  Sequence.unfold_with ~init:() ~f:(fun () x ->
      bar 1L;
      Yield (x, ()))

let take_while_with_state ~init ~f s =
  Sequence.unfold_with s ~init ~f:(fun st x ->
      match f st x with Some st' -> Yield (x, st') | None -> Done)

module Make (Lang : Lang_intf.S_with_gen) = struct
  open Lang

  let eval_program params = Program.eval (Value.eval params)

  let has_noop params p =
    let exception Noop in
    let rec eval (Program.Apply (op, args)) =
      let args = List.map ~f:eval args in
      let v = Value.eval params op args in
      if List.mem args v ~equal:[%compare.equal: Value.t] then raise Noop else v
    in
    try
      (eval p : Value.t) |> ignore;
      false
    with Noop -> true

  let irreducible params p =
    let values = Hashtbl.create (module Value) in
    let rec eval (Program.Apply (op, args) as p) =
      let args = List.map ~f:eval args in
      let out = Value.eval params op args in
      Hashtbl.update values out ~f:(fun ps -> p :: Option.value ps ~default:[]);
      out
    in
    (eval p : Value.t) |> ignore;
    not @@ Hashtbl.existsi values ~f:(fun ~key:_ ~data:ps -> List.length ps > 1)

  (** 
@param size program size
@param nprim number of primitives available
@param n number of programs to generate 
*)
  let random ~size ~n ~dir params =
    let ops = Gen.random_ops params in

    let random_program size =
      let open Option.Let_syntax in
      let nilops = List.filter ops ~f:(fun op -> Op.arity op = 0)
      and unops = List.filter ops ~f:(fun op -> Op.arity op = 1)
      and binops = List.filter ops ~f:(fun op -> Op.arity op = 2) in
      let rec random_unop op size =
        let%map p = random_tree (size - 1) in
        Program.Apply (op, [ p ])
      and random_binop op size =
        Combinat.(compositions ~n:(size - 1) ~k:2 |> to_list)
        |> List.permute
        |> List.find_map ~f:(fun ss ->
               let s = ss.(0) and s' = ss.(1) in
               let%bind p = random_tree s and p' = random_tree s' in
               return @@ Program.Apply (op, [ p; p' ]))
      and random_tree size =
        [%test_pred: int] (fun size -> size > 0) size;
        if size = 1 then
          let op = List.random_element_exn nilops in
          Some (Program.Apply (op, []))
        else if size = 2 then random_unop (List.random_element_exn unops) size
        else
          let op = List.random_element_exn (unops @ binops) in
          let arity = Op.arity op in
          if arity = 1 then random_unop op size
          else if arity = 2 then random_binop op size
          else failwith ""
      in
      let prog = Option.value_exn (random_tree size) in
      (prog, ops)
    in

    Progress.(
      with_reporters
        (bar "samples" / bar "irreducible programs"
        / tbar "unique programs" (Int64.of_int n)))
    @@ fun ((samples, irreducible_progs), unique_progs) ->
    Sequence.unfold ~init:() ~f:(fun () -> Some (random_program size, ()))
    |> sequence_progress samples
    |> Sequence.filter ~f:(fun (p, _) ->
           Gen.check params p
           && (not (has_noop params p))
           && irreducible params p)
    |> sequence_progress irreducible_progs
    |> Sequence.map ~f:(fun (prog, ops) ->
           (prog, ops, eval_program params prog))
    |> take_while_with_state
         ~init:(Set.empty (module Value))
         ~f:(fun seen (_, _, out) ->
           if Set.length seen < n then
             if Set.mem seen out then Some seen else Some (Set.add seen out)
           else None)
    |> sequence_progress unique_progs
    |> Sequence.map ~f:(fun (prog, ops, out) ->
           Gen.to_bench params ops prog out)
    |> Sequence.iteri ~f:(fun i b ->
           Bench.save [%string "%{dir}/scene_%{i#Int}.sexp"] b)
end

let random_cli (module Lang : Lang_intf.S_with_gen) =
  let open Command in
  let open Let_syntax in
  basic ~summary:"Random benchmarks"
    [%map_open
      let params = Dumb_params.Spec.cli Lang.Gen.spec
      and seed = flag "seed" (optional_with_default 0 int) ~doc:" random seed"
      and size = flag "size" (required int) ~doc:" program size"
      and dir = flag "dir" (required string) ~doc:" output directory"
      and n = anon ("n" %: int) in
      fun () ->
        Random.init seed;
        Unix.mkdir_p dir;

        let open Make (Lang) in
        random params ~size ~n ~dir]

let () =
  Command.group ~summary:"Dump benchmarks"
    [ ("random", random_cli (module Cad)) ]
  |> Command.run
