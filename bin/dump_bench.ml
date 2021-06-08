open! Core
open Staged_synth
open Cad_op
open Cad_bench

let rec union_many = function
  | [] -> failwith "unexpected empty list"
  | [ x ] -> x
  | x :: xs -> Program.Apply (Union, [ x; union_many xs ])

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

let to_sexp ~xmax ~ymax (prog, ops) =
  let input = { xmax; ymax } in
  let bench =
    Cad_bench.
      {
        ops = [];
        input;
        output = Cad_conc.dummy;
        solution = None;
        filename = None;
      }
  in
  let params = Dumb_params.(of_alist_exn [ P (Cad_params.bench, bench) ]) in
  let conc = Program.eval (Cad_conc.eval params) prog in

  let output = pixels input conc |> List.map ~f:(fun x -> if x then 1 else 0) in

  [%sexp_of: Cad_bench.Serial.t]
    Cad_bench.Serial.{ ops; input; output; solution = Some prog }

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
@param xmax canvas x length
@param ymax canvas y length
@param size program size
@param nprim number of primitives available
@param n number of programs to generate 
@param k callback with program sequence *)
  let random ~size ~n ~ops params k =
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
           (not (has_noop params p)) && irreducible params p)
    |> sequence_progress irreducible_progs
    |> take_while_with_state
         ~init:(Set.empty (module Value))
         ~f:(fun seen (prog, _) ->
           if Set.length seen < n then
             let out = eval_program params prog in
             if Set.mem seen out then Some seen else Some (Set.add seen out)
           else None)
    |> sequence_progress unique_progs
    |> Sequence.map ~f:(to_sexp ~xmax ~ymax)
    |> Sequence.mapi ~f:(fun i sexp -> ([%string "scene_%{i#Int}.sexp"], sexp))
    |> k
end

let dumps ~dir seq =
  Unix.mkdir_p dir;
  Sequence.iter seq ~f:(fun (fn, sexp) ->
      Out_channel.with_file (sprintf "%s/%s" dir fn) ~f:(fun ch ->
          Sexp.output_hum ch sexp))

let random_cli =
  let open Command in
  let open Let_syntax in
  let op =
    Arg_type.of_map
    @@ Map.of_alist_exn (module String)
    @@ [
         ("circle", `Circle);
         ("rectangle", `Rect);
         ("union", `Union);
         ("intersection", `Inter);
         ("replicate", `Repl);
       ]
  in
  basic ~summary:"Random benchmarks"
    [%map_open
      let xmax =
        flag "xmax" (optional_with_default 30 int) ~doc:" x dimension size"
      and ymax =
        flag "ymax" (optional_with_default 30 int) ~doc:" y dimension size"
      and seed = flag "seed" (optional_with_default 0 int) ~doc:" random seed"
      and size = flag "size" (required int) ~doc:" program size"
      and ops = flag "op" (listed op) ~doc:" allowed operator"
      and dir = flag "dir" (required string) ~doc:" output directory"
      and n = anon ("n" %: int) in
      fun () ->
        Random.init seed;
        random ~xmax ~ymax ~size ~ops ~n @@ dumps ~dir]

let () =
  Command.group ~summary:"Dump benchmarks" [ ("random", random_cli) ]
  |> Command.run
