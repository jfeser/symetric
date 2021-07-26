open Baseline

let spec = spec

module Make (Lang : Lang_intf.S with type Value.t = Cad_conc.t) = struct
  include Make (Lang)
  open Lang
  open Search_state

  let collect_ball ball_width ops center =
    let queue = Queue.create () in
    (try Tree_ball.Rename_insert_delete.ball (module Op) ops center ball_width (Queue.enqueue queue)
     with Program.Eval_error e -> raise @@ Program.Eval_error [%message (center : Op.t Program.t) (e : Sexp.t)]);
    Queue.to_list queue

  let dump_class class_id center ahead behind =
    let open Owl.Arr in
    let ahead = List.dedup_and_sort ahead ~compare:[%compare: Value.t]
    and behind = List.dedup_and_sort behind ~compare:[%compare: Value.t] in
    let mk_feature v = expand (Cad_conc.to_ndarray v) 2 in
    let feat =
      mk_feature center :: List.map ~f:mk_feature ahead @ List.map ~f:mk_feature behind |> Array.of_list |> concatenate
    and label =
      [| create [| 1 |] 0.0; create [| List.length ahead |] 1.0; create [| List.length behind |] 2.0 |] |> concatenate
    in
    Npy.write feat @@ sprintf "class%d_feat.npy" class_id;
    Npy.write label @@ sprintf "class%d_label.npy" class_id

  let synth params =
    let max_cost = Params.get params max_cost in
    let ss = Search_state.create max_cost in
    let bench = Params.get params Lang.bench in
    let ops = Bench.ops bench and output = Bench.output bench in

    let num_dumps = 50 in

    let rec fill cost =
      if cost > max_cost then ()
      else
        let new_states = generate_states params ss ops cost in
        let new_states = sample_states ss new_states in
        insert_states ss cost new_states;

        if cost = 8 then
          List.take (List.permute new_states) num_dumps
          |> List.iteri ~f:(fun i (v, op, args) ->
                 let ball = collect_ball 2 ops (Search_state.program_of_op_args_exn ss op args) in
                 let ahead, behind =
                   List.filter ball ~f:(fun p' ->
                       not @@ [%compare.equal: Value.t] (Program.eval (Value.eval params) p') v)
                   |> List.filter ~f:(fun p' -> Program.size p' >= cost)
                   |> List.partition_map ~f:(fun p' ->
                          let v' = Program.eval (Value.eval params) p' in
                          if
                            Search_state.cost_of ss v'
                            |> Option.map ~f:(fun c -> c >= cost)
                            |> Option.value ~default:true
                          then First v'
                          else Second v')
                 in

                 dump_class i v ahead behind);

        let solutions =
          List.filter_map new_states ~f:(fun (s, _, _) ->
              if [%compare.equal: Value.t] s output then Some (program_exn ss s) else None)
        in

        Fmt.epr "Finished cost %d\n%!" cost;
        print_stats ss;

        if not (List.is_empty solutions) then (
          List.iter solutions ~f:(fun p -> eprint_s [%message (p : Op.t Program.t)]);
          raise (Done (List.hd_exn solutions)));

        fill (cost + 1)
    in

    try fill 0 with Done p -> eprint_s [%message (p : Op.t Program.t)]
end

let cli (type op) (module Lang : Lang_intf.S with type Value.t = Cad_conc.t and type Op.t = op) =
  let module Synth = Make (Lang) in
  let spec = Dumb_params.Spec.union [ Lang.spec; Params.spec; spec ] in
  let open Command.Let_syntax in
  Command.basic ~summary:(sprintf "Enumerative baseline for %s" Lang.name)
  @@ [%map_open
       let params = Dumb_params.Spec.cli spec in
       Synth_utils.run_synth Synth.synth params]
