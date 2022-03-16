open Core
open Staged_synth
open Std
open Cad_ext
module S = Metric_synth_cad.S
open S

let path_stability ss dist path =
  let op = Path.op path and args = Path.args path in
  Path.args path
  |> List.mapi ~f:(fun i c ->
         let choices = Iter.to_list @@ in_paths ss c in
         let other_args = List.filteri args ~f:(fun j _ -> j <> i) in
         let other_choices = List.map other_args ~f:(in_paths ss) |> Iter.list_product in
         let ranks =
           other_choices
           |> Iter.map (fun other_paths ->
                  List.map choices ~f:(fun p ->
                      let args = List.map ~f:Path.value @@ List.insert other_paths i p in
                      dist op args)
                  |> List.rank ~compare:[%compare: float])
         in
         rank_stability (List.length choices) ranks)

let main ss programs =
  let ctx = Value.Ctx.create (Scene2d.Dim.create ~xres:16 ~yres:16 ~scaling:2 ()) in
  let prog =
    Iter.of_list programs
    |> Iter.find_map (fun program ->
           Program.commutative_closure ~is_commutative:Op.is_commutative program
           |> Iter.find_map (fun p ->
                  let p = S.find_term ss p in
                  let (Apply ((_, classes), _)) = p in

                  if not (List.is_empty classes) then Some p else None))
    |> Option.value_exn
  in

  let prog =
    Program.map prog ~f:(fun (op, classes) args ->
        let relevant_paths =
          Iter.of_list classes
          |> Iter.map (fun class_ ->
                 in_paths ss class_
                 |> Iter.filter (fun p ->
                        [%compare.equal: Op.t] (Path.op p) op
                        && List.for_all2_exn (Path.args p) args
                             ~f:(fun c (Apply ((_, arg_classes), _)) ->
                               List.mem ~equal:[%compare.equal: Class.t] arg_classes c)))
          |> Iter.concat |> Iter.to_list
        in
        (op, classes, relevant_paths))
  in

  let mean_stability =
    Program.iter prog
    |> Iter.filter (fun ((_, _, paths), _) ->
           List.exists paths ~f:(fun p -> List.length (Path.args p) > 0))
    |> Iter.filter_map (fun (((_, _, paths) as lhs), args) ->
           let goal =
             Program.eval
               (fun (op, _, _) args -> Value.eval ctx op args)
               (Apply (lhs, args))
           in
           let dist op args = Value.distance (Value.eval ctx op args) goal in

           Fmt.pr "Goal:@,%a@." Value.pp goal;
           Fmt.pr "Path choices:@,";
           List.iter paths ~f:(fun p -> Fmt.pr "%a@," Value.pp @@ Path.value p);
           Fmt.pr "@.";

           Iter.of_list paths
           |> Iter.map (fun p -> List.filter_map ~f:Result.ok @@ path_stability ss dist p)
           |> Iter.filter (fun s -> not (List.is_empty s))
           |> Iter.filter_map (fun s -> Iter.mean @@ Iter.of_list s)
           |> Iter.mean)
    |> Iter.mean
  in
  print_s [%message (mean_stability : float option)]

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:""
    [%map_open
      let ss_fn = anon ("BIN" %: string) in
      fun () ->
        let ss = In_channel.with_file ss_fn ~f:S.of_channel in
        main ss bench]
  |> Command.run
