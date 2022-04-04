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

let backwards_pass target_distance search_state ectx class_ =
  match S.Class.value class_ with
  | Value.Scene _ ->
      let eval = Value.mk_eval_memoized () ectx in
      Iter.forever (fun () ->
          S.local_greedy search_state (Int.ceil_log2 30) eval target_distance class_)
  | _ -> Iter.empty

let test_search search_state programs =
  let ectx = Value.Ctx.create (Scene2d.Dim.create ~xres:16 ~yres:16 ~scaling:2 ()) in

  (* let prog = *)
  (*   Iter.of_list programs *)
  (*   |> Iter.find_map (fun program -> *)
  (*          Program.commutative_closure ~is_commutative:Op.is_commutative program *)
  (*          |> Iter.find_map (fun p -> *)
  (*                 let p = S.find_term search_state p in *)
  (*                 let (Apply ((_, classes), _)) = p in *)

  (*                 if not (List.is_empty classes) then Some p else None)) *)
  (*   |> Option.value_exn *)
  (* in *)

  (* let (Apply ((_, _classes, _), _)) = *)
  (*   Program.map prog ~f:(fun (op, classes) args -> *)
  (*       let relevant_paths = *)
  (*         Iter.of_list classes *)
  (*         |> Iter.map (fun class_ -> *)
  (*                in_paths search_state class_ *)
  (*                |> Iter.filter (fun p -> *)
  (*                       [%compare.equal: Op.t] (Path.op p) op *)
  (*                       && List.for_all2_exn (Path.args p) args *)
  (*                            ~f:(fun c (Apply ((_, arg_classes), _)) -> *)
  (*                              List.mem ~equal:[%compare.equal: Class.t] arg_classes c))) *)
  (*         |> Iter.concat |> Iter.to_list *)
  (*       in *)
  (*       (op, classes, relevant_paths)) *)
  (* in *)
  let target = Program.eval (Value.eval ectx) (List.hd_exn programs) in

  let target_distance v = Value.distance target v in
  let _target_distance_annot v =
    Fmt.pr "%a@." Value.pp v;
    target_distance v
  in

  let sorted_classes =
    S.classes search_state
    |> Iter.filter (fun c -> [%compare.equal: Type.t] Scene (S.Class.type_ c))
    |> Iter.map (fun c -> (target_distance @@ S.Class.value c, c))
    |> Iter.sort ~cmp:(fun (d, _) (d', _) -> [%compare: float] d d')
  in

  sorted_classes |> Iter.take 100
  |> Iter.iter (fun (_, (class_ : S.Class.t)) ->
         let found =
           backwards_pass target_distance search_state ectx class_
           |> Iter.head |> Option.join
         in
         Option.iter found ~f:(fun found ->
             Fmt.pr "%a@." (Program.pp Op.pp) found;
             Fmt.pr "@[%a@]@." Value.pp (Program.eval (Value.eval ectx) found)))

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Print a CAD benchmark."
    [%map_open
      let bench_fn = anon ("BENCH" %: string) and ss_fn = anon ("BIN" %: string) in
      fun () ->
        let bench = Sexp.load_sexps_conv_exn bench_fn parse in
        let ss = In_channel.with_file ss_fn ~f:S.of_channel in
        test_search ss bench]
  |> Command_unix.run
