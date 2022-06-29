open Std
module Lang = Tower
open Lang
module S = Search_state_all.Make (Lang)
module Gen = Generate.Gen_iter (Lang)

let operators =
  [ Op.Loop; Drop_v; Drop_h; Move_l; Move_r; Embed; Seq ]
  @ (Iter.int_range ~start:1 ~stop:8 |> Iter.map (fun i -> Op.Int i) |> Iter.to_list)

(* parameters *)
module Params = struct
  type t = {
    local_search_steps : int;
    group_threshold : float;
    operators : Op.t list;
    max_cost : int;
    backward_pass_repeats : int;
    verbosity : int;
    validate : bool;
    target_groups : int;
    dump_search_space : string option;
    load_search_space : string option;
    use_beam_search : bool; (* if true, then disable clustering *)
    use_ranking : bool; (* if false, disable ranking clustered states *)
    extract : [ `Greedy | `Random ];
    repair : [ `Guided | `Random ]; (* if true, do not use distance to guide repair *)
    output_file : string;
  }
  [@@deriving yojson]

  let create ~group_threshold ~max_cost ~local_search_steps ~backward_pass_repeats
      ~verbosity ~validate ~n_groups ~dump_search_space ~load_search_space
      ~use_beam_search ~use_ranking ~extract ~repair ~output_file =
    {
      validate;
      local_search_steps;
      operators;
      group_threshold;
      max_cost;
      backward_pass_repeats;
      verbosity;
      target_groups = n_groups;
      dump_search_space;
      load_search_space;
      use_beam_search;
      use_ranking;
      extract;
      repair;
      output_file;
    }

  let cmd =
    let open Command.Let_syntax in
    [%map_open
      let max_cost =
        flag "-max-cost" (required int) ~doc:" the maximum size of program to evaluate"
      and dump_search_space =
        flag "-dump-search-space" (optional string)
          ~doc:" dump the search space to a file"
      and load_search_space =
        flag "-load-search-space" (optional string)
          ~doc:" load the search space from a file"
      and group_threshold =
        flag "-group-threshold" (required float)
          ~doc:"distance threshold to trigger grouping"
      and n_groups = flag "-n-groups" (required int) ~doc:" number of groups to retain"
      and local_search_steps =
        flag "-local-search-steps" (required int)
          ~doc:" number of steps to run local search"
      and backward_pass_repeats =
        flag "-backward-pass-repeats" (required int)
          ~doc:" number of times to run backward pass"
      and verbosity =
        flag "-verbosity" (optional_with_default 0 int) ~doc:" set verbosity"
      and validate = flag "-validate" no_arg ~doc:" turn on validation"
      and use_beam_search = flag "-use-beam-search" no_arg ~doc:" use beam search"
      and use_ranking =
        flag "-use-ranking"
          (optional_with_default true bool)
          ~doc:" ranking during XFTA construction"
      and extract =
        flag "-extract"
          (optional_with_default "greedy" string)
          ~doc:" method of program extraction"
      and repair =
        flag "-repair"
          (optional_with_default "guided" string)
          ~doc:" method of program repair"
      and output_file = flag "-out" (required string) ~doc:" output file" in
      fun () ->
        let extract =
          match extract with
          | "greedy" -> `Greedy
          | "random" -> `Random
          | _ -> raise_s [%message "unexpected extraction method" (extract : string)]
        in
        let repair =
          match repair with
          | "guided" -> `Guided
          | "random" -> `Random
          | _ -> raise_s [%message "unexpected extraction method" (repair : string)]
        in
        create ~max_cost ~group_threshold ~local_search_steps ~backward_pass_repeats
          ~verbosity ~validate ~n_groups ~dump_search_space ~load_search_space
          ~use_beam_search ~use_ranking ~extract ~repair ~output_file]
end

let params = Set_once.create ()
let[@inline] group_threshold () = (Set_once.get_exn params [%here]).Params.group_threshold
let[@inline] operators () = (Set_once.get_exn params [%here]).Params.operators
let[@inline] max_cost () = (Set_once.get_exn params [%here]).Params.max_cost
let[@inline] verbosity () = (Set_once.get_exn params [%here]).Params.verbosity
let[@inline] validate () = (Set_once.get_exn params [%here]).Params.validate
let[@inline] target_groups () = (Set_once.get_exn params [%here]).Params.target_groups
let[@inline] use_beam_search () = (Set_once.get_exn params [%here]).Params.use_beam_search
let[@inline] extract () = (Set_once.get_exn params [%here]).Params.extract
let[@inline] repair () = (Set_once.get_exn params [%here]).Params.repair
let[@inline] use_ranking () = (Set_once.get_exn params [%here]).Params.use_ranking
let[@inline] output_file () = (Set_once.get_exn params [%here]).Params.output_file

let[@inline] dump_search_space () =
  (Set_once.get_exn params [%here]).Params.dump_search_space

let[@inline] load_search_space () =
  (Set_once.get_exn params [%here]).Params.load_search_space

let[@inline] backward_pass_repeats () =
  (Set_once.get_exn params [%here]).Params.backward_pass_repeats

let[@inline] local_search_steps () =
  (Set_once.get_exn params [%here]).Params.local_search_steps

module Log = struct
  let start_time = Time.now ()

  let log level msgf =
    let with_time fmt =
      Format.fprintf Format.err_formatter
        ("[%a] @[" ^^ fmt ^^ "@]@.%!")
        Time.Span.pp
        (Time.diff (Time.now ()) start_time)
    in
    if level <= verbosity () then msgf with_time

  let sexp level lsexp = if level <= verbosity () then eprint_s @@ Lazy.force lsexp
end

let search_state = ref (S.create ())
let[@inline] get_search_state () = !search_state
let bench = lazy (Sexp.input_sexp In_channel.stdin |> Tower.parse)
let[@inline] ectx () = Value.Ctx.create ()

type time_span = Time.Span.t

let yojson_of_time_span t = `Float (Time.Span.to_sec t)

module Stats = struct
  type t = {
    runtime : Timer.t;
    max_cost_generated : int ref;
    groups_searched : int ref;
    cluster_time : time_span ref;
    rank_time : time_span ref;
    expansion_time : time_span ref;
    repair_time : time_span ref;
    xfta_time : time_span ref;
    extract_time : time_span ref;
  }
  [@@deriving yojson_of]

  let create () =
    {
      runtime = Timer.create ();
      max_cost_generated = ref 0;
      groups_searched = ref 0;
      cluster_time = ref Time.Span.zero;
      rank_time = ref Time.Span.zero;
      expansion_time = ref Time.Span.zero;
      repair_time = ref Time.Span.zero;
      xfta_time = ref Time.Span.zero;
      extract_time = ref Time.Span.zero;
    }
end

let stats = Stats.create ()

let write_output m_prog =
  let program_size =
    Option.map m_prog ~f:(fun p -> Float.of_int @@ Program.size p)
    |> Option.value ~default:Float.nan
  in
  let program_json =
    Option.map m_prog ~f:(fun p -> `String (Sexp.to_string @@ Lang.serialize p))
    |> Option.value ~default:`Null
  in
  let open Yojson in
  let json =
    `Assoc
      [
        ("method", `String "metric");
        ("program", program_json);
        ("program_size", `Float program_size);
        ("stats", [%yojson_of: Stats.t] stats);
        ("params", [%yojson_of: Params.t] @@ Set_once.get_exn params [%here]);
      ]
  in
  Out_channel.with_file (output_file ()) ~f:(fun ch -> Safe.to_channel ch json)

let distance v v' = Value.distance (ectx ()) v v'
let relative_distance v v' = Value.distance (ectx ()) v v'
let target_value = lazy (P.eval (Value.eval (ectx ())) (Lazy.force bench))

let target =
  lazy
    (match Lazy.force target_value with
    | Trans x -> (List.hd_exn x.summary).blocks
    | Int _ -> assert false)

let target_distance v = Value.target_distance (ectx ()) (Lazy.force target) v

let rewrite : Op.t P.t -> Op.t P.t list = function
  | Apply (Int x, []) ->
      if x < 1 then [ Apply (Int (x + 1), []) ]
      else if x > 8 then [ Apply (Int (x - 1), []) ]
      else [ Apply (Int (x + 1), []); Apply (Int (x - 1), []) ]
  | Apply (Embed, [ p ]) -> [ p ]
  | Apply (Drop_v, [ p ]) as d ->
      [
        Apply (Drop_h, [ p ]);
        Apply (Embed, [ Apply (Seq, [ Apply (Move_l, [ Apply (Int 1, []) ]); d ]) ]);
        Apply (Embed, [ Apply (Seq, [ Apply (Move_r, [ Apply (Int 1, []) ]); d ]) ]);
      ]
  | Apply (Drop_h, [ p ]) as d ->
      [
        Apply (Drop_v, [ p ]);
        Apply (Embed, [ Apply (Seq, [ Apply (Move_l, [ Apply (Int 1, []) ]); d ]) ]);
        Apply (Embed, [ Apply (Seq, [ Apply (Move_r, [ Apply (Int 1, []) ]); d ]) ]);
      ]
  | Apply
      (Seq, [ (Apply ((Move_l | Move_r), _) as m); (Apply ((Drop_v | Drop_h), []) as d) ])
    ->
      [ Apply (Seq, [ d; m ]) ]
  | _ -> []

let local_search_untimed p =
  let steps = local_search_steps () and ectx = ectx () in
  let value_eval = Value.mk_eval_memoized () in
  Local_search.of_unnormalize_tabu ~target_distance
    ~random:(match repair () with `Guided -> false | `Random -> true)
    (module Op)
    (module Value)
    rewrite
    (Program.eval (value_eval ectx))
    p
  |> Iter.map (fun p -> (target_distance (Program.eval (value_eval ectx) p), p))
  |> Iter.take steps
  |> Iter.mapi (fun i (d, x) -> (d, i, x))
  |> Iter.min_floor ~to_float:(fun (d, _, _) -> d) 0.0
  |> Option.map ~f:(fun (_, i, p) -> (i, p))
  |> Option.value ~default:(-1, p)

let local_search p =
  let ret, time = Synth_utils.timed (fun () -> local_search_untimed p) in
  (stats.repair_time := Time.Span.(time + !(stats.repair_time)));
  ret

module Edge = struct
  type t = Value.t * (Op.t[@compare.ignore]) * (S.Class.t list[@compare.ignore])
  [@@deriving compare, hash, sexp]

  let value (v, _, _) = v
  let score value = -1. *. target_distance value
  let distance (v, _, _) (v', _, _) = relative_distance v v'
end

let select_top_k_edges edges =
  Iter.ordered_groupby (module Value) ~score:Edge.score ~key:(fun (v, _, _) -> v) edges
  |> Iter.timed stats.rank_time
  |> Iter.map (fun (v, (_, es)) -> (v, es))

let select_arbitrary edges = Iter.map (fun ((v, _, _) as edge) -> (v, [ edge ])) edges

let select_edges edges =
  if use_ranking () then select_top_k_edges edges else select_arbitrary edges

let insert_states (all_edges : Edge.t Iter.t) =
  let target_groups = target_groups () in
  let search_state = get_search_state () in

  let module Edges = struct
    type t = Value.t * (Edge.t list[@compare.ignore]) [@@deriving compare, hash, sexp]

    let distance (v, _) (v', _) = relative_distance v v'
  end in
  let groups =
    all_edges |> select_edges
    |> Grouping.create_m (module Edges) (group_threshold ()) Edges.distance target_groups
  in
  (stats.cluster_time := Time.Span.(!(stats.cluster_time) + groups.runtime));

  Log.log 1 (fun m ->
      m "Generated %d groups from %d edges"
        (Hashtbl.length groups.groups)
        groups.n_samples);

  Hashtbl.iteri groups.groups ~f:(fun ~key:(group_center, center_edges) ~data:members ->
      let op, args =
        match center_edges with (_, op, args) :: _ -> (op, args) | _ -> assert false
      in
      let class_ = S.Class.create group_center (Op.ret_type op) in
      (* insert new representative (some may already exist) *)
      if not @@ S.mem_class search_state class_ then
        S.insert_class search_state group_center op @@ List.map ~f:S.Class.value args;
      List.iter members ~f:(fun (_, edges) ->
          S.insert_class_members search_state class_ edges))

let insert_states_beam all_edges =
  let search_state = get_search_state () in

  all_edges
  |> Iter.filter (fun (value, op, _) ->
         let class_ = S.Class.create value (Op.ret_type op) in
         not (S.mem_class search_state class_))
  |> Iter.top_k_distinct
       (module Value)
       ~score:Edge.score ~key:Edge.value (target_groups ())
  |> Iter.iter (fun (value, op, args) ->
         let class_ = S.Class.create value (Op.ret_type op) in
         if not (S.mem_class search_state class_) then
           S.insert_class search_state value op @@ List.map ~f:S.Class.value args)

let fill_search_space_untimed () =
  let ectx = ectx ()
  and search_state = get_search_state ()
  and ops = operators ()
  and max_cost = max_cost () in

  for cost = 1 to max_cost do
    Log.log 1 (fun m -> m "Start generating states of cost %d" cost);

    let states_iter () =
      Gen.generate_states S.search_iter S.Class.value ectx search_state ops cost
      |> Iter.timed stats.expansion_time
    in

    let (), run_time =
      Synth_utils.timed (fun () ->
          if use_beam_search () then insert_states_beam (states_iter ())
          else insert_states (states_iter ()))
    in

    incr stats.max_cost_generated;
    write_output None;
    Log.log 1 (fun m ->
        m "Finish generating states of cost %d (runtime=%a)" cost Time.Span.pp run_time)
  done

let fill_search_space () =
  let (), xfta_time = Synth_utils.timed fill_search_space_untimed in
  stats.xfta_time := xfta_time

let run_extract_untimed eval height class_ =
  let search_state = get_search_state () in
  match extract () with
  | `Greedy -> S.local_greedy search_state height eval target_distance class_
  | `Random -> S.random search_state height class_

let run_extract eval height class_ =
  let ret, extract_time =
    Synth_utils.timed (fun () -> run_extract_untimed eval height class_)
  in
  (stats.extract_time := Time.Span.(!(stats.extract_time) + extract_time));
  ret

let backwards_pass class_ =
  let max_cost = max_cost () and ectx = ectx () in
  let height = max_cost in
  assert ([%equal: Type.t] (S.Class.type_ class_) Type.output);
  let eval = Value.mk_eval_memoized () ectx in
  Iter.forever (fun () -> run_extract eval height class_ |> Option.map ~f:local_search)

let synthesize () =
  print_s [%message (Lazy.force bench : Op.t P.t)];

  Fmt.epr "Synthesizing:@,%a%!\n" (Value.pp (ectx ())) (Lazy.force target);
  Timer.start stats.runtime;

  let ectx = ectx () and backward_pass_repeats = backward_pass_repeats () in

  (match load_search_space () with
  | Some fn -> In_channel.with_file fn ~f:(fun ch -> search_state := S.of_channel ch)
  | None ->
      fill_search_space ();
      Option.iter (dump_search_space ()) ~f:(fun fn ->
          Out_channel.with_file fn ~f:(fun ch -> S.to_channel ch @@ get_search_state ())));

  write_output None;
  let search_state = get_search_state () in

  if validate () then
    S.validate search_state (Value.eval ectx) distance (group_threshold ());

  let exception Done of Op.t Program.t in
  Log.log 1 (fun m -> m "Starting backwards pass");

  let valid_classes =
    S.classes search_state
    |> Iter.filter (fun c -> [%equal: Type.t] Type.output (S.Class.type_ c))
    |> Iter.map (fun c -> (target_distance @@ S.Class.value c, c))
  in
  let classes_ =
    match extract () with
    | `Greedy ->
        valid_classes |> Iter.sort ~cmp:(fun (d, _) (d', _) -> [%compare: float] d d')
    | `Random -> valid_classes |> Iter.to_list |> List.permute |> Iter.of_list
  in
  let ret =
    try
      classes_
      |> Iter.iteri (fun i (d, (class_ : S.Class.t)) ->
             incr stats.groups_searched;
             Log.log 1 (fun m -> m "Searching candidate %d (d=%f)" i d);
             (match S.Class.value class_ with
             | Trans x ->
                 Log.log 2 (fun m ->
                     m "@.%a" (Value.pp ectx) (List.hd_exn x.summary).blocks)
             | _ -> ());

             backwards_pass class_
             |> Iter.take backward_pass_repeats
             |> Iter.filter_map Fun.id
             |> Iter.mapi (fun backwards_pass_i (local_search_i, p) ->
                    ((backwards_pass_i, local_search_i), p))
             |> Iter.map (fun (stats, p) ->
                    let v = Program.eval (Value.eval ectx) p in
                    let d = target_distance v in
                    (stats, d, v, p))
             |> Iter.min_floor ~to_float:(fun (_, d, _, _) -> d) 0.0
             |> Option.iter
                  ~f:(fun
                       ((backwards_pass_iters, local_search_iters), dist, found_value, p)
                     ->
                    (match found_value with
                    | Value.Trans x ->
                        Log.log 2 (fun m ->
                            m "Best (d=%f):@.%a" dist (Value.pp ectx)
                              (List.hd_exn x.summary).blocks)
                    | _ -> ());

                    Log.sexp 2 (lazy ([%sexp_of: Op.t Program.t] p));

                    if Float.(dist = 0.) then (
                      Log.log 0 (fun m -> m "local search iters %d" local_search_iters);
                      Log.log 0 (fun m ->
                          m "backwards pass iters %d" backwards_pass_iters);
                      raise (Done p))));
      None
    with Done p -> Some p
  in
  Timer.stop stats.runtime;
  write_output ret

let cmd =
  let open Command.Let_syntax in
  Command.basic ~summary:"Solve CAD problems with metric synthesis."
    [%map_open
      let mk_params = Params.cmd in
      fun () ->
        Set_once.set_exn params [%here] @@ mk_params ();
        synthesize ()]
