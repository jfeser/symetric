open Std
module Lang = Regex
open Lang
module S = Search_state_all.Make (Lang)
module Gen = Generate.Gen_iter (Lang)

let max_int = 15

(* parameters *)
module Params = struct
  type t = {
    sketch : string;
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
    extract : [ `Greedy | `Random | `Exhaustive ];
    repair : [ `Guided | `Random ]; (* if true, do not use distance to guide repair *)
    output_file : string;
    exhaustive_width : int;
  }
  [@@deriving yojson]

  let create ~group_threshold ~max_cost ~local_search_steps ~backward_pass_repeats
      ~verbosity ~validate ~n_groups ~dump_search_space ~load_search_space
      ~use_beam_search ~use_ranking ~extract ~repair ~output_file ~sketch
      ~exhaustive_width =
    {
      sketch;
      validate;
      local_search_steps;
      operators = Regex.Op.default_operators max_int;
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
      exhaustive_width;
    }

  let cmd =
    let open Command.Let_syntax in
    [%map_open
      let max_cost =
        flag "-max-cost" (required int) ~doc:" the maximum size of program to evaluate"
      and dump_search_space =
        flag "-dump-search-space" (optional string)
          ~doc:" dump the search space to a file"
      and sketch = flag "-sketch" (required string) ~doc:" regex sketch"
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
          (optional_with_default "exhaustive" string)
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
          | "exhaustive" -> `Exhaustive
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
          ~use_beam_search ~use_ranking ~extract ~repair ~output_file ~sketch
          ~exhaustive_width:16]
end

let params = Set_once.create ()
let[@inline] group_threshold () = (Set_once.get_exn params [%here]).Params.group_threshold
let[@inline] operators () = (Set_once.get_exn params [%here]).Params.operators
let[@inline] max_cost () = (Set_once.get_exn params [%here]).Params.max_cost
let max_height () = 1 + Int.ceil_log2 (max_cost ())
let[@inline] verbosity () = (Set_once.get_exn params [%here]).Params.verbosity
let[@inline] validate () = (Set_once.get_exn params [%here]).Params.validate
let[@inline] target_groups () = (Set_once.get_exn params [%here]).Params.target_groups
let[@inline] use_beam_search () = (Set_once.get_exn params [%here]).Params.use_beam_search
let[@inline] extract () = (Set_once.get_exn params [%here]).Params.extract
let[@inline] repair () = (Set_once.get_exn params [%here]).Params.repair
let[@inline] use_ranking () = (Set_once.get_exn params [%here]).Params.use_ranking
let[@inline] output_file () = (Set_once.get_exn params [%here]).Params.output_file
let[@inline] sketch () = (Set_once.get_exn params [%here]).Params.sketch

let[@inline] dump_search_space () =
  (Set_once.get_exn params [%here]).Params.dump_search_space

let[@inline] load_search_space () =
  (Set_once.get_exn params [%here]).Params.load_search_space

let[@inline] backward_pass_repeats () =
  (Set_once.get_exn params [%here]).Params.backward_pass_repeats

let[@inline] local_search_steps () =
  (Set_once.get_exn params [%here]).Params.local_search_steps

let[@inline] exhaustive_width () =
  (Set_once.get_exn params [%here]).Params.exhaustive_width

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
let bench = lazy (Regex_bench.load_sketch_bench (sketch ()) In_channel.stdin)
let[@inline] ectx () = Tuple.T2.get1 @@ Lazy.force bench
let[@inline] operators () = (Tuple.T2.get2 @@ Lazy.force bench) @ operators ()

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

let distance = Value.distance
let relative_distance = Value.distance
let target_distance v = Value.target_distance (ectx ()) v

let rewrite : Op.t P.t -> Op.t P.t list = function
  | Apply (Int x, []) ->
      if x <= 1 then [ Apply (Int (x + 1), []) ]
      else if x >= max_int then [ Apply (Int (x - 1), []) ]
      else [ Apply (Int (x + 1), []); Apply (Int (x - 1), []) ]
  | Apply (Repeat, [ r; n ]) -> [ Apply (Repeat_range, [ r; n; n ]) ]
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
  Synth_utils.timed (`Add stats.repair_time) (fun () -> local_search_untimed p)

module Edge = struct
  type t = Value.t * (Op.t[@compare.ignore]) * (S.Class.t list[@compare.ignore])
  [@@deriving compare, hash, sexp]

  let value (v, _, _) = v
  let score value = -1. *. target_distance value
  let distance (v, _, _) (v', _, _) = relative_distance v v'
end

exception Done of Op.t Program.t

let select_top_k_edges edges =
  Iter.ordered_groupby (module Value) ~score:Edge.score ~key:(fun (v, _, _) -> v) edges
  |> Iter.timed stats.rank_time
  |> Iter.map (fun (v, (d, es)) ->
         if Float.(d = 0.) then
           match es with
           | (_, op, args) :: _ ->
               List.map args
                 ~f:
                   (S.local_greedy (get_search_state ()) (max_height ())
                      (Value.eval (ectx ()))
                      target_distance)
               |> Option.all
               |> Option.iter ~f:(fun args -> raise (Done (Apply (op, args))));
               failwith "early exit extract failed"
               (* ; *)
               (* (v, es) *)
           | _ -> (v, es)
         else (v, es))

let select_arbitrary edges = Iter.map (fun ((v, _, _) as edge) -> (v, [ edge ])) edges

let select_edges edges =
  if use_ranking () then select_top_k_edges edges else select_arbitrary edges

let insert_states cost (all_edges : Edge.t Iter.t) =
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
      let type_ = Op.ret_type op in
      let class_ = S.Class.create type_ cost group_center in
      (* insert new representative (some may already exist) *)
      if not @@ S.mem_class search_state class_ then
        S.insert_class search_state type_ cost group_center op args;
      List.iter members ~f:(fun (_, edges) ->
          S.insert_class_members search_state class_ edges))

let insert_states_beam cost all_edges =
  let search_state = get_search_state () in

  all_edges
  |> Iter.filter (fun (value, op, _) ->
         let type_ = Op.ret_type op in
         let class_ = S.Class.create type_ cost value in
         not (S.mem_class search_state class_))
  |> Iter.top_k_distinct
       (module Value)
       ~score:Edge.score ~key:Edge.value (target_groups ())
  |> Iter.iter (fun (value, op, args) ->
         let type_ = Op.ret_type op in
         let class_ = S.Class.create type_ cost value in
         if not (S.mem_class search_state class_) then
           S.insert_class search_state type_ cost value op args)

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

    let run_time = ref Time.Span.zero in
    Synth_utils.timed (`Set run_time) (fun () ->
        if use_beam_search () then insert_states_beam cost (states_iter ())
        else insert_states cost (states_iter ()));

    incr stats.max_cost_generated;
    write_output None;
    Log.log 1 (fun m ->
        m "Finish generating states of cost %d (runtime=%a)" cost Time.Span.pp !run_time)
  done

let fill_search_space () =
  Synth_utils.timed (`Set stats.xfta_time) fill_search_space_untimed

let run_extract_untimed eval class_ =
  let search_state = get_search_state () in
  match extract () with
  | `Greedy -> S.local_greedy search_state (max_height ()) eval target_distance class_
  | `Random -> S.random search_state (max_height ()) class_
  | `Exhaustive ->
      S.exhaustive ~width:(exhaustive_width ()) search_state (max_height ()) eval
        target_distance class_

let run_extract eval class_ =
  Synth_utils.timed (`Add stats.extract_time) (fun () -> run_extract_untimed eval class_)

let backwards_pass class_ =
  let ectx = ectx () in
  assert ([%equal: Type.t] (S.Class.type_ class_) Type.output);
  let eval = Value.mk_eval_memoized () ectx in
  Iter.forever (fun () -> run_extract eval class_ |> Option.map ~f:local_search)

let synthesize () =
  eprint_s [%message (ectx () : Value.Ctx.t)];

  Timer.start stats.runtime;

  let ectx = ectx () and backward_pass_repeats = backward_pass_repeats () in

  try
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

    Log.log 1 (fun m -> m "Starting backwards pass");

    let classes =
      S.classes search_state
      |> Iter.filter (fun c -> [%compare.equal: Type.t] Type.output (S.Class.type_ c))
      |> Iter.map (fun c -> (target_distance @@ S.Class.value c, c))
      |> Iter.sort ~cmp:(fun (d, _) (d', _) -> [%compare: float] d d')
    in

    Iter.iteri
      (fun i (d, (class_ : S.Class.t)) ->
        incr stats.groups_searched;
        Log.log 1 (fun m -> m "Searching candidate %d (d=%f)" i d);
        Log.log 2 (fun m -> m "@.%a" Value.pp (S.Class.value class_));

        backwards_pass class_
        |> Iter.take backward_pass_repeats
        |> Iter.filter_map Fun.id
        |> Iter.mapi (fun backwards_pass_i (local_search_i, p) ->
               ((backwards_pass_i, local_search_i), p))
        |> Iter.min_floor
             ~to_float:(fun (_, p) -> target_distance @@ Program.eval (Value.eval ectx) p)
             0.0
        |> Option.iter ~f:(fun ((backwards_pass_iters, local_search_iters), p) ->
               let found_value = Program.eval (Value.eval ectx) p in

               Log.log 2 (fun m ->
                   m "Best (d=%f):@.%a" (target_distance found_value) Value.pp found_value);
               Log.sexp 2 (lazy ([%sexp_of: Op.t Program.t] p));

               if Float.(target_distance found_value = 0.) then (
                 Log.log 0 (fun m -> m "local search iters %d" local_search_iters);
                 Log.log 0 (fun m -> m "backwards pass iters %d" backwards_pass_iters);
                 raise (Done p))))
      classes;
    write_output None
  with Done p ->
    Timer.stop stats.runtime;
    write_output (Some p)

let cmd =
  let open Command.Let_syntax in
  Command.basic ~summary:"Solve CAD problems with metric synthesis."
    [%map_open
      let mk_params = Params.cmd in
      fun () ->
        Set_once.set_exn params [%here] @@ mk_params ();
        synthesize ()]
