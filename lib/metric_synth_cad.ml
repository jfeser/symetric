open Std
module Lang = Cad_ext
open Lang
module S = Search_state_all.Make (Lang)
module Gen = Generate.Gen_iter (Lang)

(* constants *)
let max_repeat_count = 4
let target_groups_err = 0.1

(* parameters *)
module Params = struct
  type t = {
    size : Scene2d.Dim.t;
    local_search_steps : int;
    group_threshold : float;
    operators : Op.t list;
    max_cost : int;
    backward_pass_repeats : int;
    verbosity : int;
    target_program : Op.t Program.t;
    validate : bool;
    target_groups : int;
    dump_search_space : string option;
    load_search_space : string option;
    output_file : string;
    use_beam_search : bool;
  }
  [@@deriving yojson]

  let create : dim:Scene2d.Dim.t -> _ =
   fun ~dim:size ~group_threshold ~max_cost ~local_search_steps ~backward_pass_repeats
       ~verbosity ~validate ~n_groups ~dump_search_space ~load_search_space ~output_file
       ~use_beam_search target_prog ->
    let operators = Cad_ext.Op.default_operators ~xres:size.xres ~yres:size.yres in

    List.find (Program.ops target_prog) ~f:(fun op ->
        not @@ List.mem ~equal:[%compare.equal: Op.t] operators op)
    |> Option.iter ~f:(fun op ->
           raise_s [%message "program not in search space" (op : Op.t)]);
    {
      validate;
      local_search_steps;
      size;
      operators;
      group_threshold;
      max_cost;
      backward_pass_repeats;
      verbosity;
      target_program = target_prog;
      target_groups = n_groups;
      dump_search_space;
      load_search_space;
      output_file;
      use_beam_search;
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
        flag "-group-threshold"
          (optional_with_default 0.1 float)
          ~doc:"distance threshold to trigger grouping"
      and n_groups =
        flag "-n-groups"
          (optional_with_default 1_000 int)
          ~doc:" number of groups to retain"
      and local_search_steps =
        flag "-local-search-steps"
          (optional_with_default 1_000 int)
          ~doc:" number of steps to run local search"
      and backward_pass_repeats =
        flag "-backward-pass-repeats"
          (optional_with_default 10 int)
          ~doc:" number of times to run backward pass"
      and dim = Scene2d.Dim.param
      and verbosity =
        flag "-verbosity" (optional_with_default 0 int) ~doc:" set verbosity"
      and validate = flag "-validate" no_arg ~doc:" turn on validation"
      and use_beam_search = flag "-use-beam-search" no_arg ~doc:" use beam search"
      and output_file = flag "-out" (required string) ~doc:" output to file" in
      fun () ->
        let target_prog = Lang.parse @@ Sexp.input_sexp In_channel.stdin in
        create ~max_cost ~group_threshold ~local_search_steps ~dim ~backward_pass_repeats
          ~verbosity ~validate ~n_groups ~dump_search_space ~load_search_space
          ~output_file ~use_beam_search target_prog]
end

let params = Set_once.create ()
let[@inline] size () = (Set_once.get_exn params [%here]).Params.size
let[@inline] group_threshold () = (Set_once.get_exn params [%here]).Params.group_threshold
let[@inline] operators () = (Set_once.get_exn params [%here]).Params.operators
let[@inline] max_cost () = (Set_once.get_exn params [%here]).Params.max_cost
let[@inline] verbosity () = (Set_once.get_exn params [%here]).Params.verbosity
let[@inline] target_program () = (Set_once.get_exn params [%here]).Params.target_program
let[@inline] validate () = (Set_once.get_exn params [%here]).Params.validate
let[@inline] target_groups () = (Set_once.get_exn params [%here]).Params.target_groups
let[@inline] output_file () = (Set_once.get_exn params [%here]).Params.output_file
let[@inline] use_beam_search () = (Set_once.get_exn params [%here]).Params.use_beam_search

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
let ectx = lazy (Value.Ctx.create (size ()))
let[@inline] ectx () = Lazy.force ectx
let target = lazy (Program.eval (Value.eval (ectx ())) @@ target_program ())
let[@inline] target () = Lazy.force target

type time_span = Time.Span.t

let yojson_of_time_span t = `Float (Time.Span.to_sec t)

module Stats = struct
  type t = {
    runtime : Timer.t;
    max_cost_generated : int ref;
    groups_searched : int ref;
    space_contains_target : bool option ref;
    grouping_time : time_span Sample.Quantile_estimator.t;
  }
  [@@deriving yojson_of]

  let create () =
    {
      runtime = Timer.create ();
      max_cost_generated = ref 0;
      groups_searched = ref 0;
      space_contains_target = ref None;
      grouping_time =
        Sample.Quantile_estimator.create [%compare: Time.Span.t] Time.Span.zero;
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

let relative_distance (v : Value.t) (v' : Value.t) =
  match (v, v') with
  | Scene x, Scene x' ->
      let target_scene = match target () with Scene t -> t | _ -> assert false in
      let n = Scene2d.(pixels @@ sub target_scene x)
      and n' = Scene2d.(pixels @@ sub target_scene x') in
      let p = Scene2d.(pixels @@ sub x target_scene)
      and p' = Scene2d.(pixels @@ sub x' target_scene) in
      let union = Bitarray.(hamming_weight (or_ n n') + hamming_weight (or_ p p'))
      and inter = Bitarray.(hamming_weight (and_ n n') + hamming_weight (and_ p p')) in
      assert (union >= inter && inter >= 0);
      if union = 0 then 0.0 else 1.0 -. (Float.of_int inter /. Float.of_int union)
  | v, v' -> if [%compare.equal: Value.t] v v' then 0.0 else Float.infinity

let distance (v : Value.t) (v' : Value.t) =
  match (v, v') with
  (* | Scene x, Scene x' when not Scene2d.(corner_overlap x x') -> Float.infinity *)
  | v, v' -> Value.distance v v'

let target_distance v = distance (target ()) v

let local_search p =
  let target = target ()
  and size = size ()
  and steps = local_search_steps ()
  and ectx = ectx () in
  let value_eval = Value.mk_eval_memoized () in
  Local_search.of_unnormalize_tabu ~target ~dist:distance
    (module Op)
    (module Value)
    (function
      | Apply (Int x, []) ->
          if x <= 0 then [ Apply (Int (x + 1), []) ]
          else if x >= max size.Scene2d.Dim.xres size.yres then
            [ Apply (Int (x - 1), []) ]
          else [ Apply (Int (x + 1), []); Apply (Int (x - 1), []) ]
      | Apply (Rep_count x, []) ->
          if x <= 1 then [ Apply (Rep_count (x + 1), []) ]
          else if x >= max_repeat_count then [ Apply (Rep_count (x - 1), []) ]
          else [ Apply (Rep_count (x + 1), []); Apply (Rep_count (x - 1), []) ]
      | Apply (Circle, [ Apply (Int x, []); Apply (Int y, []); Apply (Int r, []) ]) ->
          [
            Apply
              ( Rect,
                [
                  Apply (Int (x - r), []);
                  Apply (Int (y - r), []);
                  Apply (Int (x + r), []);
                  Apply (Int (y + r), []);
                ] );
          ]
      | Apply
          ( Rect,
            [
              Apply (Int lx, []);
              Apply (Int ly, []);
              Apply (Int hx, []);
              Apply (Int hy, []);
            ] )
        when hx - lx = hy - ly ->
          let r = (hx - lx) / 2 in
          [
            Apply
              ( Circle,
                [ Apply (Int (lx + r), []); Apply (Int (ly + r), []); Apply (Int r, []) ]
              );
          ]
      | _ -> [])
    (Program.eval (value_eval ectx))
    p
  |> Iter.map (fun p -> (target_distance (Program.eval (value_eval ectx) p), p))
  |> Iter.take steps
  |> Iter.mapi (fun i (d, x) -> (d, i, x))
  |> Iter.min_floor ~to_float:(fun (d, _, _) -> d) 0.0
  |> Option.map ~f:(fun (_, i, p) -> (i, p))
  |> Option.value ~default:(-1, p)

module Edge = struct
  type t = Value.t * (Op.t[@compare.ignore]) * (S.Class.t list[@compare.ignore])
  [@@deriving compare, hash, sexp]

  let value (v, _, _) = v
  let score value = -1. *. target_distance value
  let distance (v, _, _) (v', _, _) = relative_distance v v'
end

let insert_states (all_edges : Edge.t Iter.t) =
  let target_groups = target_groups () in
  let search_state = get_search_state () in

  let module Edges = struct
    type t = Value.t * (Edge.t list[@compare.ignore]) [@@deriving compare, hash, sexp]

    let distance (v, _) (v', _) = relative_distance v v'
  end in
  let groups, _group_time =
    Synth_utils.timed (fun () ->
        all_edges
        |> Iter.ordered_groupby (module Value) ~score:Edge.score ~key:(fun (v, _, _) -> v)
        |> Iter.map (fun (v, (_, es)) -> (v, es))
        |> Grouping.create_m
             (module Edges)
             (group_threshold ()) Edges.distance target_groups)
  in
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

let fill_search_space () =
  let ectx = ectx ()
  and search_state = get_search_state ()
  and ops = operators ()
  and max_cost = max_cost () in

  Log.log 1 (fun m -> m "Goal:\n%a" Value.pp (target ()));

  for cost = 1 to max_cost do
    Log.log 1 (fun m -> m "Start generating states of cost %d" cost);

    let states_iter () =
      Gen.generate_states S.search_iter S.Class.value ectx search_state ops cost
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

let backwards_pass class_ =
  let search_state = get_search_state () and max_cost = max_cost () and ectx = ectx () in
  match S.Class.value class_ with
  | Value.Scene _ ->
      let eval = Value.mk_eval_memoized () ectx in
      Iter.forever (fun () ->
          S.local_greedy search_state (Int.ceil_log2 max_cost) eval target_distance class_
          |> Option.map ~f:local_search)
  | _ -> Iter.empty

let check_for_target_program () =
  let p = S.find_term (get_search_state ()) (target_program ()) in
  Log.sexp 1 (lazy [%message (p : (Op.t * _ list) Program.t)]);
  stats.space_contains_target :=
    Some (Iter.for_all (fun (_, xs) -> List.length xs > 0) (Program.iter p))

let synthesize () =
  Timer.start stats.runtime;

  let target = target ()
  and ectx = ectx ()
  and backward_pass_repeats = backward_pass_repeats () in

  (match load_search_space () with
  | Some fn -> In_channel.with_file fn ~f:(fun ch -> search_state := S.of_channel ch)
  | None ->
      fill_search_space ();
      Option.iter (dump_search_space ()) ~f:(fun fn ->
          Out_channel.with_file fn ~f:(fun ch -> S.to_channel ch @@ get_search_state ())));

  check_for_target_program ();
  write_output None;
  let search_state = get_search_state () in

  if validate () then
    S.validate search_state (Value.eval ectx) distance (group_threshold ());

  let exception Done of Op.t Program.t in
  Log.log 1 (fun m -> m "Starting backwards pass");

  let ret =
    try
      S.classes search_state
      |> Iter.filter (fun c -> [%compare.equal: Type.t] Scene (S.Class.type_ c))
      |> Iter.map (fun c -> (target_distance @@ S.Class.value c, c))
      |> Iter.sort ~cmp:(fun (d, _) (d', _) -> [%compare: float] d d')
      |> Iter.iteri (fun i (d, (class_ : S.Class.t)) ->
             incr stats.groups_searched;
             Log.log 1 (fun m -> m "Searching candidate %d (d=%f)" i d);
             Log.log 2 (fun m -> m "@.%a" Value.pp (S.Class.value class_));

             backwards_pass class_
             |> Iter.take backward_pass_repeats
             |> Iter.filter_map Fun.id
             |> Iter.mapi (fun backwards_pass_i (local_search_i, p) ->
                    ((backwards_pass_i, local_search_i), p))
             |> Iter.min_floor
                  ~to_float:(fun (_, p) ->
                    target_distance @@ Program.eval (Value.eval ectx) p)
                  0.0
             |> Option.iter ~f:(fun ((backwards_pass_iters, local_search_iters), p) ->
                    let found_value = Program.eval (Value.eval ectx) p in

                    Log.log 2 (fun m ->
                        m "Best (d=%f):@.%a" (target_distance found_value) Value.pp
                          found_value);
                    Log.sexp 2 (lazy [%message (p : Op.t Program.t)]);

                    if [%compare.equal: Value.t] target found_value then (
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
