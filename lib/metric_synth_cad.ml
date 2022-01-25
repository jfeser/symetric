open Std
module Lang = Cad_ext
open Lang
module S = Search_state_all.Make (Lang)
module Gen = Generate.Gen_iter (Lang)

module Params = struct
  type t = {
    size : Scene2d.Dim.t;
    ectx : Value.Ctx.t;
    local_search_steps : int;
    target : Value.t;
    group_threshold : float;
    operators : Op.t list;
    max_cost : int;
    backward_pass_repeats : int;
    verbosity : int;
    search_state : S.t;
    target_program : Op.t Program.t;
    validate : bool;
    target_groups : int;
  }
end

let params = Set_once.create ()
let[@inline] size () = (Set_once.get_exn params [%here]).Params.size
let[@inline] ectx () = (Set_once.get_exn params [%here]).Params.ectx
let[@inline] target () = (Set_once.get_exn params [%here]).Params.target
let[@inline] search_state () = (Set_once.get_exn params [%here]).Params.search_state
let[@inline] group_threshold () = (Set_once.get_exn params [%here]).Params.group_threshold
let[@inline] operators () = (Set_once.get_exn params [%here]).Params.operators
let[@inline] max_cost () = (Set_once.get_exn params [%here]).Params.max_cost
let[@inline] verbosity () = (Set_once.get_exn params [%here]).Params.verbosity
let[@inline] target_program () = (Set_once.get_exn params [%here]).Params.target_program
let[@inline] validate () = (Set_once.get_exn params [%here]).Params.validate
let[@inline] target_groups () = (Set_once.get_exn params [%here]).Params.target_groups
let max_repeat_count = 4
let target_groups_err = 0.5

module Log = struct
  let start_time = Time.now ()

  let log level msgf =
    let with_time fmt =
      Format.fprintf Format.err_formatter
        ("[%a] @[" ^^ fmt ^^ "@]@.%!")
        Time.Span.pp
        (Time.diff (Time.now ()) start_time)
    in
    if level >= verbosity () then msgf with_time

  let sexp level lsexp = if level >= verbosity () then eprint_s @@ Lazy.force lsexp
end

let[@inline] backward_pass_repeats () =
  (Set_once.get_exn params [%here]).Params.backward_pass_repeats

let[@inline] local_search_steps () =
  (Set_once.get_exn params [%here]).Params.local_search_steps

let runtime = ref Time.Span.zero
let distance = Value.distance
let target_distance v = distance (target ()) v

let local_search p =
  let target = target ()
  and size = size ()
  and steps = local_search_steps ()
  and ectx = ectx () in
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
    (Program.eval (Value.eval_memoized ectx))
    p
  |> Iter.map (fun p -> (target_distance (Program.eval (Value.eval_memoized ectx) p), p))
  |> Iter.take steps
  |> Iter.min_floor ~to_float:(fun (d, _) -> d) 0.0
  |> Option.map ~f:(fun (_, p) -> p)
  |> Option.value ~default:p

type group_result = { groups : S.Class.t list Hashtbl.M(S.Class).t; n_queries : int }

module NList = Non_empty_list

(** return a mapping from representatives to the members of their groups *)
let group_states_vp states : group_result =
  let thresh = group_threshold () and search_state = search_state () in

  let distance c c' =
    let v = S.Class.value c and v' = S.Class.value c' in
    distance v v'
  in
  let create_vp = Vpt.create distance `Random in

  let reference_vp = ref (create_vp @@ (S.classes search_state |> Iter.to_list))
  and reference = ref []
  and groups = Hashtbl.create (module S.Class) in
  let n_queries = ref 0 in

  let find_close c f =
    incr n_queries;
    if List.length !reference > 100 then (
      reference_vp := create_vp ((Iter.to_list @@ Vpt.iter !reference_vp) @ !reference);
      reference := []);
    (Iter.append
       (Vpt.neighbors distance c thresh !reference_vp)
       (Iter.filter (fun c' -> distance c c' <=. thresh) (Iter.of_list !reference)))
      f
  in

  List.iter states ~f:(fun v ->
      let is_empty =
        find_close v
        |> Iter.iter_is_empty (fun c' ->
               Hashtbl.update groups c' ~f:(function None -> [ v ] | Some vs -> v :: vs))
      in
      if is_empty then (
        Hashtbl.add_exn groups ~key:v ~data:[];
        reference := v :: !reference));

  { groups; n_queries = !n_queries }

let prev_group_sample = ref (-1)

let insert_states edges =
  let target_groups = target_groups () in
  let target_groups_min =
    Float.(to_int @@ (of_int target_groups *. (1.0 -. target_groups_err)))
  and target_groups_max =
    Float.(to_int @@ (of_int target_groups *. (1.0 +. target_groups_err)))
  in

  let search_state = search_state () in

  (* group edges that produce the same state *)
  let n_states = ref 0 in
  let distinct_edges = Hashtbl.create (module S.Class) in
  edges (fun ((value, op, _) as edge) ->
      let class_ = S.Class.create value (Op.ret_type op) in
      incr n_states;
      Hashtbl.update distinct_edges class_ ~f:(function
        | None -> NList.singleton edge
        | Some edges -> NList.cons edge edges));

  Log.sexp 0
    (lazy
      (let min, max, mean =
         Iter.of_hashtbl distinct_edges
         |> Iter.map (fun (c, _) -> target_distance @@ S.Class.value c)
         |> Iter.stats
       in
       [%message "all states" (min : float) (max : float) (mean : float)]));

  Log.log 1 (fun m ->
      m "(%d, %d distinct) states." !n_states (Hashtbl.length distinct_edges));

  let distinct_states =
    Hashtbl.keys distinct_edges
    |> List.map ~f:(fun c -> (target_distance @@ S.Class.value c, c))
    |> List.sort ~compare:[%compare: float * _]
    |> List.map ~f:Tuple.T2.get2
  in

  let rec adaptive_group prev_ngroups ct =
    let n_states = List.length distinct_states in
    let sample = List.permute @@ List.take ~n:ct distinct_states in
    let group_result = group_states_vp sample in
    let groups = group_result.groups in
    let n_groups = Hashtbl.length groups in

    Log.sexp 0 (lazy [%message (ct : int) (n_groups : int)]);

    if n_groups > target_groups_max then adaptive_group 0 (ct / 2)
    else if ct >= n_states || n_groups >= target_groups_min then
      (groups, List.length sample)
    else if prev_ngroups > 0 then
      let t_n = target_groups - n_groups and n_n0 = n_groups - prev_ngroups in
      let incr = Float.(to_int (of_int target_groups *. (of_int t_n /. of_int n_n0))) in
      adaptive_group n_groups (ct + incr)
    else adaptive_group n_groups (ct + target_groups)
  in

  let (groups, n_sample), _group_time =
    Synth_utils.timed (fun () ->
        adaptive_group 0
          (if !prev_group_sample > 0 then !prev_group_sample else target_groups))
  in
  prev_group_sample := n_sample;

  Hashtbl.iteri groups ~f:(fun ~key:class_ ~data:members ->
      (* insert new representative (some may already exist) *)
      if not @@ S.mem_class search_state class_ then (
        let value, op, args = NList.hd @@ Hashtbl.find_exn distinct_edges class_ in
        S.insert_class search_state value op @@ List.map ~f:S.Class.value args;

        let edges =
          List.concat_map members ~f:(fun c ->
              NList.to_list @@ Hashtbl.find_exn distinct_edges c)
        in
        S.insert_class_members search_state class_ edges))

let fill_search_space () =
  let ectx = ectx ()
  and search_state = search_state ()
  and ops = operators ()
  and max_cost = max_cost () in

  Log.log 1 (fun m -> m "Goal:\n%a" Value.pp (target ()));

  for cost = 1 to max_cost do
    Log.log 1 (fun m -> m "Start generating states of cost %d" cost);

    let (), run_time =
      Synth_utils.timed (fun () ->
          Gen.generate_states S.search_iter S.Class.value ectx search_state ops cost
          |> insert_states)
    in

    Log.log 1 (fun m ->
        m "Finish generating states of cost %d (runtime=%a)" cost Time.Span.pp run_time)
  done;
  exit 1

let backwards_pass class_ =
  let search_state = search_state () and max_cost = max_cost () and ectx = ectx () in
  match S.Class.value class_ with
  | Value.Scene _ ->
      Iter.forever (fun () ->
          S.local_greedy search_state (Int.ceil_log2 max_cost) (Value.eval_memoized ectx)
            target_distance class_
          |> Option.map ~f:local_search)
      |> Iter.filter_map Fun.id
  | _ -> Iter.empty

let synthesize () =
  let start_time = Time.now () in
  let search_state = search_state ()
  and target = target ()
  and ectx = ectx ()
  and backward_pass_repeats = backward_pass_repeats () in
  fill_search_space ();
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
             Log.log 1 (fun m -> m "Searching candidate %d (d=%f)" i d);
             Log.log 2 (fun m -> m "%a" Value.pp class_.value);

             backwards_pass class_
             |> Iter.take backward_pass_repeats
             |> Iter.min_floor
                  ~to_float:(fun p -> target_distance @@ Program.eval (Value.eval ectx) p)
                  0.0
             |> Option.iter ~f:(fun p ->
                    let found_value = Program.eval (Value.eval ectx) p in

                    Log.log 2 (fun m ->
                        m "Best (d=%f):\n%a" (target_distance found_value) Value.pp
                          found_value);
                    Log.sexp 2 (lazy [%message (p : Op.t Program.t)]);

                    if [%compare.equal: Value.t] target found_value then raise (Done p)));
      None
    with Done p -> Some p
  in
  runtime := Time.diff (Time.now ()) start_time;
  ret

let set_params ~scene_width ~scene_height ~group_threshold ~max_cost ~local_search_steps
    ~backward_pass_repeats ~verbosity ~validate ~scaling ~n_groups target_prog =
  let size = Scene2d.Dim.create ~xres:scene_width ~yres:scene_height ~scaling () in
  let ectx = Value.Ctx.create size in
  let target = Program.eval (Value.eval ectx) target_prog in
  let operators =
    Op.[ Union; Circle; Rect; Repl; Sub ]
    @ (List.range 0 (max size.xres size.yres) |> List.map ~f:(fun i -> Op.Int i))
    @ (List.range 2 5 |> List.map ~f:(fun i -> Op.Rep_count i))
  in

  List.find (Program.ops target_prog) ~f:(fun op ->
      not @@ List.mem ~equal:[%compare.equal: Op.t] operators op)
  |> Option.iter ~f:(fun op ->
         raise_s [%message "program not in search space" (op : Op.t)]);

  Set_once.set_exn params [%here]
    Params.
      {
        validate;
        local_search_steps;
        size;
        ectx;
        target;
        operators;
        group_threshold;
        max_cost;
        backward_pass_repeats;
        verbosity;
        search_state = S.create ();
        target_program = target_prog;
        target_groups = n_groups;
      }

let print_output m_prog =
  let program_size =
    Option.map m_prog ~f:(fun p -> Float.of_int @@ Program.size p)
    |> Option.value ~default:Float.nan
  in
  let program_json =
    Option.map m_prog ~f:(fun p -> `String (Sexp.to_string @@ Lang.serialize p))
    |> Option.value ~default:`Null
  in
  let open Yojson in
  Basic.to_channel Out_channel.stdout
    (`Assoc
      [
        ("method", `String "metric");
        ("scene_width", `Int (size ()).xres);
        ("scene_height", `Int (size ()).yres);
        ("scaling", `Int (size ()).scaling);
        ("local_search_steps", `Int (local_search_steps ()));
        ("group_threshold", `Float (group_threshold ()));
        ("max_cost", `Int (max_cost ()));
        ("backward_pass_repeats", `Int (backward_pass_repeats ()));
        ("program_size", `Float program_size);
        ("runtime", `Float (Time.Span.to_sec !runtime));
        ("program", program_json);
        ("n_groups", `Int (target_groups ()));
      ])

let cmd =
  let open Command.Let_syntax in
  Command.basic ~summary:"Solve CAD problems with metric synthesis."
    [%map_open
      let max_cost =
        flag "-max-cost" (required int) ~doc:" the maximum size of program to evaluate"
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
      and scene_width =
        flag "-scene-width" (optional_with_default 16 int) ~doc:" scene width in pixels"
      and scene_height =
        flag "-scene-height" (optional_with_default 16 int) ~doc:" scene height in pixels"
      and scaling =
        flag "-scaling" (optional_with_default 1 int) ~doc:" scene scaling factor"
      and verbosity =
        flag "-verbosity" (optional_with_default 0 int) ~doc:" set verbosity"
      and validate = flag "-validate" no_arg ~doc:" turn on validation" in
      fun () ->
        set_params ~max_cost ~group_threshold ~local_search_steps ~scene_width
          ~scene_height ~backward_pass_repeats ~verbosity ~validate ~scaling ~n_groups
        @@ Lang.parse
        @@ Sexp.input_sexp In_channel.stdin;
        synthesize () |> print_output]
