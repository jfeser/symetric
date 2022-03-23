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
  let user_time () = (Unix.times ()).tms_cutime
  let sys_time () = (Unix.times ()).tms_cstime

  type t = {
    runtime : Timer.t;
    user_time : float Getter.t;
    sys_time : float Getter.t;
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
      user_time;
      sys_time;
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

let similarity (v : Value.t) (v' : Value.t) =
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

type group_result = { groups : S.Class.t list Hashtbl.M(S.Class).t; n_queries : int }

(** return a mapping from representatives to the members of their groups *)
let group_states_vp states : group_result =
  let thresh = group_threshold () in

  let distance c c' =
    let v = S.Class.value c and v' = S.Class.value c' in
    similarity v v'
  in
  let create_vp = Vpt.create distance `Random in

  let reference_vp = ref (create_vp @@ (* (S.classes search_state |> Iter.to_list) *) [])
  and reference = ref []
  and groups = Hashtbl.create (module S.Class) in
  let group_radii = Hashtbl.create (module S.Class) in
  let n_queries = ref 0 in

  let find_close c f =
    incr n_queries;
    if thresh >. 0.0 then (
      if List.length !reference > 100 then (
        reference_vp := create_vp ((Iter.to_list @@ Vpt.iter !reference_vp) @ !reference);
        reference := []);
      (Iter.append
         (Vpt.neighbors distance c thresh !reference_vp)
         (Iter.filter (fun c' -> distance c c' <=. thresh) (Iter.of_list !reference)))
        f)
  in

  states (fun v ->
      let is_empty =
        find_close v
        |> Iter.iter_is_empty (fun c' ->
               Hashtbl.update groups c' ~f:(function None -> [ v ] | Some vs -> v :: vs);
               Hashtbl.update group_radii c' ~f:(function
                 | None -> (distance c' v, 1)
                 | Some (n, d) -> (n +. distance c' v, d + 1)))
      in
      if is_empty then (
        Hashtbl.add_exn group_radii ~key:v ~data:(0.0, 1);
        Hashtbl.add_exn groups ~key:v ~data:[];
        reference := v :: !reference));
  { groups; n_queries = !n_queries }

module Edge = struct
  let value (v, _, _) = v
  let score value = -1. *. target_distance value
end

module Top_k_cache = struct
  type ('a, 'b) t = {
    gen_cache : int -> ('a, 'b list) Hashtbl.t;
    mutable cache : ('a, 'b list) Hashtbl.t;
    score : 'a -> float;
  }

  let create ?(initial_cached = 10_000) ~score gen_cache =
    { gen_cache; cache = gen_cache initial_cached; score }

  let get this n =
    if n > Hashtbl.length this.cache then this.cache <- this.gen_cache n;
    let ret =
      Hashtbl.create (Hashtbl.hashable_s this.cache) ~size:n ~growth_allowed:false
    in
    Iter.of_hashtbl this.cache
    |> Iter.map (fun (k, v) -> (this.score k, (k, v)))
    |> Iter.top_k ~compare:(fun (d, _) (d', _) -> [%compare: float] d d') n
    |> Iter.iter (fun (_, (k, v)) -> Hashtbl.add_exn ret ~key:k ~data:v);
    ret
end

let insert_states all_edges =
  let target_groups = target_groups () in
  let target_groups_min =
    Float.(to_int @@ (of_int target_groups *. (1.0 -. target_groups_err)))
  and target_groups_max =
    Float.(to_int @@ (of_int target_groups *. (1.0 +. target_groups_err)))
  in
  let search_state = get_search_state () in

  let top_k_cache =
    Top_k_cache.create ~score:Edge.score (fun n ->
        Iter.top_k_distinct_grouped
          (module Value)
          ~key:Edge.value ~score:Edge.score n (all_edges ()))
  in
  let sample n = Top_k_cache.get top_k_cache n in

  let group n_sample =
    let sample = sample n_sample in
    let group_result =
      group_states_vp @@ fun f ->
      Hashtbl.iter sample ~f:(function
        | (value, op, _) :: _ -> f (S.Class.create value (Op.ret_type op))
        | _ -> assert false)
    in
    let groups = group_result.groups in
    Log.sexp 0 (lazy [%message (n_sample : int) (Hashtbl.length groups : int)]);
    (groups, sample)
  in

  (* compute an upper bound on n_sample. if a good group is found, return it *)
  let rec adaptive_group_find_max n_sample =
    let groups, sample = group n_sample in
    let n_groups = Hashtbl.length groups and n_states = Hashtbl.length sample in

    if n_groups > target_groups_max then First n_sample
    else if n_sample > n_states || n_groups >= target_groups_min then
      Second (groups, sample)
    else adaptive_group_find_max (n_sample * 2)
  in

  let rec adaptive_group min_sample max_sample =
    let n_sample = (min_sample + max_sample) / 2 in
    let groups, sample = group n_sample in
    let n_groups = Hashtbl.length groups and n_states = Hashtbl.length sample in

    if min_sample >= max_sample then (groups, sample)
    else if n_groups > target_groups_max then adaptive_group min_sample n_sample
    else if n_sample > n_states || n_groups >= target_groups_min then (groups, sample)
    else adaptive_group n_sample max_sample
  in

  let (groups, sample), _group_time =
    Synth_utils.timed (fun () ->
        match adaptive_group_find_max target_groups with
        | First max_sample -> adaptive_group (max_sample / 2) max_sample
        | Second gs -> gs)
  in

  Hashtbl.iteri groups ~f:(fun ~key:class_ ~data:members ->
      (* insert new representative (some may already exist) *)
      (if not @@ S.mem_class search_state class_ then
       let value, op, args =
         List.hd_exn @@ Hashtbl.find_exn sample (S.Class.value class_)
       in
       S.insert_class search_state value op @@ List.map ~f:S.Class.value args);

      List.iter members ~f:(fun c ->
          S.insert_class_members search_state class_
          @@ Hashtbl.find_exn sample (S.Class.value c)))

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
          else insert_states states_iter)
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
