open Std
open Cad_ext
module S = Search_state_all.Make (Cad_ext)
module Gen = Generate.Gen_iter (Cad_ext)

module Params = struct
  type t = {
    size : Scene.Size.t;
    ectx : Value.Ctx.t;
    local_search_steps : int;
    target : Scene.t;
    target_edges : Scene.t;
    group_threshold : float;
    operators : Op.t list;
    filter : bool;
    max_cost : int;
    backward_pass_repeats : int;
    verbose : bool;
    search_state : S.t;
  }
end

let params = Set_once.create ()
let[@inline] size () = (Set_once.get_exn params [%here]).Params.size
let[@inline] ectx () = (Set_once.get_exn params [%here]).Params.ectx
let[@inline] target () = (Set_once.get_exn params [%here]).Params.target
let[@inline] target_edges () = (Set_once.get_exn params [%here]).Params.target_edges
let[@inline] search_state () = (Set_once.get_exn params [%here]).Params.search_state
let[@inline] group_threshold () = (Set_once.get_exn params [%here]).Params.group_threshold
let[@inline] operators () = (Set_once.get_exn params [%here]).Params.operators
let[@inline] filter () = (Set_once.get_exn params [%here]).Params.filter
let[@inline] max_cost () = (Set_once.get_exn params [%here]).Params.max_cost
let[@inline] verbose () = (Set_once.get_exn params [%here]).Params.verbose

let[@inline] backward_pass_repeats () =
  (Set_once.get_exn params [%here]).Params.backward_pass_repeats

let[@inline] local_search_steps () =
  (Set_once.get_exn params [%here]).Params.local_search_steps

let runtime = ref Time.Span.zero

let distance (v : Value.t) (v' : Value.t) =
  match (v, v') with Scene x, Scene x' -> Scene.jaccard x x' | _ -> Float.infinity

let local_search p =
  let target = Value.Scene (target ())
  and size = size ()
  and steps = local_search_steps ()
  and ectx = ectx () in
  Local_search.of_unnormalize_tabu ~target ~dist:distance
    (module Op)
    (module Value)
    (function
      | Apply (Int x, []) ->
          if x = 0 then [ Apply (Int (x + 1), []) ]
          else if x = max size.Scene.Size.xres size.yres then [ Apply (Int (x - 1), []) ]
          else [ Apply (Int (x + 1), []); Apply (Int (x - 1), []) ]
      | _ -> [])
    (Program.eval (Value.eval ectx))
    p
  |> Iter.map (fun p -> (distance target (Program.eval (Value.eval ectx) p), p))
  |> Iter.take steps
  |> Iter.min_floor ~to_float:(fun (d, _) -> d) 0.0
  |> Option.map ~f:(fun (_, p) -> p)
  |> Option.value ~default:p

let insert_states_nonempty ~cost ~type_ states =
  let search_state = search_state () and thresh = group_threshold () in
  let insert key states =
    List.iter states ~f:(fun (value, op, args) ->
        S.insert ~key search_state ~cost value op args)
  in

  let states =
    List.map states ~f:(fun ((v, _, _) as x) -> (v, x))
    |> List.group_by (module Value)
    (* If the state already exists in the space, insert its edges *)
    |> List.filter_map ~f:(function
         | value, ((_, op, _) :: _ as states)
           when S.mem search_state { value; type_ = Op.ret_type op } ->
             insert value states;
             None
         | value, ((_, op, args) :: _ as states) ->
             Some (value, ref false, op, args, states)
         | _, [] -> None)
    |> List.permute
  in

  let reference_list = S.states ~type_ search_state |> Iter.to_list in

  let list_time = ref Time.Span.zero in
  let[@inline] lookup_list v =
    let start_time = Time.now () in
    let ret = List.filter reference_list ~f:(fun v' -> Float.(distance v v' < thresh)) in
    (list_time := Time.(Span.( + ) !list_time @@ diff (now ()) start_time));
    ret
  in

  let n_found_vp = ref 0 and n_found_list = ref 0 in
  let (_ : _) =
    List.fold states ~init:[] ~f:(fun kept (v, _, _, _, states) ->
        let inserted_vp = ref false in
        let n_insertions = ref 0 in
        let close_existing = lookup_list v in
        List.iter close_existing ~f:(fun v' ->
            inserted_vp := true;
            insert v' states;
            incr n_insertions);
        if !inserted_vp then incr n_found_vp;

        let inserted_list = ref false in
        List.iter kept ~f:(fun v' ->
            if Float.(distance v v' < thresh) then (
              inserted_list := true;
              insert v' states));
        if !inserted_list then incr n_found_list;

        if !inserted_vp || !inserted_list then kept
        else (
          insert v states;
          v :: kept))
  in
  ()

let insert_states ~cost ~type_ = function
  | [] -> ()
  | states -> insert_states_nonempty ~cost ~type_ states

let filter_states states =
  let target = target () and target_edges = target_edges () and size = size () in
  Iter.filter
    (fun (v, _, _) ->
      match v with
      | Value.Scene s ->
          Bitarray.hamming_weight (Bitarray.and_ (Scene.pixels target) (Scene.pixels s))
          > 0
          && Bitarray.hamming_weight
               (Bitarray.and_ (Scene.pixels target_edges)
                  (Scene.pixels @@ Scene.edges size s))
             > 0
      | _ -> true)
    states

let fill_search_space () =
  let ectx = ectx ()
  and search_state = search_state ()
  and ops = operators ()
  and filter = filter ()
  and max_cost = max_cost ()
  and verbose = verbose () in

  for cost = 1 to max_cost do
    if verbose then Fmt.epr "Generating states of cost %d.\n%!" cost;
    let states = Gen.generate_states S.search_iter ectx search_state ops cost in
    let states = if filter then filter_states states else states in
    states
    |> Iter.map (fun ((_, op, _) as state) -> (Op.ret_type op, state))
    |> Iter.group_by (module Type)
    |> Iter.iter (fun (type_, states) -> insert_states ~cost ~type_ states)
  done

let synthesize () =
  let start_time = Time.now () in
  let search_state = search_state ()
  and target = Value.Scene (target ())
  and ectx = ectx ()
  and max_cost = max_cost ()
  and backward_pass_repeats = backward_pass_repeats () in
  fill_search_space ();

  let exception Done of Op.t Program.t in
  let ret =
    try
      Iter.of_hashtbl search_state.values
      |> Iter.map (fun ((key : S.Attr.t), data) ->
             Iter.of_queue data
             |> Iter.map (fun v ->
                    (distance target v, S.TValue.{ value = v; type_ = key.type_ })))
      |> Iter.concat
      |> Iter.top_k ~cmp:(fun (d, _) (d', _) -> [%compare: float] d' d) 100
      |> Iter.sort ~cmp:(fun (d, _) (d', _) -> [%compare: float] d d')
      |> Iter.iter (fun (_, (tv : S.TValue.t)) ->
             match tv.value with
             | Value.Scene _ ->
                 for _ = 1 to backward_pass_repeats do
                   let p =
                     S.local_greedy (Value.pp ectx) search_state (Int.ceil_log2 max_cost)
                       (Value.eval ectx) (distance target) tv
                     |> Option.value_exn
                   in
                   let found_value = Program.eval (Value.eval ectx) @@ local_search p in
                   if [%compare.equal: Value.t] target found_value then raise (Done p)
                 done
             | _ -> ());
      None
    with Done p -> Some p
  in
  runtime := Time.diff (Time.now ()) start_time;
  ret

let set_params ~scene_width ~scene_height ~group_threshold ~max_cost ~local_search_steps
    ~backward_pass_repeats ~filter ~verbose target =
  let size = Scene.Size.create ~xres:scene_width ~yres:scene_height () in
  let ectx = Value.Ctx.create size in
  let target_value = Program.eval (Value.eval ectx) target in
  let target_scene = match target_value with Scene s -> s | _ -> assert false in
  let operators =
    Op.[ Union; Circle; Rect; Repl; Sub ]
    @ (List.range 0 (max size.xres size.yres) |> List.map ~f:(fun i -> Op.Int i))
    @ (List.range 2 5 |> List.map ~f:(fun i -> Op.Rep_count i))
  in

  Set_once.set_exn params [%here]
    Params.
      {
        local_search_steps;
        size;
        ectx;
        target = target_scene;
        operators;
        group_threshold;
        max_cost;
        backward_pass_repeats;
        filter;
        verbose;
        search_state = S.create ();
        target_edges = Scene.edges size target_scene;
      }

let print_output m_prog =
  let program_size =
    Option.map m_prog ~f:(fun p -> Float.of_int @@ Program.size p)
    |> Option.value ~default:Float.nan
  in
  let program_json =
    Option.map m_prog ~f:(fun p -> `String (Sexp.to_string @@ Cad_ext.serialize p))
    |> Option.value ~default:`Null
  in
  let open Yojson in
  Basic.to_channel Out_channel.stdout
    (`Assoc
      [
        ("method", `String "metric");
        ("scene_width", `Int (size ()).xres);
        ("scene_height", `Int (size ()).yres);
        ("local_search_steps", `Int (local_search_steps ()));
        ("group_threshold", `Float (group_threshold ()));
        ("filter", `Bool (filter ()));
        ("max_cost", `Int (max_cost ()));
        ("backward_pass_repeats", `Int (backward_pass_repeats ()));
        ("program_size", `Float program_size);
        ("runtime", `Float (Time.Span.to_sec !runtime));
        ("program", program_json);
      ])

let cmd =
  let open Command.Let_syntax in
  Command.basic ~summary:"Solve CAD problems with metric synthesis."
    [%map_open
      let max_cost =
        flag "-max-cost"
          (optional_with_default 22 int)
          ~doc:" the maximum size of program to evaluate"
      and group_threshold =
        flag "-group-threshold"
          (optional_with_default 0.2 float)
          ~doc:"distance threshold to trigger grouping"
      and local_search_steps =
        flag "-local-search-steps"
          (optional_with_default 1000 int)
          ~doc:" number of steps to run local search"
      and backward_pass_repeats =
        flag "-backward-pass-repeats"
          (optional_with_default 10 int)
          ~doc:" number of times to run backward pass"
      and filter =
        flag "-use-filter"
          (optional_with_default true bool)
          ~doc:" use heuristic filtering"
      and scene_width =
        flag "-scene-width" (optional_with_default 12 int) ~doc:" scene width in pixels"
      and scene_height =
        flag "-scene-height" (optional_with_default 20 int) ~doc:" scene height in pixels"
      and verbose = flag "-verbose" no_arg ~doc:" increase verbosity" in
      fun () ->
        set_params ~max_cost ~group_threshold ~local_search_steps ~scene_width
          ~scene_height ~backward_pass_repeats ~filter ~verbose
        @@ Cad_ext.parse
        @@ Sexp.input_sexp In_channel.stdin;
        synthesize () |> print_output]
