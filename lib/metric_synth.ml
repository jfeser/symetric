open Std

module type DSL = sig
  module Type : sig
    type t [@@deriving compare, equal, hash, sexp]

    val output : t
  end

  module Op : sig
    type t [@@deriving compare, equal, hash, sexp, yojson]

    val cost : t -> int
    val arity : t -> int
    val args_type : t -> Type.t list
    val ret_type : t -> Type.t
    val is_commutative : t -> bool
    val pp : t Fmt.t
  end

  module Value : sig
    type t [@@deriving compare, equal, hash, sexp]

    module Ctx : sig
      type t

      val default : t
    end

    include Comparator.S with type t := t

    val mk_eval_memoized : unit -> Ctx.t -> Op.t -> t list -> t
    val eval : Ctx.t -> Op.t -> t list -> t
    val distance : t -> t -> float
    val is_error : t -> bool
    val pp : t Fmt.t
  end

  val operators : Op.t list
  val parse : Sexp.t -> Op.t Program.t
  val serialize : Op.t Program.t -> Sexp.t
  val rewrite : Op.t Program.t -> Op.t Program.t list
end

(* constants *)
let max_repeat_count = 4

(* parameters *)
module Params = struct
  type t = {
    local_search_steps : int;
    group_threshold : float;
    max_cost : int;
    backward_pass_repeats : int;
    verbosity : int;
    validate : bool;
    target_groups : int;
    dump_search_space : string option;
    load_search_space : string option;
    output_file : string;
    use_beam_search : bool; (* if true, then disable clustering *)
    use_ranking : bool; (* if false, disable ranking clustered states *)
    extract : [ `Greedy | `Random | `Centroid | `Exhaustive ];
    repair : [ `Guided | `Random ];
    exhaustive_width : int;
  }
  [@@deriving yojson]

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
      and exhaustive_width =
        flag "-exhaustive-width" (optional_with_default 4 int)
          ~doc:" search width for exhaustive extraction"
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
      and output_file = flag "-out" (required string) ~doc:" output to file" in
      let extract =
        match extract with
        | "greedy" -> `Greedy
        | "random" -> `Random
        | "centroid" -> `Centroid
        | "exhaustive" -> `Exhaustive
        | _ -> raise_s [%message "unexpected" (extract : string)]
      in
      let repair =
        match repair with
        | "guided" -> `Guided
        | "random" -> `Random
        | _ -> raise_s [%message "unexpected" (repair : string)]
      in
      {
        validate;
        local_search_steps;
        group_threshold;
        max_cost;
        backward_pass_repeats;
        verbosity;
        target_groups = n_groups;
        dump_search_space;
        load_search_space;
        output_file;
        use_beam_search;
        use_ranking;
        extract;
        repair;
        exhaustive_width;
      }]
end

module Stats = struct
  type time_span = Time.Span.t

  let yojson_of_time_span t = `Float (Time.Span.to_sec t)

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

module Log = struct
  let start_time = Time.now ()

  let log (p : Params.t) level msgf =
    let with_time fmt =
      Format.fprintf Format.err_formatter
        ("[%a] @[" ^^ fmt ^^ "@]@.%!")
        Time.Span.pp
        (Time.diff (Time.now ()) start_time)
    in
    if level <= p.verbosity then msgf with_time

  let sexp (p : Params.t) level lsexp =
    if level <= p.verbosity then eprint_s @@ Lazy.force lsexp
end

module Make (Lang : DSL) = struct
  open Lang
  module S = Search_state_all.Make (Lang)
  module Gen = Generate.Gen_iter (Lang)

  let search_state = ref (S.create ())
  let[@inline] get_search_state () = !search_state
  let ectx = Value.Ctx.default
  let target_program = Set_once.create ()

  let target =
    lazy (Program.eval (Value.eval ectx) @@ Set_once.get_exn target_program [%here])

  let[@inline] target () = Lazy.force target
  let stats = Stats.create ()

  let write_output (p : Params.t) m_prog =
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
          ("params", [%yojson_of: Params.t] p);
        ]
    in
    Out_channel.with_file p.output_file ~f:(fun ch -> Safe.to_channel ch json)

  let distance = Value.distance
  let target_distance v = Value.distance (target ()) v

  let local_search (p : Params.t) program =
    Synth_utils.timed (`Add stats.repair_time) (fun () ->
        let value_eval = Value.mk_eval_memoized () in
        Local_search.of_unnormalize_tabu ~target_distance
          ~random:(match p.repair with `Guided -> false | `Random -> true)
          (module Op)
          (module Value)
          rewrite
          (Program.eval (value_eval ectx))
          program
        |> Iter.map (fun p -> (target_distance (Program.eval (value_eval ectx) p), p))
        |> Iter.take p.local_search_steps
        |> Iter.mapi (fun i (d, x) -> (d, i, x))
        |> Iter.min_floor ~to_float:(fun (d, _, _) -> d) 0.0
        |> Option.map ~f:(fun (_, i, p) -> (i, p))
        |> Option.value ~default:(-1, program))

  module Edge = struct
    type t = Value.t * (Op.t[@compare.ignore]) * (S.Class.t list[@compare.ignore])
    [@@deriving compare, hash, sexp]

    let value (v, _, _) = v
    let score value = -1. *. target_distance value
    let distance (v, _, _) (v', _, _) = distance v v'
  end

  let select_top_k_edges edges =
    Iter.ordered_groupby (module Value) ~score:Edge.score ~key:(fun (v, _, _) -> v) edges
    |> Iter.timed stats.rank_time
    |> Iter.map (fun (v, (_, es)) -> (v, es))

  let select_arbitrary edges = Iter.map (fun ((v, _, _) as edge) -> (v, [ edge ])) edges

  let select_edges (p : Params.t) edges =
    if p.use_ranking then select_top_k_edges edges else select_arbitrary edges

  let insert_states (p : Params.t) cost (all_edges : Edge.t Iter.t) =
    let search_state = get_search_state () in

    let module Edges = struct
      type t = Value.t * (Edge.t list[@compare.ignore]) [@@deriving compare, hash, sexp]

      let distance (v, _) (v', _) = distance v v'
    end in
    let groups =
      all_edges |> select_edges p
      |> Grouping.create_m (module Edges) p.group_threshold Edges.distance p.target_groups
    in
    (stats.cluster_time := Time.Span.(!(stats.cluster_time) + groups.runtime));

    Log.log p 1 (fun m ->
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

  let insert_states_beam (p : Params.t) cost all_edges =
    let search_state = get_search_state () in

    all_edges
    |> Iter.filter (fun (value, op, _) ->
           let type_ = Op.ret_type op in
           let class_ = S.Class.create type_ cost value in
           not (S.mem_class search_state class_))
    |> Iter.top_k_distinct
         (module Value)
         ~score:Edge.score ~key:Edge.value p.target_groups
    |> Iter.iter (fun (value, op, args) ->
           let type_ = Op.ret_type op in
           let class_ = S.Class.create type_ cost value in
           if not (S.mem_class search_state class_) then
             S.insert_class search_state type_ cost value op args)

  let fill_search_space (p : Params.t) =
    Synth_utils.timed (`Set stats.xfta_time) (fun () ->
        let search_state = get_search_state () in

        Log.log p 1 (fun m -> m "Goal:\n%a" Value.pp (target ()));

        let states_iter cost =
          Gen.generate_states S.search_iter S.Class.value ectx search_state operators cost
          |> Iter.timed stats.expansion_time
        in

        let run_time = ref Time.Span.zero in

        for cost = 1 to p.max_cost do
          Log.log p 1 (fun m -> m "Start generating states of cost %d" cost);

          Synth_utils.timed (`Set run_time) (fun () ->
              if p.use_beam_search then insert_states_beam p cost (states_iter cost)
              else insert_states p cost (states_iter cost));

          incr stats.max_cost_generated;
          write_output p None;
          Log.log p 1 (fun m ->
              m "Finish generating states of cost %d (runtime=%a)" cost Time.Span.pp
                !run_time)
        done)

  let extract (p : Params.t) eval height class_ =
    Synth_utils.timed (`Add stats.extract_time) (fun () ->
        let search_state = get_search_state () in
        match p.extract with
        | `Greedy -> S.local_greedy search_state height eval target_distance class_
        | `Exhaustive ->
            S.exhaustive ~width:p.exhaustive_width search_state height eval
              target_distance class_
        | `Random -> S.random search_state height class_
        | `Centroid -> S.centroid search_state height class_)

  let backwards_pass (p : Params.t) class_ =
    let height = Int.ceil_log2 p.max_cost in
    if [%equal: Type.t] (S.Class.type_ class_) Type.output then
      let eval = Value.mk_eval_memoized () ectx in
      Iter.forever (fun () ->
          extract p eval height class_ |> Option.map ~f:(local_search p))
    else Iter.empty

  let synthesize (p : Params.t) target_program' =
    Set_once.set_exn target_program [%here] target_program';
    let target = target () in
    Timer.start stats.runtime;

    (match p.load_search_space with
    | Some fn -> In_channel.with_file fn ~f:(fun ch -> search_state := S.of_channel ch)
    | None ->
        fill_search_space p;
        Option.iter p.dump_search_space ~f:(fun fn ->
            Out_channel.with_file fn ~f:(fun ch -> S.to_channel ch @@ get_search_state ())));

    write_output p None;
    let search_state = get_search_state () in

    let exception Done of Op.t Program.t in
    Log.log p 1 (fun m -> m "Starting backwards pass");

    let classes =
      S.classes search_state
      |> Iter.filter (fun c -> [%compare.equal: Type.t] Type.output (S.Class.type_ c))
      |> Iter.map (fun c -> (target_distance @@ S.Class.value c, c))
      |> Iter.sort ~cmp:(fun (d, _) (d', _) -> [%compare: float] d d')
    in
    let ret =
      try
        classes
        |> Iter.iteri (fun i (d, (class_ : S.Class.t)) ->
               incr stats.groups_searched;
               Log.log p 1 (fun m -> m "Searching candidate %d (d=%f)" i d);
               Log.log p 2 (fun m -> m "@.%a" Value.pp (S.Class.value class_));

               backwards_pass p class_
               |> Iter.take p.backward_pass_repeats
               |> Iter.filter_map Fun.id
               |> Iter.mapi (fun backwards_pass_i (local_search_i, p) ->
                      ((backwards_pass_i, local_search_i), p))
               |> Iter.min_floor
                    ~to_float:(fun (_, p) ->
                      target_distance @@ Program.eval (Value.eval ectx) p)
                    0.0
               |> Option.iter
                    ~f:(fun ((backwards_pass_iters, local_search_iters), program) ->
                      let found_value = Program.eval (Value.eval ectx) program in

                      Log.log p 2 (fun m ->
                          m "Best (d=%f):@.%a" (target_distance found_value) Value.pp
                            found_value);
                      Log.sexp p 2 (lazy [%message (program : Op.t Program.t)]);

                      if [%compare.equal: Value.t] target found_value then (
                        Log.log p 0 (fun m ->
                            m "local search iters %d" local_search_iters);
                        Log.log p 0 (fun m ->
                            m "backwards pass iters %d" backwards_pass_iters);
                        raise (Done program))));
        None
      with Done p -> Some p
    in
    Timer.stop stats.runtime;
    write_output p ret
end

let cmd (module Dsl : DSL) =
  let open Make (Dsl) in
  let open Command.Let_syntax in
  Command.basic ~summary:"Solve CAD problems with metric synthesis."
    [%map_open
      let params = Params.cmd in
      fun () -> synthesize params (Dsl.parse (Sexp.input_sexp In_channel.stdin))]
