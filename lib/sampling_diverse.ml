include struct
  open Dumb_params

  let spec = Spec.inherit_ Baseline.spec "sampling-diverse"

  let retain_thresh =
    Spec.add spec
    @@ Param.float ~name:"retain-thresh" ~aliases:[ "r" ]
         ~doc:" retain points that are at least r away from their closest neighbor" ~init:(`Cli (Some 0.0)) ()

  let search_thresh =
    Spec.add spec
    @@ Param.float ~name:"search-thresh" ~aliases:[ "d" ] ~doc:" exhaustive search threshold" ~init:(`Cli (Some 1.0)) ()

  let ball_width =
    Spec.add spec @@ Param.int ~name:"width" ~aliases:[ "w" ] ~doc:" exhaustive search width" ~init:(`Cli (Some 2)) ()

  let diversity =
    Spec.add spec @@ Param.bool ~name:"diversity" ~doc:" use diversity sampling" ~init:(`Cli (Some true)) ()

  let bank_size = Spec.add spec @@ Param.float_ref ~name:"bank-size" ()

  let final_value_dist = Spec.add spec @@ Param.float_ref ~name:"final-value-dist" ()

  let final_program_dist = Spec.add spec @@ Param.float_ref ~name:"final-program-dist" ()

  let program_cost = Spec.add spec @@ Param.float_ref ~name:"program-cost" ()

  let found_program = Spec.add spec @@ Param.bool_ref ~name:"found-program" ()

  let closest_program = Spec.add spec @@ Param.float_ref ~name:"closest-program" ()

  let have_parts = Spec.add spec @@ Param.float_ref ~name:"have-parts" ()

  let total_parts = Spec.add spec @@ Param.float_ref ~name:"total-parts" ()

  let synth = Spec.add spec @@ Param.const_str ~name:"synth" "sampling-diverse"

  let value_dist = Spec.add spec @@ Param.float_list ~json:false ~name:"value-dist" ()

  let local_search_param ~name ?(json = true) () =
    (module struct
      type t = [ `Bounded | `Stochastic ] [@@deriving sexp_of]

      let key = Univ_map.Key.create ~name [%sexp_of: t]

      let name = name

      let of_string = function
        | "bounded" -> `Bounded
        | "stochastic" -> `Stochastic
        | kind -> raise_s [%message "unexpected" (kind : string)]

      let to_string = function `Bounded -> "bounded" | `Stochastic -> "stochastic"

      let arg_type = Command.Arg_type.create of_string

      let init =
        First
          (let open Command.Param in
          let param =
            flag_optional_with_default_doc name ~doc:" kind of local search" ~default:`Bounded arg_type sexp_of_t
          in
          map param ~f:(fun v -> Univ_map.Packed.T (key, v)))

      let to_json = if json then Option.return @@ fun v -> `String (to_string v) else None
    end : Param.S
      with type t = [ `Bounded | `Stochastic ])

  let local_search = Spec.add spec @@ Param.create @@ local_search_param ~name:"local" ()
end

module Make (Lang : Lang_intf.S) = struct
  open Lang
  module Search_state = Search_state_append.Make (Lang)
  open Search_state
  module Gen = Synth_utils.Generate_list (Lang)
  module Baseline = Baseline.Make (Lang)

  class synthesizer params =
    object (self)
      inherit Baseline.synthesizer params as super

      val bank_size = Params.get params bank_size

      val search_thresh = Params.get params search_thresh

      val diversity = Params.get params diversity

      val ball_width = Params.get params ball_width

      val mutable retain_thresh = Params.get params retain_thresh

      method dist = Value.dist params

      method! insert_states cost states =
        super#insert_states cost states;
        bank_size := Float.of_int @@ Search_state.length search_state

      method dedup_states states =
        states
        |> List.filter ~f:(fun (s, _, _) -> not (Search_state.mem search_state s))
        |> List.dedup_and_sort ~compare:(fun (s, _, _) (s', _, _) -> [%compare: Value.t] s s')

      method search_neighbors ?(view = ignore) center found =
        match Params.get params local_search with
        | `Bounded -> (
            let check_program p =
              let v = Program.eval (Value.eval params) p in
              view v;
              if Value.equal v output then found p
            in
            try Tree_ball.Rename_insert_delete.ball (module Op) ops center ball_width check_program
            with Program.Eval_error e -> raise @@ Program.Eval_error [%message (center : Op.t Program.t) (e : Sexp.t)])
        | `Stochastic -> (
            try
              Tree_ball.Rename_insert_delete.stochastic
                (module Op)
                ~score:(fun p -> 1.0 -. (self#dist output @@ Program.eval (Value.eval params) p))
                ops center
                (fun p _ ->
                  let v = Program.eval (Value.eval params) p in
                  view v;
                  if [%compare.equal: Value.t] v output then found p)
            with Program.Eval_error e -> raise @@ Program.Eval_error [%message (center : Op.t Program.t) (e : Sexp.t)])

      method check_states cost new_states =
        List.iter new_states ~f:(fun (d, _, op, args) ->
            if Float.(d <= search_thresh) then (
              let center = program_of_op_args_exn search_state op args in

              self#search_neighbors center @@ fun p ->
              let final_value_dist = Params.get params final_value_dist
              and final_program_dist = Params.get params final_program_dist
              and program_cost = Params.get params program_cost
              and found_program = Params.get params found_program in

              final_value_dist := d;
              final_program_dist := Float.of_int @@ Tree_dist.zhang_sasha ~eq:[%compare.equal: Op.t] center p;
              program_cost := Float.of_int cost;
              found_program := true;
              raise (Baseline.Done p)))
      (** Check neighbors around new states *)

      method sample_states new_states =
        let sample_diverse new_states =
          Dumb_progress.List.map ~name:"sampling" new_states ~f:(fun ((_, new_state, _, _) as x) ->
              let min_dist =
                List.map (states search_state) ~f:(self#dist new_state)
                |> List.min_elt ~compare:[%compare: float]
                |> Option.value ~default:Float.infinity
              in
              (min_dist, x))
          |> List.filter ~f:(fun (d, _) -> Float.(d >= retain_thresh))
          |> List.map ~f:Tuple.T2.get2
        in
        if diversity then sample_diverse new_states else new_states

      method! generate_states cost =
        let new_states =
          super#generate_states cost |> self#dedup_states
          |> List.map ~f:(fun (s, op, args) -> (self#dist output s, s, op, args))
        in
        self#check_states cost new_states;
        self#sample_states new_states |> List.map ~f:(fun (_, x, y, z) -> (x, y, z))

      method! run =
        let rec reduce_retain_thresh () =
          match super#run with
          | Some p -> Some p
          | None ->
              (* TODO: Should we clear the search space here? *)
              retain_thresh <- retain_thresh /. 2.0;
              reduce_retain_thresh ()
        in
        reduce_retain_thresh ()
    end

  let synth params = Option.iter (new synthesizer params)#run ~f:(fun p -> eprint_s [%message (p : Op.t Program.t)])
end

let cli (type value op) (module Lang : Lang_intf.S with type Value.t = value and type Op.t = op) =
  let module Synth = Make (Lang) in
  let spec = Dumb_params.Spec.union [ Lang.spec; Params.spec; spec ] in
  let open Command.Let_syntax in
  Command.basic
    ~summary:(sprintf "Diversity sampling for %s" Lang.name)
    [%map_open
      let params = Dumb_params.Spec.cli spec in
      Synth_utils.run_synth Synth.synth params]
