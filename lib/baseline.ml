open Std

include struct
  open Dumb_params

  let spec = Spec.create ~name:"baseline" ()

  let synth = Spec.add spec @@ Param.const_str ~name:"synth" "baseline"

  let max_cost = Spec.add spec @@ Param.int ~name:"max-cost" ~doc:" max search cost" ()

  let verbose = Spec.add spec @@ Param.bool ~init:(`Cli (Some false)) ~name:"verbose" ~doc:" verbose output" ()

  let bank_size = Spec.add spec @@ Param.float_ref ~name:"bank-size" ()

  let found_program = Spec.add spec @@ Param.bool_ref ~name:"found-program" ()

  let program_cost = Spec.add spec @@ Param.float_ref ~name:"program-cost" ()
end

module Make (Lang : Lang_intf.S) = struct
  open Lang
  module Search_state = Search_state_all.Make (Lang)
  module Gen = Synth_utils.Generate_list (Lang)

  exception Done of Op.t Program.t

  module Ctx = struct
    type t = {
      max_cost : int;
      verbose : bool;
      ectx : Value.Ctx.t;
      ops : Op.t list;
      output : Value.t;
      bank_size : float ref;
      found_program : bool ref;
      program_cost : float ref;
    }

    let create ?stats ?(verbose = false) ~max_cost ectx ops output () =
      let stats = Option.value_lazy stats ~default:(lazy (Stats.create ())) in
      {
        max_cost;
        verbose;
        ectx;
        ops;
        output;
        bank_size = Stats.add_probe_exn stats "bank-size";
        found_program = ref false;
        program_cost = Stats.add_probe_exn stats "program-cost";
      }

    let of_params params =
      let bench = Params.get params bench in
      {
        max_cost = Params.get params max_cost;
        verbose = Params.get params verbose;
        ectx = Value.Ctx.of_params params;
        ops = Bench.ops bench;
        output = Bench.output bench;
        bank_size = Params.get params bank_size;
        found_program = Params.get params found_program;
        program_cost = Params.get params program_cost;
      }
  end

  class synthesizer (ctx : Ctx.t) =
    object (self : 'self)
      val max_cost = ctx.max_cost

      val verbose = ctx.verbose

      val search_state = Search_state.create ctx.max_cost

      val eval_ctx = ctx.ectx

      val ops = ctx.ops

      val output = ctx.output

      method get_search_state = search_state

      method generate_states cost = Gen.generate_states Search_state.search eval_ctx search_state ops cost

      method insert_states cost states =
        List.iter states ~f:(fun (state, op, args) -> Search_state.insert search_state cost state op args);
        ctx.bank_size := Float.of_int @@ Search_state.length search_state

      method check_states states =
        List.find_map states ~f:(fun (s, op, args) ->
            if [%compare.equal: Value.t] s output then Some (Search_state.program_of_op_args_exn search_state op args)
            else None)
        |> Option.iter ~f:(fun p -> raise (Done p))

      method fill cost =
        let new_states = self#generate_states cost in
        self#insert_states cost new_states;
        self#check_states new_states;
        if verbose then (
          Fmt.epr "Finished cost %d\n%!" cost;
          Search_state.print_stats search_state)

      method run =
        try
          for cost = 0 to max_cost do
            self#fill cost
          done;
          None
        with Done p ->
          ctx.found_program := true;
          ctx.program_cost := Float.of_int @@ Program.size p;
          assert (Value.equal (Program.eval (Value.eval eval_ctx) p) output);
          Some p
    end
end

let cli (type value op) (module Lang : Lang_intf.S with type Value.t = value and type Op.t = op) =
  let module Synth = Make (Lang) in
  let spec = Dumb_params.Spec.union [ Lang.spec; Params.spec; spec ] in
  let open Command.Let_syntax in
  Command.basic ~summary:(sprintf "Enumerative baseline for %s" Lang.name)
  @@ [%map_open
       let params = Dumb_params.Spec.cli spec in
       Synth_utils.run_synth
         (fun params -> new Synth.synthesizer @@ Synth.Ctx.of_params params)
         params
         (Option.iter ~f:(fun p -> eprint_s ([%sexp_of: Lang.Op.t Program.t] p)))]
