include struct
  open Dumb_params

  let spec = Spec.create ~name:"baseline" ()

  let synth = Spec.add spec @@ Param.const_str ~name:"synth" "baseline"

  let max_cost = Spec.add spec @@ Param.int ~name:"max-cost" ~doc:" max search cost" ()

  let bank_size = Spec.add spec @@ Param.float_ref ~name:"bank-size" ()

  let found_program = Spec.add spec @@ Param.bool_ref ~name:"found-program" ()

  let program_cost = Spec.add spec @@ Param.float_ref ~name:"program-cost" ()
end

module Make (Lang : Lang_intf.S) = struct
  open Lang
  module Search_state = Search_state_all.Make (Lang)
  module Gen = Synth_utils.Generate_list (Lang)

  exception Done of Op.t Program.t

  class synthesizer params =
    let _max_cost = Params.get params max_cost and _bench = Params.get params Lang.bench in
    object (self : 'self)
      val max_cost = _max_cost

      val bench = _bench

      val _ops = Bench.ops _bench

      val _output = Bench.output _bench

      val search_state = Search_state.create _max_cost

      val bank_size = Params.get params bank_size

      method ops = _ops

      method output = _output

      method get_search_state = search_state

      method generate_states = Gen.generate_states Search_state.search params search_state _ops

      method insert_states cost states =
        List.iter states ~f:(fun (state, op, args) -> Search_state.insert search_state cost state op args);
        bank_size := Float.of_int @@ Search_state.length search_state

      method check_states states =
        List.find_map states ~f:(fun (s, _, _) ->
            if [%compare.equal: Value.t] s _output then Some (Search_state.program_exn search_state s) else None)
        |> Option.iter ~f:(fun p -> raise (Done p))

      method fill cost =
        let new_states = self#generate_states cost in
        self#insert_states cost new_states;
        self#check_states new_states;
        Fmt.epr "Finished cost %d\n%!" cost;
        Search_state.print_stats search_state

      method run =
        try
          for cost = 0 to max_cost do
            self#fill cost
          done;
          None
        with Done p ->
          Params.get params found_program := true;
          Params.get params program_cost := Float.of_int @@ Program.size p;
          assert (Value.equal (Program.eval (Value.eval params) p) _output);
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
         (fun params -> new Synth.synthesizer params)
         params
         (Option.iter ~f:(fun p -> eprint_s ([%sexp_of: Lang.Op.t Program.t] p)))]
