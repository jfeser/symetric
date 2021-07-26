include struct
  open Dumb_params

  let spec = Spec.create ~name:"baseline" ()

  let synth = Spec.add spec @@ Param.const_str ~name:"synth" "baseline"

  let max_cost = Spec.add spec @@ Param.int ~name:"max-cost" ~doc:" max search cost" ()
end

module Make (Lang : Lang_intf.S) = struct
  open Lang
  module Search_state = Search_state_append.Make (Lang)
  module Gen = Synth_utils.Generate_list (Lang)

  exception Done of Op.t Program.t

  class synthesizer params =
    let _max_cost = Params.get params max_cost and _bench = Params.get params Lang.bench in
    object (self : 'self)
      val max_cost = _max_cost

      val bench = _bench

      val ops = Bench.ops _bench

      val output = Bench.output _bench

      val search_state = Search_state.create _max_cost

      method generate_states = Gen.generate_states Search_state.search params search_state ops

      method insert_states cost states =
        List.iter states ~f:(fun (state, op, args) -> Search_state.insert search_state cost state op args)

      method fill cost =
        let new_states = self#generate_states cost in
        self#insert_states cost new_states;

        let solutions =
          List.filter_map new_states ~f:(fun (s, _, _) ->
              if [%compare.equal: Value.t] s output then Some (Search_state.program_exn search_state s) else None)
        in

        Fmt.epr "Finished cost %d\n%!" cost;
        Search_state.print_stats search_state;

        if not (List.is_empty solutions) then (
          List.iter solutions ~f:(fun p -> eprint_s [%message (p : Op.t Program.t)]);
          raise (Done (List.hd_exn solutions)))

      method run =
        try
          for cost = 0 to max_cost do
            self#fill cost
          done;
          None
        with Done p ->
          assert (Value.equal (Program.eval (Value.eval params) p) output);
          Some p
    end

  let synth params = Option.iter (new synthesizer params)#run ~f:(fun p -> eprint_s [%message (p : Op.t Program.t)])
end

let cli (type value op) (module Lang : Lang_intf.S with type Value.t = value and type Op.t = op) =
  let module Synth = Make (Lang) in
  let spec = Dumb_params.Spec.union [ Lang.spec; Params.spec; spec ] in
  let open Command.Let_syntax in
  Command.basic ~summary:(sprintf "Enumerative baseline for %s" Lang.name)
  @@ [%map_open
       let params = Dumb_params.Spec.cli spec in
       Synth_utils.run_synth Synth.synth params]
