include struct
  open Dumb_params

  let spec = Spec.inherit_ Baseline.spec "local-search-diverse"

  let (_ : _) = Spec.add spec @@ Param.const_str ~name:"synth" "local-search-diverse"
end

include struct
  module Lang = Cad
  open Lang
  module Parent = Baseline.Make (Lang)
  module Search_state = Parent.Search_state

  let local_search = Local_search.stochastic

  class synthesizer params =
    let _bench = Params.get params bench in
    object (self)
      inherit Parent.synthesizer params as super

      method dedup_states states =
        states
        |> List.filter ~f:(fun (s, _, _) -> not (Search_state.mem search_state s))
        |> List.dedup_and_sort ~compare:(fun (s, _, _) (s', _, _) -> [%compare: Value.t] s s')

      method search_close_states new_states =
        let new_states_a = Array.of_list new_states in
        let classes =
          List.mapi new_states ~f:(fun i ((v, _, _) as x) -> (v, (i, x, Union_find.create i)))
          |> Hashtbl.of_alist_exn (module Value)
        in
        let module F = Flat_program.Make (Op) in
        Hashtbl.iteri classes ~f:(fun ~key:_ ~data:(i, (_, op, args), c) ->
            if Union_find.get c = i then
              let p = Search_state.program_of_op_args_exn search_state op args in
              local_search params _bench
                ~view:(fun v' ->
                  match Hashtbl.find classes v' with Some (_, _, c') -> Union_find.union c c' | None -> ())
                Cad_conc.jaccard p
                (fun p -> raise @@ Parent.Done p));
        let to_keep =
          Hashtbl.data classes |> List.map ~f:Tuple.T3.get3 |> List.map ~f:Union_find.get
          |> List.dedup_and_sort ~compare
          |> List.map ~f:(fun i -> new_states_a.(i))
        in
        Fmt.epr "Retained %d/%d new states\n\n%!" (List.length to_keep) (List.length new_states);
        to_keep

      method! generate_states cost =
        let new_states = super#generate_states cost |> self#dedup_states in
        self#search_close_states new_states
    end

  let synth params = Option.iter (new synthesizer params)#run ~f:(fun p -> eprint_s [%message (p : Op.t Program.t)])
end

let cli =
  let spec = Dumb_params.Spec.union [ Lang.spec; Params.spec; spec ] in
  let open Command.Let_syntax in
  Command.basic ~summary:(sprintf "Diversity sampling for %s" Lang.name)
  @@ [%map_open
       let params = Dumb_params.Spec.cli spec in
       Synth_utils.run_synth
         (fun params -> new synthesizer params)
         params
         (Option.iter ~f:(fun p -> eprint_s [%message (p : Lang.Op.t Program.t)]))]
