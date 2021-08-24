include struct
  open Dumb_params

  let spec = Spec.inherit_ Baseline.spec "local-search-diverse"

  let search_close_states_time =
    Spec.add spec @@ Param.(mut @@ float ~name:"search-close-states-time" ~doc:"" ~init:(`Default (Fun.const 0.0)) ())

  let sample_states_time =
    Spec.add spec @@ Param.(mut @@ float ~name:"sample-states-time" ~doc:"" ~init:(`Default (Fun.const 0.0)) ())

  let (_ : _) = Spec.add spec @@ Param.const_str ~name:"synth" "local-search-diverse"
end

include struct
  module Lang = Cad
  open Lang
  module Parent = Baseline.Make (Lang)
  module Search_state = Parent.Search_state

  let local_search = Local_search.full

  let with_time t f =
    let start_time = Time.now () in
    let ret = f () in
    let end_time = Time.now () in
    (t := !t +. Time.Span.(to_ms Time.(diff end_time start_time)));
    ret

  class synthesizer params =
    let _bench = Params.get params bench
    and _search_width = 100
    and _search_close_states_time = Params.get params search_close_states_time
    and _sample_states_time = Params.get params sample_states_time in
    object (self)
      inherit Parent.synthesizer params as super

      val search_width = _search_width

      val mutable sample_width = _search_width

      method dedup_states states =
        states
        |> List.filter ~f:(fun (s, _, _) -> not (Search_state.mem search_state s))
        |> List.dedup_and_sort ~compare:(fun (s, _, _) (s', _, _) -> [%compare: Value.t] s s')

      method search_close_states new_states =
        with_time _search_close_states_time @@ fun () ->
        let search_states = List.filter new_states ~f:(fun (v, _, _) -> Float.(Cad_conc.jaccard _output v < 0.05)) in
        Fmt.epr "Searching %d/%d neighborhoods\n%!" (List.length search_states) (List.length new_states);

        List.iter search_states ~f:(fun (_, op, args) ->
            let center = Search_state.program_of_op_args_exn search_state op args in
            local_search ~n:search_width ~target:_output params center @@ fun p _ ->
            if [%compare.equal: Value.t] (Program.eval (Value.eval params) p) _output then raise @@ Parent.Done p)

      method sample_diverse_states new_states =
        with_time _sample_states_time @@ fun () ->
        let new_states_a = Array.of_list new_states in
        let classes =
          List.mapi new_states ~f:(fun i ((v, _, _) as x) -> (v, (i, x, Union_find.create i)))
          |> Hashtbl.of_alist_exn (module Value)
        in
        let module F = Flat_program.Make (Op) in
        Hashtbl.iteri classes ~f:(fun ~key:_ ~data:(i, (_, op, args), c) ->
            if Union_find.get c = i then
              for _ = 0 to 2 do
                let p = Search_state.program_of_op_args_exn search_state op args in
                local_search ~n:sample_width params p (fun p _ ->
                    let v' = Program.eval (Value.eval params) p in

                    (* if [%compare.equal: Value.t] v' _output then raise @@ Parent.Done p; *)
                    match Hashtbl.find classes v' with Some (_, _, c') -> Union_find.union c c' | None -> ())
              done);

        let to_keep =
          Hashtbl.data classes |> List.map ~f:Tuple.T3.get3 |> List.map ~f:Union_find.get
          |> List.dedup_and_sort ~compare
          |> List.map ~f:(fun i -> new_states_a.(i))
        in
        Fmt.epr "Retained %d/%d new states\n\n%!" (List.length to_keep) (List.length new_states);
        to_keep

      method! generate_states cost =
        let new_states = super#generate_states cost |> self#dedup_states in
        self#search_close_states new_states;
        self#sample_diverse_states new_states

      method! run =
        let rec reduce_sample_width () =
          match super#run with
          | Some p -> Some p
          | None ->
              Search_state.clear search_state;
              sample_width <- sample_width / 2;
              reduce_sample_width ()
        in
        reduce_sample_width ()
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
