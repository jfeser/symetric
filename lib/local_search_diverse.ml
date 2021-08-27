include struct
  open Dumb_params

  let spec = Spec.inherit_ Baseline.spec "local-search-diverse"

  let search_close_states_time =
    Spec.add spec @@ Param.(mut @@ float ~name:"search-close-states-time" ~doc:"" ~init:(`Default (Fun.const 0.0)) ())

  let sample_states_time =
    Spec.add spec @@ Param.(mut @@ float ~name:"sample-states-time" ~doc:"" ~init:(`Default (Fun.const 0.0)) ())

  let rules_fn = Spec.add spec @@ Param.string ~name:"rules-file" ~init:(`Cli (Some "")) ~doc:"" ()

  let (_ : _) = Spec.add spec @@ Param.const_str ~name:"synth" "local-search-diverse"
end

include struct
  module Lang = Cad
  open Lang
  module Parent = Baseline.Make (Lang)
  module Search_state = Parent.Search_state

  let with_time t f =
    let start_time = Time.now () in
    let ret = f () in
    let end_time = Time.now () in
    (t := !t +. Time.Span.(to_ms Time.(diff end_time start_time)));
    ret

  class synthesizer params =
    let _search_width = 100
    and _search_close_states_time = Params.get params search_close_states_time
    and _sample_states_time = Params.get params sample_states_time
    and _max_cost = Params.get params Baseline.max_cost
    and _ops = Bench.ops @@ Params.get params bench
    and _rules =
      let fn = Params.get params rules_fn in
      if String.(fn = "") then None
      else
        let rules =
          Sexp.load_sexp_conv_exn fn [%of_sexp: (Local_search.Rule.t * float * float * float) list]
          |> List.map ~f:(fun (r, _, _, _) -> r)
        in
        let rules = List.take rules 100 in

        Some rules
    in
    object (self)
      inherit Parent.synthesizer (Parent.Ctx.of_params params) as super

      val search_width = _search_width

      val sample_width = Array.create ~len:(_max_cost + 1) 2

      val pats = Option.value _rules ~default:Local_search.Pattern.(rename_patterns _ops @ push_pull_replicate _ops)

      method local_search ?n ?target =
        Local_search.of_rules_root_only ?n ?target pats (Program.eval (Value.eval eval_ctx))

      method local_search_diverse ~n term k =
        let k' t' =
          k t';
          self#local_search_diverse ~n:(n - 1) term k
        in
        if n > 0 then
          let open Local_search in
          List.iter pats ~f:(fun rule ->
              Option.iter ~f:k' @@ Pattern.rewrite_root rule term;
              Option.iter ~f:k' @@ Pattern.rewrite_root (Rule.flip rule) term)

      method dedup_states states =
        states
        |> List.filter ~f:(fun (s, _, _) -> not (Search_state.mem search_state s))
        |> List.dedup_and_sort ~compare:(fun (s, _, _) (s', _, _) -> [%compare: Value.t] s s')

      method search_close_states new_states =
        with_time _search_close_states_time @@ fun () ->
        let search_states = List.filter new_states ~f:(fun (v, _, _) -> Float.(Cad_conc.jaccard output v < 0.05)) in
        Fmt.epr "Searching %d/%d neighborhoods\n%!" (List.length search_states) (List.length new_states);

        List.iter search_states ~f:(fun (_, op, args) ->
            let center = Search_state.program_of_op_args_exn search_state op args in
            self#local_search ~n:search_width ~target:output center @@ fun (p, _) ->
            if [%compare.equal: Value.t] (Program.eval (Value.eval eval_ctx) p) output then raise @@ Parent.Done p)

      method sample_diverse_states cost new_states =
        with_time _sample_states_time @@ fun () ->
        let width = sample_width.(cost) in
        if width > 0 then (
          let new_states_a = Array.of_list new_states in
          let classes =
            List.mapi new_states ~f:(fun i ((v, _, _) as x) -> (v, (x, Union_find.create i)))
            |> Hashtbl.of_alist_exn (module Value)
          in
          let module F = Flat_program.Make (Op) in
          Hashtbl.iter classes ~f:(fun ((_, op, args), class_) ->
              let p = Search_state.program_of_op_args_exn search_state op args in
              self#local_search_diverse ~n:width p (fun p ->
                  Program.eval (Value.eval eval_ctx) p
                  |> Hashtbl.find classes
                  |> Option.iter ~f:(fun (_, class_') -> Union_find.union class_ class_')));

          let to_keep =
            Hashtbl.data classes |> List.map ~f:Tuple.T2.get2 |> List.map ~f:Union_find.get
            |> List.dedup_and_sort ~compare
          in
          Fmt.epr "Retained %d/%d new states\n%!" (List.length to_keep) (List.length new_states);
          List.map to_keep ~f:(fun i -> new_states_a.(i)))
        else new_states

      method! generate_states cost =
        let new_states = super#generate_states cost |> self#dedup_states in
        self#search_close_states new_states;
        self#sample_diverse_states cost new_states

      method! run =
        let rec reduce_sample_width () =
          match super#run with
          | Some p -> Some p
          | None ->
              Search_state.clear search_state;
              let idx, _ = Array.findi_exn sample_width ~f:(fun _ v -> v > 0) in
              sample_width.(idx) <- 0;
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
