open Std

include struct
  open Dumb_params

  let spec = Spec.inherit_ Baseline.spec "local-search-diverse"

  let search_close_states_time =
    Spec.add spec @@ Param.(mut @@ float ~name:"search-close-states-time" ~doc:"" ~init:(`Default (Fun.const 0.0)) ())

  let sample_states_time =
    Spec.add spec @@ Param.(mut @@ float ~name:"sample-states-time" ~doc:"" ~init:(`Default (Fun.const 0.0)) ())

  let rules_fn = Spec.add spec @@ Param.string ~name:"rules-file" ~init:(`Cli (Some "")) ~doc:"" ()

  let rule_sets = Spec.add spec @@ Param.string ~name:"rule-sets" ~init:(`Cli (Some "")) ~doc:"" ()

  let distance = Spec.add spec @@ Param.string ~name:"distance" ~init:(`Cli (Some "jaccard")) ~doc:"" ()

  let search_thresh =
    Spec.add spec @@ Param.string ~name:"search-thresh" ~init:(`Cli (Some "(Distance 0.01)")) ~doc:"" ()

  let (_ : _) = Spec.add spec @@ Param.const_str ~name:"synth" "local-search-diverse"
end

let sum_to arr x =
  let sum = ref 0 in
  for i = 0 to x do
    sum := !sum + arr.(i)
  done;
  !sum

module Search_thresh = struct
  type t = Distance of float | Top_k of int | Top_frac of float [@@deriving sexp]
end

module Make (Lang : Lang_intf.S) = struct
  open Lang
  module Parent = Baseline.Make (Lang)
  module Search_state = Parent.Search_state

  let with_time t f =
    let start_time = Time.now () in
    let ret = f () in
    let end_time = Time.now () in
    (t := !t +. Time.Span.(to_ms Time.(diff end_time start_time)));
    ret

  module Ctx = struct
    type t = {
      search_width : int;
      search_thresh : Search_thresh.t;
      rules : Op.t Local_search.Rule.t list;
      distance : Value.t -> Value.t -> float;
      parent_ctx : Parent.Ctx.t;
      search_close_states_time : float ref;
      sample_states_time : float ref;
    }

    let create ?(search_width = 10) ?(verbose = false) ?stats ~search_thresh ~rules ~distance ~max_cost ectx ops output
        =
      let stats = Option.value_lazy stats ~default:(lazy (Stats.create ())) in
      let parent_ctx = Parent.Ctx.create ~stats ~verbose ~max_cost ectx ops output in
      {
        search_width;
        search_thresh;
        rules;
        distance;
        parent_ctx;
        search_close_states_time = Stats.add_probe_exn stats "search-close-states-time";
        sample_states_time = Stats.add_probe_exn stats "sample-states-time";
      }
  end

  class synthesizer (ctx : Ctx.t) =
    object (self)
      inherit Parent.synthesizer ctx.parent_ctx as super

      val sample_width =
        let arr = Array.create ~len:(ctx.parent_ctx.max_cost + 1) 2 in
        arr.(0) <- 0;
        arr

      method local_search ~target =
        Local_search.of_rules_tabu ~target ~dist:ctx.distance
          (module Op)
          ctx.rules
          (Program.eval (Value.eval ctx.parent_ctx.ectx))

      method local_search_diverse ~n term k =
        let k' t' =
          k t';
          self#local_search_diverse ~n:(n - 1) term k
        in
        if n > 0 then
          let open Local_search in
          List.iter ctx.rules ~f:(fun rule ->
              Option.iter ~f:k' @@ Pattern.rewrite_root (module Op) rule term;
              Option.iter ~f:k' @@ Pattern.rewrite_root (module Op) (Rule.flip rule) term)

      method dedup_states states =
        let module S = Search_state in
        states
        |> List.map ~f:(fun ((value, op, _) as x) -> (S.TValue.{ type_ = Op.ret_type op; value }, x))
        |> List.filter ~f:(fun (tval, _) -> not (S.mem search_state tval))
        |> List.dedup_and_sort ~compare:(fun (tval, _) (tval', _) -> [%compare: S.TValue.t] tval tval')
        |> List.map ~f:(fun (_, x) -> x)

      method search_close_states new_states =
        let top_k k =
          let sorted_states =
            List.map new_states ~f:(fun ((v, _, _) as s) -> (ctx.distance output v, s))
            |> List.sort ~compare:(fun (d, _) (d', _) -> [%compare: float] d d')
          in
          List.take sorted_states k |> List.map ~f:Tuple.T2.get2
        in

        with_time ctx.search_close_states_time @@ fun () ->
        let search_states =
          match ctx.search_thresh with
          | Search_thresh.Distance d -> List.filter new_states ~f:(fun (v, _, _) -> Float.(ctx.distance output v < d))
          | Top_k k -> top_k k
          | Top_frac p ->
              let k = Float.(to_int (p *. of_int (List.length new_states))) in
              top_k k
        in
        Fmt.epr "Searching %d/%d neighborhoods\n%!" (List.length search_states) (List.length new_states);

        List.iter search_states ~f:(fun (_, op, args) ->
            let center = Search_state.program_of_op_args_exn search_state op args in
            self#local_search ~target:output center |> Iter.take ctx.search_width
            |> Iter.iter (fun p ->
                   if [%compare.equal: Value.t] (Program.eval (Value.eval eval_ctx) p) output then
                     raise @@ Parent.Done p))

      method sample_diverse_states cost new_states =
        let module TValue = struct
          type t = Value.t * Type.t [@@deriving compare, hash, sexp_of]
        end in
        with_time ctx.sample_states_time @@ fun () ->
        let width = sample_width.(cost) in
        if width > 0 then (
          let new_states_a = Array.of_list new_states in
          let classes =
            List.mapi new_states ~f:(fun i ((v, op, _) as x) -> ((v, Op.ret_type op), (x, Union_find.create i)))
            |> Hashtbl.of_alist_exn (module TValue)
          in
          let module F = Flat_program.Make (Op) in
          Hashtbl.iter classes ~f:(fun ((_, op, args), class_) ->
              let p = Search_state.program_of_op_args_exn search_state op args in
              self#local_search_diverse ~n:width p (fun p ->
                  let v = Program.eval (Value.eval eval_ctx) p in
                  let (Apply (op, _)) = p in
                  let t = Op.ret_type op in

                  Hashtbl.find classes (v, t) |> Option.iter ~f:(fun (_, class_') -> Union_find.union class_ class_')));

          let to_keep =
            Hashtbl.data classes |> List.map ~f:Tuple.T2.get2 |> List.map ~f:Union_find.get
            |> List.dedup_and_sort ~compare
          in
          Fmt.epr "Retained %d/%d new states\n%!" (List.length to_keep) (List.length new_states);
          let kept_states = List.map to_keep ~f:(fun i -> new_states_a.(i)) in
          print_s [%message (List.map ~f:(fun (_, op, _) -> op) kept_states : Op.t list)];
          kept_states)
        else new_states

      method! generate_states cost =
        let new_states = super#generate_states cost in
        super#check_states new_states;
        let new_states = self#dedup_states new_states in
        self#search_close_states new_states;
        let new_states' = self#sample_diverse_states cost new_states in
        new_states'

      method! run =
        let rec reduce_sample_width () =
          match super#run with
          | Some p -> Some p
          | None ->
              Search_state.clear search_state;
              let idx, _ = Array.findi_exn sample_width ~f:(fun _ v -> v > 0) in
              sample_width.(idx) <- sample_width.(idx) - 1;
              reduce_sample_width ()
        in
        reduce_sample_width ()
    end

  let synth params = Option.iter (new synthesizer params)#run ~f:(fun p -> eprint_s [%message (p : Op.t Program.t)])
end
