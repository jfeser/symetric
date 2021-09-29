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

module type Lang_intf = sig
  module Type : sig
    type t [@@deriving compare, hash, sexp]

    include Comparator.S with type t := t

    val output : t
  end

  module Op : Op_intf.S with type type_ = Type.t

  module Value : sig
    type t [@@deriving compare, equal, hash, sexp_of]

    module Ctx : sig
      type t

      val of_params : Params.t -> t
    end

    include Comparator.S with type t := t

    val eval : Ctx.t -> Op.t -> t list -> t

    val dist : Ctx.t -> t -> t -> float
  end

  module Bench : sig
    type t [@@deriving of_sexp]

    val ops : t -> Op.t list

    val output : t -> Value.t

    val solution_exn : t -> Op.t Program.t

    val load : string -> t

    val save : string -> t -> unit
  end

  val name : string

  val bench : (Bench.t, Dumb_params.Param.bound) Dumb_params.Param.t

  val spec : Dumb_params.Spec.t
end

module Make (Lang : Lang_intf) = struct
  open Lang
  module Parent = Baseline.Make (Lang)
  module Search_state = Parent.Search_state

  let with_time_probe t f =
    let start_time = Time.now () in
    let ret = f () in
    let end_time = Time.now () in
    (t := !t +. Time.Span.(to_ms Time.(diff end_time start_time)));
    ret

  let with_time f =
    let start_time = Time.now () in
    let ret = f () in
    let end_time = Time.now () in
    (ret, Time.diff end_time start_time)

  module Ctx = struct
    type t = {
      search_width : int;
      search_thresh : Search_thresh.t;
      rules : Op.t Local_search.Rule.t list;
      distance : Value.t -> Value.t -> float;
      parent_ctx : Parent.Ctx.t;
      search_close_states_time : float ref;
      sample_states_time : float ref;
      output : Value.t;
    }

    let create ?(search_width = 10) ?(verbose = false) ?stats ~search_thresh ~rules ~distance ~max_cost ectx ops output
        =
      let stats = Option.value_lazy stats ~default:(lazy (Stats.create ())) in
      let parent_ctx = Parent.Ctx.create ~stats ~verbose ~max_cost ectx ops (`Value output) in

      {
        search_width;
        search_thresh;
        rules = Local_search.Rule.normalize [%compare: Op.t] rules;
        distance;
        parent_ctx;
        search_close_states_time = Stats.add_probe_exn stats "search-close-states-time";
        sample_states_time = Stats.add_probe_exn stats "sample-states-time";
        output;
      }
  end

  module Term_set = struct
    type op = Op.t

    type t = {
      search_state : (Search_state.t[@sexp.opaque]); [@ignore]
      value : [ `List of (Value.t * Op.t * Value.t list) list | `Ref of Search_state.TValue.t ];
    }
    [@@deriving compare, sexp_of]

    let heads ({ value; search_state } as ts) =
      let of_op_args =
        Iter.map (fun (op, args) ->
            let type_ = Op.ret_type op in
            (op, List.map args ~f:(fun value -> { ts with value = `Ref { type_; value } })))
      in

      match value with
      | `List vs -> Iter.of_list vs |> Iter.map (fun (_, o, a) -> (o, a)) |> of_op_args
      | `Ref tv -> (
          match Hashtbl.find search_state.paths tv with
          | Some terms -> Iter.of_queue terms |> Iter.map (fun (_, o, a) -> (o, a)) |> of_op_args
          | None -> Iter.empty)

    let of_states search_state states = { search_state; value = `List states }

    let value_exn = function { value = `List _; _ } -> failwith "no value" | { value = `Ref tv; _ } -> tv.value
  end

  class synthesizer (ctx : Ctx.t) =
    object (self)
      inherit Parent.synthesizer ctx.parent_ctx as super

      val output = ctx.output

      val sample_width =
        let arr = Array.create ~len:(ctx.parent_ctx.max_cost + 1) 2 in
        arr.(0) <- 0;
        arr

      method local_search ~target =
        Local_search.of_rules_tabu ~target ~dist:ctx.distance
          (module Op)
          ctx.rules
          (Program.eval (Value.eval ctx.parent_ctx.ectx))

      method apply_rules states =
        let module Subst = struct
          type k = int [@@deriving sexp]

          type v = Term_set.t [@@deriving sexp_of]

          type t = v Map.M(Int).t [@@deriving sexp_of]

          let get = Map.find

          let set t k v = Map.set t ~key:k ~data:v

          let empty = Map.empty (module Int)
        end in
        let eval_subst s p =
          let rec eval_subst = function
            | Local_search.Pattern.Apply (op, args) -> Value.eval ctx.parent_ctx.ectx op @@ List.map args ~f:eval_subst
            | Var v -> (
                match Subst.get s v with
                | Some term_set -> Term_set.value_exn term_set
                | None ->
                    raise_s
                      [%message "no value for binding" (v : int) (s : Subst.t) (p : (Op.t, int) Local_search.Pattern.t)]
                )
          in
          eval_subst p
        in

        Iter.of_list states
        |> Iter.map (fun state ->
               Iter.of_list ctx.rules
               |> Iter.map (fun (lhs, rhs) ->
                      Local_search.Pattern.match_
                        (module Op)
                        (module Term_set)
                        (module Subst)
                        lhs
                        (Term_set.of_states search_state [ state ])
                      |> Iter.map (fun s -> eval_subst s rhs))
               |> Iter.concat
               |> Iter.map (fun v -> (state, v)))
        |> Iter.concat

      method search_close_states new_states =
        let top_k k =
          let sorted_states =
            List.map new_states ~f:(fun ((v, _, _) as s) -> (ctx.distance output v, s))
            |> List.sort ~compare:(fun (d, _) (d', _) -> [%compare: float] d d')
          in
          List.take sorted_states k |> List.map ~f:Tuple.T2.get2
        in

        with_time_probe ctx.search_close_states_time @@ fun () ->
        let search_states =
          match ctx.search_thresh with
          | Search_thresh.Distance d -> List.filter new_states ~f:(fun (v, _, _) -> Float.(ctx.distance output v < d))
          | Top_k k -> top_k k
          | Top_frac p ->
              let k = Float.(to_int (p *. of_int (List.length new_states))) in
              top_k k
        in

        List.iter search_states ~f:(fun (_, op, args) ->
            let center = Search_state.program_of_op_args_exn search_state op args in
            self#local_search ~target:output center |> Iter.take ctx.search_width
            |> Iter.iter (fun p ->
                   if [%compare.equal: Value.t] (Program.eval (Value.eval eval_ctx) p) output then
                     raise @@ Parent.Done p));
        List.length search_states

      method group_states cost new_states =
        let module TValue = struct
          type t = Value.t * Type.t [@@deriving compare, hash, sexp_of]
        end in
        with_time_probe ctx.sample_states_time @@ fun () ->
        let width = sample_width.(cost) in
        if width > 0 then (
          let classes =
            List.map new_states ~f:(fun ((v, op, _) as state) -> ((v, Op.ret_type op), state))
            |> Hashtbl.of_alist_multi (module TValue)
          in
          let classes =
            let ctr = ref 0 in
            Hashtbl.map classes ~f:(fun states ->
                incr ctr;
                (states, Union_find.create !ctr))
          in

          self#apply_rules new_states
          |> Iter.iter (fun ((v, op, _), alt_value) ->
                 let t = Op.ret_type op in
                 let key = (v, t) and alt_key = (alt_value, t) in
                 match (Hashtbl.find classes key, Hashtbl.find classes alt_key) with
                 | Some (_, c), Some (_, c') -> Union_find.union c c'
                 | _ -> ());

          let to_keep = Hashtbl.create (module Int) in
          Hashtbl.iter classes ~f:(fun (states, class_id) ->
              Hashtbl.update to_keep (Union_find.get class_id) ~f:(function
                | Some states' -> states @ states'
                | None -> states));
          Hashtbl.data to_keep)
        else List.map ~f:(fun s -> [ s ]) new_states

      method! fill cost =
        let new_states = self#generate_states_ cost in
        List.iter new_states ~f:super#check_states;
        self#insert_states_ cost new_states;
        if verbose then (
          Fmt.epr "Finished cost %d\n%!" cost;
          Search_state.print_stats search_state)

      method insert_states_ cost states =
        Search_state.insert_groups search_state cost states;
        ctx.parent_ctx.bank_size := Float.of_int @@ Search_state.length search_state

      method generate_states_ cost =
        Fmt.epr "Generating states of cost %d...\n%!" cost;
        let new_states, gen_time = with_time (fun () -> super#generate_states cost) in
        Fmt.epr "Generated %d states in %a.\n%!" (List.length new_states) Time.Span.pp gen_time;

        let n_searched, search_time = with_time (fun () -> self#search_close_states new_states) in
        Fmt.epr "Searched %d close states in %a.\n%!" n_searched Time.Span.pp search_time;

        let groups, group_time = with_time (fun () -> self#group_states cost new_states) in
        Fmt.epr "Created %d groups in %a.\n%!" (List.length groups) Time.Span.pp group_time;
        groups

      method! run =
        let rec reduce_sample_width () =
          let ret, time = with_time (fun () -> super#run) in
          Fmt.epr "Completed iteration in %a.\n%!" Time.Span.pp time;

          match ret with
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
