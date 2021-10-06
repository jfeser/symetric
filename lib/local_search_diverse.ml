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
    type t [@@deriving compare, equal, hash, sexp]

    module Ctx : sig
      type t

      val of_params : Params.t -> t
    end

    include Comparator.S with type t := t

    val is_error : t -> bool

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
  module Lang = struct
    include Lang
    module Value = Memoized_value.Make_cached (Op) (Value)
  end

  open Lang
  module Search_state = Search_state_all.Make (Lang)
  module Gen = Generate.Gen_list (Lang)

  let with_time_probe t f =
    let start_time = Time.now () in
    let ret = f () in
    let end_time = Time.now () in
    (t := Time.Span.(!t + Time.diff end_time start_time));
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
      normalize : (Op.t Program.t -> Op.t Program.t) option;
      distance : Value.t -> Value.t -> float;
      search_close_states_time : Time.Span.t ref;
      sample_states_time : Time.Span.t ref;
      output : Value.t;
      ectx : Value.Ctx.t;
      max_cost : int;
      verbose : bool;
      ops : Op.t list;
      bank_size : float ref;
      found_program : bool ref;
      program_cost : float ref;
      states_grouped : int ref;
      groups_created : int ref;
    }

    let create ?(search_width = 10) ?(verbose = false) ?stats ?normalize ~search_thresh ~rules ~distance
        ?(max_cost = 64) ectx ops output =
      let stats = Option.value_lazy stats ~default:(lazy (Stats.create ())) in

      {
        search_width;
        search_thresh;
        rules = Local_search.Rule.normalize [%compare: Op.t] rules;
        normalize;
        distance = (fun v v' -> distance (Value.value v) (Value.value v'));
        search_close_states_time = ref Time.Span.zero;
        sample_states_time = ref Time.Span.zero;
        output = Value.create output;
        ectx;
        max_cost;
        verbose;
        ops;
        bank_size = Stats.add_probe_exn stats "bank-size";
        program_cost = Stats.add_probe_exn stats "program-cost";
        found_program = ref false;
        states_grouped = ref 0;
        groups_created = ref 0;
      }
  end

  module Op_program = struct
    type t = Op.t Program.t [@@deriving compare, hash, sexp]
  end

  module Term_set = struct
    type op = Op.t

    type t = {
      search_state : Search_state.t; [@ignore]
      value : [ `List of (Value.t * Op.t * Value.t list) list | `Ref of Search_state.TValue.t ];
    }
    [@@deriving compare, sexp]

    let heads ({ value; search_state } as ts) =
      let of_op_args =
        Iter.map (fun (op, args) ->
            (op, List.map2_exn (Op.args_type op) args ~f:(fun type_ value -> { ts with value = `Ref { type_; value } })))
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

  module Subst = struct
    type k = int [@@deriving sexp]

    type v = Term_set.t [@@deriving sexp]

    type t = v Map.M(Int).t [@@deriving sexp]

    let get = Map.find

    let set t k v = Map.set t ~key:k ~data:v

    let empty = Map.empty (module Int)
  end

  module HCons = struct
    module Term = struct
      type t = [ `Value of Value.t | `Apply of Op.t * int list ] [@@deriving compare, hash]
    end

    type t = { classes : int Hashtbl.M(Term).t; mutable class_id : int }

    let rec id_of cons = function
      | `Apply (op, args) -> (
          let norm_app = `Apply (op, List.map ~f:(id_of cons) args) in
          match Hashtbl.find cons.classes norm_app with
          | Some id -> id
          | None ->
              let id = cons.class_id in
              cons.class_id <- id + 1;
              Hashtbl.set cons.classes ~key:norm_app ~data:id;
              id)
      | `Value v -> (
          match Hashtbl.find cons.classes (`Value v) with
          | Some id -> id
          | None ->
              let id = cons.class_id in
              cons.class_id <- id + 1;
              Hashtbl.set cons.classes ~key:(`Value v) ~data:id;
              id)
  end

  class synthesizer (ctx : Ctx.t) =
    object (self)
      val mutable enum_bound = 0

      val mutable upper_bound = 0

      val search_state = Search_state.create ctx.max_cost

      method local_search ~target =
        Local_search.of_rules_tabu ~target ~dist:ctx.distance
          (module Op)
          (module Value)
          ctx.rules
          (Program.eval (Value.eval ctx.ectx))

      method apply_rules states =
        let eval_subst_value s p =
          let rec eval_subst = function
            | Local_search.Pattern.Apply (op, args) -> Value.eval ctx.ectx op @@ List.map args ~f:eval_subst
            | Var v -> (
                match Subst.get s v with
                | Some term_set -> Term_set.value_exn term_set
                | None ->
                    raise_s [%message "no value for binding" (v : int) (s : Subst.t) (p : Op.t Local_search.Pattern.t)])
          in
          eval_subst p
        in

        let _eval_subst_term s p =
          let rec eval_subst = function
            | Local_search.Pattern.Apply (op, args) -> `Apply (op, List.map args ~f:eval_subst)
            | Var v -> (
                match Subst.get s v with
                | Some term_set -> `Value (Term_set.value_exn term_set)
                | None ->
                    raise_s [%message "no value for binding" (v : int) (s : Subst.t) (p : Op.t Local_search.Pattern.t)])
          in
          eval_subst p
        in

        Iter.of_list states
        |> Iter.map (fun state ->
               Iter.of_list ctx.rules
               |> Iter.map (fun (lhs, rhs) ->
                      let term_set = Term_set.of_states search_state [ state ] in

                      (* print_s *)
                      (*   [%message *)
                      (*     "matching" (state : Value.t * Op.t * Value.t list) ((lhs, rhs) : Op.t Local_search.Rule.t)]; *)

                      (* print_s [%message "args" ((lhs, term_set) : Op.t Local_search.Pattern.t * Term_set.t)]; *)
                      Local_search.Pattern.match_ (module Op) (module Term_set) (module Subst) lhs term_set
                      |> Iter.map (fun s -> (* print_s [%message "found match" (s : Subst.t)]; *)
                                            eval_subst_value s rhs))
               |> Iter.concat
               |> Iter.map (fun v -> (state, v)))
        |> Iter.concat

      method search_close_states new_states =
        let top_k k new_states =
          Iter.of_list new_states
          |> Iter.map (fun ((v, _, _) as s) -> (-.ctx.distance ctx.output v, s))
          |> Iter.top_k ~cmp:[%compare: float * _] k
        in

        with_time_probe ctx.search_close_states_time @@ fun () ->
        let search_states =
          match ctx.search_thresh with
          | Search_thresh.Distance thresh ->
              Iter.of_list new_states
              |> Iter.filter_map (fun ((v, _, _) as s) ->
                     let dist = ctx.distance ctx.output v in
                     if Float.(dist < thresh) then Some (dist, s) else None)
          | Top_k k -> top_k k new_states
          | Top_frac p ->
              let k = Float.(to_int (p *. of_int (List.length new_states))) in
              top_k k new_states
        in
        let search_states = search_states |> Iter.filter (fun (d, _) -> Float.(d < infinity)) in

        let examined = ref 0 in
        let solution =
          search_states
          |> Iter.find_map (fun (_, (_, op, args)) ->
                 let center = Search_state.random_program_of_op_args_exn search_state op args in
                 self#local_search ~target:ctx.output center
                 |> Iter.take ctx.search_width
                 |> Iter.find_mapi (fun step p ->
                        incr examined;
                        if [%compare.equal: Value.t] (Program.eval (Value.eval ctx.ectx) p) ctx.output then (
                          eprint_s [%message "local search" (step : int)];
                          Option.return p)
                        else None))
        in
        (solution, !examined)

      (* method group_states cost new_states = *)
      (*   let module TValue = struct *)
      (*     type t = Value.t * Type.t [@@deriving compare, hash, sexp_of] *)
      (*   end in *)
      (*   let module Term = struct *)
      (*     type t = [ `Value of Value.t | `Apply of Op.t * t list ] [@@deriving compare, hash, sexp_of] *)
      (*   end in *)
      (*   with_time_probe ctx.sample_states_time @@ fun () -> *)
      (*   if cost > enum_bound then *)
      (*     let term_ids = Hashtbl.create (module Term) in *)
      (*     let term_id_ctr = ref 0 in *)

      (*     List.mapi new_states ~f:(fun id state -> *)
      (*         let state_id = Union_find.create id in *)
      (*         self#apply_rules [ state ] *)
      (*         |> Iter.iter (fun (_, term) -> *)
      (*                match Hashtbl.find term_ids term with *)
      (*                | Some term_id -> Union_find.union state_id term_id *)
      (*                | None -> *)
      (*                    let term_id = Union_find.create !term_id_ctr in *)
      (*                    incr term_id_ctr; *)
      (*                    Hashtbl.set term_ids ~key:term ~data:term_id); *)
      (*         (state_id, state)) *)
      (*     |> List.map ~f:(fun (id, state) -> (Union_find.get id, state)) *)
      (*     |> List.group_by (module Int) *)
      (*     |> List.map ~f:Tuple.T2.get2 *)
      (*   else List.map ~f:(fun s -> [ s ]) new_states *)

      method group_states cost new_states =
        let module TValue = struct
          type t = Value.t * Type.t [@@deriving compare, hash, sexp_of]
        end in
        with_time_probe ctx.sample_states_time @@ fun () ->
        ctx.states_grouped := !(ctx.states_grouped) + List.length new_states;
        let groups =
          if cost > enum_bound then (
            match ctx.normalize with
            | Some normalize ->
                let groups = Hashtbl.create (module Op_program) in
                List.iter new_states ~f:(fun ((_, op, args) as state) ->
                    let prog = Search_state.program_of_op_args_exn search_state op args in
                    let nprog = normalize prog in
                    Hashtbl.update groups nprog ~f:(function None -> [ state ] | Some xs -> state :: xs));
                Hashtbl.data groups
            | None ->
                let classes =
                  List.map new_states ~f:(fun ((v, op, _) as state) -> ((v, Op.ret_type op), state))
                  |> Hashtbl.of_alist_multi (module TValue)
                in
                let classes =
                  let ctr = ref 0 in
                  Hashtbl.map classes ~f:(fun states ->
                      incr ctr;
                      (states, Union_find.create !ctr, ref false))
                in

                self#apply_rules new_states
                |> Iter.iter (fun ((v, op, _), alt_value) ->
                       let t = Op.ret_type op in
                       let key = (v, t) and alt_key = (alt_value, t) in
                       (* print_s [%message (v : Value.t) (alt_value : Value.t)]; *)
                       (* if Search_state.(mem search_state TValue.{ type_ = t; value = alt_value }) then *)
                       (*   Option.iter ~f:(fun (_, _, dead) -> dead := true) @@ Hashtbl.find classes key *)
                       (* else *)
                       match (Hashtbl.find classes key, Hashtbl.find classes alt_key) with
                       | Some (_, c, _), Some (_, c', _) -> Union_find.union c c'
                       | _ -> ());

                let to_keep = Hashtbl.create (module Int) in
                Hashtbl.iter classes ~f:(fun (states, class_id, _) ->
                    (* if !dead then print_s [%message "dead" (states : (Value.t * _ * _) list)]; *)
                    (* if not !dead then *)
                    Hashtbl.update to_keep (Union_find.get class_id) ~f:(function
                      | Some states' -> states @ states'
                      | None -> states));
                Hashtbl.data to_keep)
          else List.map ~f:(fun s -> [ s ]) new_states
        in
        ctx.groups_created := !(ctx.groups_created) + List.length groups;
        (* print_s [%message (List.take groups 10 : (Value.t * _ * _) list list)]; *)
        groups

      method fill cost =
        match self#generate_states cost with
        | First solution -> Some solution
        | Second new_states ->
            self#insert_states_ cost new_states;
            if ctx.verbose then (
              Fmt.epr "Finished cost %d\n%!" cost;
              Search_state.print_stats search_state);
            None

      method insert_states_ cost states =
        Search_state.insert_groups search_state cost states;
        ctx.bank_size := Float.of_int @@ Search_state.length search_state

      method check_states states =
        List.find_map states ~f:(fun (s, op, args) ->
            if [%compare.equal: Value.t] ctx.output s then
              Option.return @@ Search_state.program_of_op_args_exn search_state op args
            else None)

      method generate_states cost : _ Either.t =
        if ctx.verbose then Fmt.epr "Generating states of cost %d...\n%!" cost;
        let new_states, gen_time =
          with_time (fun () -> Gen.generate_states Search_state.search ctx.ectx search_state ctx.ops cost)
        in
        if ctx.verbose then Fmt.epr "Generated %d states in %a.\n%!" (List.length new_states) Time.Span.pp gen_time;

        match self#check_states new_states with
        | Some solution -> First solution
        | None ->
            if cost < upper_bound then (
              let (solution, n_searched), search_time = with_time (fun () -> self#search_close_states new_states) in
              match solution with
              | Some solution -> First solution
              | None ->
                  if ctx.verbose then Fmt.epr "Searched %d close states in %a.\n%!" n_searched Time.Span.pp search_time;

                  let groups, group_time = with_time (fun () -> self#group_states cost new_states) in
                  if ctx.verbose then
                    Fmt.epr "Created %d groups in %a.\n%!" (List.length groups) Time.Span.pp group_time;
                  Second groups)
            else Second []

      method run =
        let solution =
          Synth_utils.luby_cutoff 2.0 16
          |> Iter.find_map (fun upper ->
                 let bound = Float.to_int upper in
                 let scaled_bound = bound in
                 upper_bound <- scaled_bound;
                 enum_bound <- 0;

                 Search_state.clear search_state;

                 ctx.sample_states_time := Time.Span.zero;
                 ctx.search_close_states_time := Time.Span.zero;
                 ctx.groups_created := 0;
                 ctx.states_grouped := 0;

                 let ret, time =
                   with_time (fun () -> Iter.(0 -- upper_bound) |> Iter.find_map (fun cost -> self#fill cost))
                 in
                 if ctx.verbose then
                   Fmt.epr "Completed iteration in %a. (search %a) (grouping %a %d/%d)\n%!" Time.Span.pp time
                     Time.Span.pp !(ctx.search_close_states_time) Time.Span.pp !(ctx.sample_states_time)
                     !(ctx.groups_created) !(ctx.states_grouped);
                 ret)
        in
        Option.iter solution ~f:(fun p ->
            ctx.found_program := true;
            ctx.program_cost := Float.of_int @@ Program.size p);
        solution
    end

  let synth params = Option.iter (new synthesizer params)#run ~f:(fun p -> eprint_s [%message (p : Op.t Program.t)])
end
