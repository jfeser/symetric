open Std

module Search_thresh = struct
  type t = Distance of float | Top_k of int | Top_frac of float [@@deriving sexp]
end

module type Lang_intf = sig
  module Type : sig
    type t [@@deriving compare, hash, sexp]

    val output : t
  end

  module Op : sig
    type t [@@deriving compare, hash, sexp]

    val pp : t Fmt.t
    val cost : t -> int
    val arity : t -> int
    val args_type : t -> Type.t list
    val ret_type : t -> Type.t
    val is_commutative : t -> bool
  end

  module Value : sig
    type t [@@deriving compare, hash, sexp]

    module Ctx : sig
      type t
    end

    val pp : Ctx.t -> t Fmt.t
    val is_error : t -> bool
    val eval : Ctx.t -> Op.t -> t list -> t
  end
end

module Make (Lang : Lang_intf) = struct
  module Value0 = Lang.Value

  module Lang = struct
    include Lang

    module Op = struct
      include Op
      include Comparator.Make (Op)
    end
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
      unnormalize : Op.t Program.t -> Op.t Program.t list;
      normalize : Op.t Program.t -> Op.t Program.t;
      distance : Value.t -> Value.t -> float;
      search_close_states_time : Time.Span.t ref;
      sample_states_time : Time.Span.t ref;
      output : Value.t;
      ectx : Value.Ctx.t;
      verbose : bool;
      ops : Op.t list;
      bank_size : float ref;
      found_program : bool ref;
      program_cost : float ref;
      states_grouped : int ref;
      groups_created : int ref;
      on_close_state : Op.t Program.t -> Value0.t -> unit;
      on_existing : Value0.t -> Value0.t -> unit;
      after_local_search : Op.t Program.t -> Value0.t -> unit;
      on_groups : (Value0.t * Op.t * Value.t list) list list -> unit;
      find_term : (Op.t Program.t * ((Op.t * Value0.t list) Program.t -> unit)) option;
      tabu_length : int;
      thresh : float;
      best : (float * Value.t option) ref;
    }

    let create ?(on_close_state = fun _ _ -> ()) ?(after_local_search = fun _ _ -> ()) ?(on_groups = fun _ -> ())
        ?find_term ?(on_existing = fun _ _ -> ()) ?(search_width = 10) ?(tabu_length = 1000) ?(verbose = false) ?stats
        ?(unnormalize = fun _ -> []) ?(normalize = Fun.id) ~search_thresh ~distance ?(thresh = 0.1) ectx ops output =
      let stats = Option.value_lazy stats ~default:(lazy (Stats.create ())) in

      {
        search_width;
        search_thresh;
        unnormalize;
        normalize;
        distance;
        search_close_states_time = ref Time.Span.zero;
        sample_states_time = ref Time.Span.zero;
        output;
        ectx;
        verbose;
        ops;
        bank_size = Stats.add_probe_exn stats "bank-size";
        program_cost = Stats.add_probe_exn stats "program-cost";
        found_program = ref false;
        states_grouped = ref 0;
        groups_created = ref 0;
        on_close_state;
        on_existing;
        after_local_search;
        on_groups;
        tabu_length;
        thresh;
        find_term;
        best = ref (Float.infinity, None);
      }
  end

  class synthesizer (ctx : Ctx.t) =
    object (self)
      val mutable upper_bound = 0
      val close_states = Hashtbl.create (module Value)
      val search_state = Search_state.create ()
      method get_search_state = search_state
      method local_search ~target size value op args = failwith "unimplemented"

      (* Local_search.of_unnormalize_tabu ~max_tabu:ctx.tabu_length ~target ~dist:ctx.distance *)
      (*   (module Op) *)
      (*   (module Value) *)
      (*   ctx.unnormalize *)
      (*   (Program.eval (Value.eval ctx.ectx)) *)

      method search_close_states size new_states =
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
          |> Iter.filter (fun (_, (_, op, _)) -> [%compare.equal: Type.t] Type.output (Op.ret_type op))
          |> Iter.find_map (fun (_, (value, op, args)) ->
                 let center = Search_state.random_program_of_op_args_exn search_state (Int.ceil_log2 size) op args in
                 ctx.on_close_state center value;

                 let last_state = ref None in
                 let m_solution =
                   self#local_search ~target:ctx.output size value op args
                   |> Iter.take ctx.search_width
                   |> Iter.find_mapi (fun step p ->
                          incr examined;
                          let value = Program.eval (Value.eval ctx.ectx) p in

                          ctx.on_close_state p value;
                          last_state := Some (p, value);
                          if [%compare.equal: Value.t] value ctx.output then (
                            eprint_s [%message "local search" (step : int)];
                            Option.return p)
                          else None)
                 in
                 Option.iter !last_state ~f:(fun (p, v) -> ctx.after_local_search p v);
                 m_solution)
        in
        (solution, !examined)

      method fill cost =
        let new_states = self#generate_states cost in
        if ctx.verbose then Fmt.epr "Inserting %d states...\n%!" @@ List.length new_states;

        (* Group new states by type and insert *)
        List.map new_states ~f:(fun ((_, op, _) as state) -> (Op.ret_type op, state))
        |> List.group_by (module Type)
        |> List.iter ~f:(fun (type_, states) ->
               let new_states, time = with_time (fun () -> self#insert_states_ cost type_ new_states) in
               if ctx.verbose then
                 Fmt.epr "Inserted %d %a states in %a.\n%!" (List.length new_states) Sexp.pp
                   ([%sexp_of: Type.t] type_)
                   Time.Span.pp time);

        Option.iter ctx.find_term ~f:(fun (t, f) ->
            Fmt.epr "Looking for close terms...\n%!";
            Search_state.find_term search_state t |> f);

        let solution, n_searched = self#search_close_states cost new_states in
        if ctx.verbose then Fmt.epr "Searched %d close states.\n%!" n_searched;
        Option.iter solution ~f:(fun _ -> failwith "found solution");

        if ctx.verbose then (
          Fmt.epr "Finished cost %d\n%!" cost;
          Search_state.print_stats search_state)

      method insert_states_ cost type_ states =
        let states =
          List.map states ~f:(fun ((v, _, _) as x) -> (v, x))
          |> List.group_by (module Value)
          |> List.filter_map ~f:(function
               | value, ((_, op, args) :: _ as states) ->
                   if Search_state.mem search_state { value; type_ = Op.ret_type op } then (
                     List.iter states ~f:(fun (_, op, args) -> Search_state.insert search_state ~cost value op args);
                     None)
                   else Some (value, ref false, op, args, states)
               | _, [] -> None)
          |> List.permute
        in

        if List.is_empty states then []
        else (
          if ctx.verbose then (
            Fmt.epr "Distinct states: %d.\n%!" @@ List.length states;
            Fmt.epr "New states: %d.\n%!" @@ List.length
            @@ List.filter states ~f:(fun (v, _, op, _, _) ->
                   not @@ Search_state.mem search_state { value = v; type_ = Op.ret_type op }));

          let reference =
            Search_state.states ~type_ search_state |> Iter.to_list |> Vpt.create ctx.distance (`Good 10)
          in
          let insert key states =
            List.iter states ~f:(fun (value, op, args) -> Search_state.insert ~key search_state ~cost value op args)
          in
          List.fold states ~init:[] ~f:(fun kept (v, _, _, _, states) ->
              let inserted = ref false in
              Vpt.neighbors ctx.distance v ctx.thresh reference
              |> Iter.iter (fun v' ->
                     inserted := true;
                     insert v' states);
              List.iter kept ~f:(fun v' ->
                  if Float.(ctx.distance v v' < ctx.thresh) then (
                    inserted := true;
                    insert v' states));
              if !inserted then kept
              else (
                insert v states;
                v :: kept)))

      method check_states states =
        List.find_map states ~f:(fun (s, op, args) ->
            if [%compare.equal: Value.t] ctx.output s then
              Option.return @@ Search_state.program_of_op_args_exn search_state op args
            else None)

      method generate_states cost =
        if ctx.verbose then Fmt.epr "Generating states of cost %d...\n%!" cost;
        let new_states, gen_time =
          with_time (fun () -> Gen.generate_states Search_state.search ctx.ectx search_state ctx.ops cost)
        in
        if ctx.verbose then Fmt.epr "Generated %d states in %a.\n%!" (List.length new_states) Time.Span.pp gen_time;
        new_states

      method run =
        Synth_utils.luby_cutoff 2.0 16
        |> Iter.iter (fun upper ->
               let bound = Float.to_int upper in
               let scaled_bound = bound in
               upper_bound <- scaled_bound;

               Search_state.clear search_state;

               ctx.sample_states_time := Time.Span.zero;
               ctx.search_close_states_time := Time.Span.zero;
               ctx.groups_created := 0;
               ctx.states_grouped := 0;

               let ret, time =
                 with_time (fun () -> Iter.(0 -- upper_bound) |> Iter.iter (fun cost -> self#fill cost))
               in
               if ctx.verbose then
                 Fmt.epr "Completed iteration in %a. (search %a) (grouping %a %d/%d)\n%!" Time.Span.pp time Time.Span.pp
                   !(ctx.search_close_states_time) Time.Span.pp !(ctx.sample_states_time) !(ctx.groups_created)
                   !(ctx.states_grouped);
               ret)
    end

  let synth params = (new synthesizer params)#run
end
