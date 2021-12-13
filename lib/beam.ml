open Std

module type Lang_intf = sig
  module Type : sig
    type t [@@deriving compare, hash, sexp]

    val output : t
  end

  module Op : sig
    type t [@@deriving compare, hash, sexp]

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

    val eval : Ctx.t -> Op.t -> t list -> t
    val is_error : t -> bool
    val distance : t -> t -> float
  end
end

module Make (Lang : Lang_intf) = struct
  open Lang
  module Search_state = Search_state_all.Make (Lang)
  module Gen = Generate.Gen_iter (Lang)

  exception Done of Op.t Program.t

  module Ctx = struct
    type t = {
      max_cost : int;
      verbose : bool;
      ectx : Value.Ctx.t;
      ops : Op.t list;
      goal : Value.t;
      k : int;
      bank_size : float ref;
      found_program : bool ref;
      program_cost : float ref;
    }

    let create ?stats ?(verbose = false) ?(max_cost = Int.max_value) ?(k = 100) ectx ops goal =
      let stats = Option.value_lazy stats ~default:(lazy (Stats.create ())) in
      {
        max_cost;
        verbose;
        ectx;
        ops;
        goal;
        k;
        bank_size = Stats.add_probe_exn stats "bank-size";
        found_program = ref false;
        program_cost = Stats.add_probe_exn stats "program-cost";
      }
  end

  class synthesizer (ctx : Ctx.t) =
    object (self : 'self)
      val max_cost = ctx.max_cost
      val verbose = ctx.verbose
      val search_state = Search_state.create ()
      val eval_ctx = ctx.ectx
      val ops = ctx.ops
      method get_search_state = search_state

      method generate_states cost =
        Gen.generate_states
          (fun ss ~cost ~type_ -> Iter.of_list @@ Search_state.search ss ~cost ~type_)
          eval_ctx search_state ops cost

      method select_closest_states states =
        Iter.map (fun ((v, _, _) as state) -> (Value.distance v ctx.goal, state)) states
        |> Iter.top_k ~cmp:(fun (d, _) (d', _) -> -[%compare: float] d d') ctx.k
        |> Iter.map Tuple.T2.get2

      method insert_states cost states =
        Iter.iter (fun (state, op, args) -> Search_state.insert search_state cost state op args) states;
        ctx.bank_size := Float.of_int @@ Search_state.length search_state

      method check_states =
        Iter.map (fun ((s, op, args) as state) ->
            (if [%compare.equal: Value.t] s ctx.goal then
             let p = Search_state.program_of_op_args_exn search_state op args in
             raise (Done p));
            state)

      method fill cost =
        self#generate_states cost |> self#check_states |> self#select_closest_states |> self#insert_states cost;

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
          Some p
    end
end
