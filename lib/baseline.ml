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
  end
end

module Make (Lang : Lang_intf) = struct
  open Lang
  module Search_state = Search_state_all.Make (Lang)
  module Gen = Generate.Gen_list (Lang)

  exception Done of Op.t Program.t

  module Ctx = struct
    type t = {
      max_cost : int;
      verbose : bool;
      ectx : Value.Ctx.t;
      ops : Op.t list;
      goal : Op.t -> Value.t -> bool;
      bank_size : float ref;
      found_program : bool ref;
      program_cost : float ref;
    }

    let create ?stats ?(verbose = false) ?(max_cost = Int.max_value) ectx ops goal =
      let stats = Option.value_lazy stats ~default:(lazy (Stats.create ())) in
      {
        max_cost;
        verbose;
        ectx;
        ops;
        goal =
          (match goal with
          | `Value v ->
              fun op v' -> [%compare.equal: Type.t] (Op.ret_type op) Type.output && [%compare.equal: Value.t] v v'
          | `Pred p -> p);
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

      method generate_states cost = Gen.generate_states Search_state.search eval_ctx search_state ops cost

      method insert_states cost states =
        List.iter states ~f:(fun (state, op, args) -> Search_state.insert search_state cost state op args);
        ctx.bank_size := Float.of_int @@ Search_state.length search_state

      method check_states states =
        List.iter states ~f:(fun (s, op, args) ->
            if ctx.goal op s then
              let p = Search_state.program_of_op_args_exn search_state op args in
              raise (Done p))

      method fill cost =
        let new_states = self#generate_states cost in
        self#insert_states cost new_states;
        self#check_states new_states;
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
