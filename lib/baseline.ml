open Std

module type Lang_intf = sig
  module Type : sig
    type t [@@deriving compare, equal, hash, sexp]

    val default : t
    val output : t
  end

  module Op : sig
    type t [@@deriving compare, equal, hash, sexp]

    val default : t
    val pp : t Fmt.t
    val cost : t -> int
    val arity : t -> int
    val args_type : t -> Type.t list
    val ret_type : t -> Type.t
    val is_commutative : t -> bool
  end

  module Value : sig
    type t [@@deriving compare, equal, hash, sexp]

    module Ctx : sig
      type t
    end

    val default : t
    val eval : Ctx.t -> Op.t -> t list -> t
    val is_error : t -> bool
  end
end

module Make (Lang : Lang_intf) = struct
  open Lang
  module S = Search_state_all.Make (Lang)
  module Gen = Generate.Gen_iter (Lang)

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
              fun op v' ->
                [%compare.equal: Type.t] (Op.ret_type op) Type.output
                && [%compare.equal: Value.t] v v'
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
      val search_state = S.create ()
      val eval_ctx = ctx.ectx
      val ops = ctx.ops
      method get_search_state = search_state

      method generate_states cost =
        Gen.generate_states S.search_iter S.Class.value eval_ctx search_state ops cost

      method insert_states states =
        let new_states =
          Iter.filter
            (fun (state, op, args) ->
              let in_bank = S.mem search_state (Op.ret_type op) state in
              if not in_bank then S.insert_class search_state state op args;
              not in_bank)
            states
        in
        ctx.bank_size := Float.of_int @@ S.length search_state;
        new_states

      method check_states states =
        Iter.iter
          (fun (s, op, args) ->
            if ctx.goal op s then
              let p =
                S.program_of_op_args_exn search_state (Int.ceil_log2 max_cost) op args
              in
              raise (Done p))
          states

      method fill cost =
        let new_states = self#generate_states cost in
        let inserted_states = self#insert_states new_states in
        self#check_states inserted_states;
        if verbose then (
          Fmt.epr "Finished cost %d\n%!" cost;
          S.print_stats search_state)

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
