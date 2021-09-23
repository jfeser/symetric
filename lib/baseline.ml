open Std

include struct
  open Dumb_params

  let spec = Spec.create ~name:"baseline" ()

  let synth = Spec.add spec @@ Param.const_str ~name:"synth" "baseline"

  let max_cost = Spec.add spec @@ Param.int ~name:"max-cost" ~doc:" max search cost" ()

  let verbose = Spec.add spec @@ Param.bool ~init:(`Cli (Some false)) ~name:"verbose" ~doc:" verbose output" ()

  let bank_size = Spec.add spec @@ Param.float_ref ~name:"bank-size" ()

  let found_program = Spec.add spec @@ Param.bool_ref ~name:"found-program" ()

  let program_cost = Spec.add spec @@ Param.float_ref ~name:"program-cost" ()
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
  end

  module Bench : sig
    type t [@@deriving of_sexp]

    val ops : t -> Op.t list

    val output : t -> Value.t
  end

  val bench : (Bench.t, Dumb_params.Param.bound) Dumb_params.Param.t
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

    let create ?stats ?(verbose = false) ~max_cost ectx ops goal =
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

    let of_params params =
      let bench = Params.get params bench in
      {
        max_cost = Params.get params max_cost;
        verbose = Params.get params verbose;
        ectx = Value.Ctx.of_params params;
        ops = Bench.ops bench;
        goal =
          (fun op v ->
            [%compare.equal: Type.t] (Op.ret_type op) Type.output && ([%compare.equal: Value.t] v @@ Bench.output bench));
        bank_size = Params.get params bank_size;
        found_program = Params.get params found_program;
        program_cost = Params.get params program_cost;
      }
  end

  class synthesizer (ctx : Ctx.t) =
    object (self : 'self)
      val max_cost = ctx.max_cost

      val verbose = ctx.verbose

      val search_state = Search_state.create ctx.max_cost

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

let cli (type value op) (module Lang : Lang_intf.S with type Value.t = value and type Op.t = op) =
  let module Synth = Make (Lang) in
  let spec = Dumb_params.Spec.union [ Lang.spec; Params.spec; spec ] in
  let open Command.Let_syntax in
  Command.basic ~summary:(sprintf "Enumerative baseline for %s" Lang.name)
  @@ [%map_open
       let params = Dumb_params.Spec.cli spec in
       Synth_utils.run_synth
         (fun params -> new Synth.synthesizer @@ Synth.Ctx.of_params params)
         params
         (Option.iter ~f:(fun p -> eprint_s ([%sexp_of: Lang.Op.t Program.t] p)))]
