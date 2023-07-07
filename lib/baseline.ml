open Std

module type DSL = sig
  module Type : sig
    type t [@@deriving compare, equal, hash, sexp]

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

    val default : t
    val eval : Op.t -> t list -> t
    val is_error : t -> bool
  end

  val operators : Op.t list
end

module Params = struct
  type t = { max_cost : int; verbose : bool } [@@deriving yojson]

  let default_verbose = false
  let default_max_cost = Int.max_value

  let create ?(verbose = default_verbose) ?(max_cost = default_max_cost) () =
    { max_cost; verbose }

  let param =
    let open Command.Let_syntax in
    [%map_open
      let max_cost =
        flag "max-cost" (optional int) ~doc:"maximum cost of programs to consider"
      and verbose = flag "verbose" no_arg ~doc:"print verbose output" in
      create ~verbose ?max_cost ()]
end

module Stats = struct
  type t = { runtime : Timer.t; bank_size : float ref; program_cost : float ref }
  [@@deriving yojson_of]

  let create () =
    { runtime = Timer.create (); bank_size = ref 0.; program_cost = ref Float.nan }
end

module Make (Lang : DSL) = struct
  open Lang
  module S = Search_state_all.Make (Lang)

  exception Done of Op.t Program.t

  type t = {
    params : Params.t;
    goal : Op.t -> Value.t -> bool;
    stats : Stats.t;
    search_state : S.t;
  }

  let generate_states this cost =
    Generate.generate
      (module Lang)
      (S.search_iter this.search_state)
      S.Class.value operators cost

  let insert_states this cost states =
    let new_states =
      Iter.filter
        (fun (state, op, args) ->
          let type_ = Op.ret_type op in
          let in_bank = S.mem this.search_state type_ cost state in
          if not in_bank then S.insert_class this.search_state type_ cost state op args;
          not in_bank)
        states
    in
    this.stats.bank_size := Float.of_int @@ S.length this.search_state;
    new_states

  let check_states this states =
    Iter.iter
      (fun (s, op, args) ->
        if this.goal op s then
          let p =
            S.program_of_op_args_exn this.search_state
              (Int.ceil_log2 this.params.max_cost)
              op args
          in
          raise (Done p))
      states

  let fill this cost =
    let new_states = generate_states this cost in
    let inserted_states = insert_states this cost new_states in
    check_states this inserted_states;
    if this.params.verbose then (
      Fmt.epr "Finished cost %d\n%!" cost;
      S.print_stats this.search_state)

  let synthesize ?(log = fun _ _ -> ()) params goal =
    let this =
      {
        goal =
          (match goal with
          | `Value v ->
              fun op v' ->
                [%compare.equal: Type.t] (Op.ret_type op) Type.output
                && [%compare.equal: Value.t] v v'
          | `Pred p -> p);
        stats = Stats.create ();
        search_state = S.create ();
        params;
      }
    in
    Timer.start this.stats.runtime;
    try
      for cost = 0 to params.max_cost do
        fill this cost;
        log this.stats None
      done;
      Timer.stop this.stats.runtime;
      None
    with Done p ->
      this.stats.program_cost := Float.of_int @@ Program.size p;
      log this.stats (Some p);
      Timer.stop this.stats.runtime;
      Some p
end

let synthesize (type op value)
    (module Dsl : DSL with type Op.t = op and type Value.t = value) ?log params goal =
  let module Synth = Make (Dsl) in
  let mk_log output stats m_prog =
    let program_size =
      Option.map m_prog ~f:(fun p -> Float.of_int @@ Program.size p)
      |> Option.value ~default:Float.nan
    in
    let program_json =
      Option.map m_prog ~f:(fun p ->
          `String (Sexp.to_string @@ [%sexp_of: Dsl.Op.t Program.t] p))
      |> Option.value ~default:`Null
    in
    output
      (`Assoc
        [
          ("method", `String "enumeration");
          ("program_size", `Float program_size);
          ("program", program_json);
          ("stats", Stats.yojson_of_t stats);
        ])
  in
  let log = Option.map log ~f:mk_log in
  Synth.synthesize ?log params goal
