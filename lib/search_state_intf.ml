module type S = sig
  type type_
  type value
  type op

  module Attr : sig
    type t = { cost : int; type_ : type_ } [@@deriving compare, hash, sexp]

    include Comparator.S with type t := t

    val create : int -> type_ -> t
  end

  module Class : sig
    type t = { type_ : type_; value : value } [@@deriving compare, hash, sexp]

    include Comparator.S with type t := t

    val default : t
    val create : value -> type_ -> t
    val value : t -> value
    val type_ : t -> type_
  end

  module Path0 : sig
    type t = {
      op : op;
      args : Class.t list;
      value : value;
      mutable cost : int;
      mutable height : int;
    }
    [@@deriving sexp]

    val default : t
    val create : value -> op -> Class.t list -> t
    val pp : t Fmt.t
  end

  type paths = { paths : Path0.t Sek.Ephemeral.t; min_cost : int; min_height : int }
  [@@deriving sexp]

  type t = {
    classes : Class.t Sek.Ephemeral.t Hashtbl.M(Attr).t;
    paths : paths Hashtbl.M(Class).t;
  }
  [@@deriving sexp]

  module Path : sig
    type ctx = t
    type t = Path0.t

    val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
    val create : value -> op -> Class.t list -> t
    val op : t -> op
    val args : t -> Class.t list
    val value : t -> value
    val cost : ctx -> t -> int
    val height : ctx -> t -> int
    val default : t
  end
  with type ctx := t

  val create : unit -> t
  val to_channel : Out_channel.t -> t -> unit
  val of_channel : In_channel.t -> t
  val pp_dot : Format.formatter -> t -> unit
  val search_iter : t -> cost:int -> type_:type_ -> (Class.t -> unit) -> unit
  val search : t -> cost:int -> type_:type_ -> value list
  val find_term : t -> op Program.t -> (op * Class.t list) Program.t
  val mem_class : t -> Class.t -> bool
  val classes : t -> (Class.t -> unit) -> unit
  val insert_class_members : t -> Class.t -> (value * op * Class.t list) list -> unit
  val insert_class : t -> value -> op -> value list -> unit
  val length : t -> int
  val print_stats : t -> unit
  val program_exn : t -> int -> Class.t -> op Program.t
  val program_of_op_args_exn : t -> int -> op -> Class.t list -> op Program.t
  val random_program_exn : ?max_cost:int -> t -> Class.t -> op Program.t
  val clear : t -> unit

  val validate :
    t -> (op -> value list -> value) -> (value -> value -> float) -> float -> unit

  val local_greedy_new :
    t ->
    int ->
    (op -> value list -> value) ->
    (value -> float) ->
    Class.t ->
    op Program.t option

  val local_greedy :
    t ->
    int ->
    (op -> value list -> value) ->
    (value -> float) ->
    Class.t ->
    op Program.t option
end
