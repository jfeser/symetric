module type DSL = sig
  module Type : sig
    type t [@@deriving compare, equal, hash, sexp]
  end

  module Op : sig
    type t [@@deriving compare, equal, hash, sexp]

    val cost : t -> int
    val args_type : t -> Type.t list
    val ret_type : t -> Type.t
    val pp : t Fmt.t
  end

  module Value : sig
    type t [@@deriving compare, equal, hash, sexp]
  end
end

module Make (Dsl : DSL) : sig
  module Class : sig
    type t [@@deriving compare, hash, sexp]

    include Comparator.S with type t := t

    val create : Dsl.Value.t -> Dsl.Type.t -> t
    val value : t -> Dsl.Value.t
    val type_ : t -> Dsl.Type.t
  end

  type t [@@deriving sexp]

  module Path : sig
    type ctx = t
    type t [@@deriving sexp]

    val pp : t Fmt.t
    val op : t -> Dsl.Op.t
    val args : t -> Class.t list
    val value : t -> Dsl.Value.t
  end
  with type ctx := t

  val create : unit -> t
  (** Create a new search state. *)

  (** {0 Access iterators} *)

  val in_paths : t -> Class.t -> Path.t Iter.t
  (** Iterator over the hyperedges that point to a particular class. *)

  val classes : t -> Class.t Iter.t

  (** {0 Serialization} *)

  val to_channel : Out_channel.t -> t -> unit
  val of_channel : In_channel.t -> t
  val search_iter : t -> cost:int -> type_:Dsl.Type.t -> Class.t Iter.t
  val search : t -> cost:int -> type_:Dsl.Type.t -> Dsl.Value.t list
  val find_term : t -> Dsl.Op.t Program.t -> (Dsl.Op.t * Class.t list) Program.t
  val mem_class : t -> Class.t -> bool

  val insert_class_members :
    t -> Class.t -> (Dsl.Value.t * Dsl.Op.t * Class.t list) list -> unit

  val insert_class : t -> Dsl.Value.t -> Dsl.Op.t -> Dsl.Value.t list -> unit
  val length : t -> int
  val print_stats : t -> unit
  val program_of_op_args_exn : t -> int -> Dsl.Op.t -> Class.t list -> Dsl.Op.t Program.t

  val validate :
    t ->
    (Dsl.Op.t -> Dsl.Value.t list -> Dsl.Value.t) ->
    (Dsl.Value.t -> Dsl.Value.t -> float) ->
    float ->
    unit

  val local_greedy :
    t ->
    int ->
    (Dsl.Op.t -> Dsl.Value.t list -> Dsl.Value.t) ->
    (Dsl.Value.t -> float) ->
    Class.t ->
    Dsl.Op.t Program.t option

  val exhaustive :
    ?width:int ->
    t ->
    int ->
    (Dsl.Op.t -> Dsl.Value.t list -> Dsl.Value.t) ->
    (Dsl.Value.t -> float) ->
    Class.t ->
    Dsl.Op.t Program.t option

  val random : t -> int -> Class.t -> Dsl.Op.t Program.t option
  val centroid : t -> int -> Class.t -> Dsl.Op.t Program.t option
end
