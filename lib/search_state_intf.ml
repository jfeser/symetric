module type S = sig
  type type_
  type value
  type op

  module Class : sig
    type t [@@deriving compare, hash, sexp]

    include Comparator.S with type t := t

    val default : t
    val create : value -> type_ -> t
    val value : t -> value
    val type_ : t -> type_
  end

  type t [@@deriving sexp]

  module Path : sig
    type ctx = t
    type t [@@deriving sexp]

    val create : value -> op -> Class.t list -> t
    val pp : t Fmt.t
    val op : t -> op
    val args : t -> Class.t list
    val value : t -> value
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
  val search_iter : t -> cost:int -> type_:type_ -> Class.t Iter.t
  val search : t -> cost:int -> type_:type_ -> value list
  val find_term : t -> op Program.t -> (op * Class.t list) Program.t
  val mem_class : t -> Class.t -> bool
  val insert_class_members : t -> Class.t -> (value * op * Class.t list) list -> unit
  val insert_class : t -> value -> op -> value list -> unit
  val length : t -> int
  val print_stats : t -> unit
  val program_of_op_args_exn : t -> int -> op -> Class.t list -> op Program.t

  val validate :
    t -> (op -> value list -> value) -> (value -> value -> float) -> float -> unit

  val local_greedy :
    t ->
    int ->
    (op -> value list -> value) ->
    (value -> float) ->
    Class.t ->
    op Program.t option

  val random : t -> int -> Class.t -> op Program.t option
end
