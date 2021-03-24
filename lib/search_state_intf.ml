module type S = sig
  type t

  type op

  type type_

  type abs

  type params

  module Args_label : sig
    type t = Op of op | Merge [@@deriving compare, hash, sexp]
  end

  module Args : sig
    type t [@@deriving compare, hash, sexp_of]

    include Comparator.S with type t := t

    type ctx

    val create : ctx -> Args_label.t -> t

    val id : t -> int

    val label : ctx -> t -> Args_label.t

    val op_exn : ctx -> t -> op

    val graphviz_pp : ctx -> t Fmt.t

    val to_message : ctx -> t -> Sexp.t
  end
  with type ctx := t

  module State : sig
    type t [@@deriving compare, hash, sexp_of]

    include Comparator.S with type t := t

    type ctx

    val create : ctx -> abs -> int -> type_ -> t Is_fresh.t

    val id : t -> int

    val state : ctx -> t -> abs

    val cost : ctx -> t -> int

    val type_ : ctx -> t -> type_

    val graphviz_pp : ctx -> t Fmt.t

    val to_message : ctx -> t -> Sexp.t
  end
  with type ctx := t

  module Node : sig
    type t [@@deriving compare, hash, sexp_of]

    include Comparator.S with type t := t

    type ctx

    val match_ : args:(Args.t -> 'a) -> state:(State.t -> 'a) -> t -> 'a

    val id : t -> int

    val is_state : t -> bool

    val is_args : t -> bool

    val of_args : Args.t -> t

    val of_state : State.t -> t

    val to_args : t -> Args.t option

    val to_state : t -> State.t option

    val to_args_exn : t -> Args.t

    val to_state_exn : t -> State.t
  end
  with type ctx := t

  module G : sig
    include
      Graph_ext.LABELED_GRAPH with type vertex = Node.t and type label = int

    include
      Graph_ext.CHANGED
        with type graph = t
         and type vertex = V.t
         and type edge = E.t

    module Fold :
      Graph_ext.FOLDS
        with type graph = t
         and type vertex = V.t
         and type edge = E.t
  end

  val create : params -> t

  val params : t -> params

  val graph : t -> G.t

  val states_of_cost : t -> int -> State.t list

  val filter : t -> f:(G.V.t -> bool) -> unit

  val nb_vertex : t -> int

  val inputs : t -> Args.t -> State.t list

  val fix_up : t -> unit

  val insert_hyper_edge : ?state:abs -> t -> State.t list -> op -> int -> unit

  val insert_merge : t -> State.t list -> abs -> unit

  module type ATTR = sig
    val vertex_name : G.V.t -> string

    val vertex_attributes : G.V.t -> Graph.Graphviz.DotAttributes.vertex list
  end

  val attr : t -> (module ATTR)

  val dump_detailed :
    ?suffix:string ->
    ?cone:(G.V.t -> bool) ->
    ?separator:(G.V.t -> bool) ->
    ?refinement:(G.V.t * int * G.V.t -> bool) ->
    ?depth:int ->
    t ->
    unit

  val dump_detailed_graph :
    ?suffix:string ->
    ?cone:(G.V.t -> bool) ->
    ?separator:(G.V.t -> bool) ->
    ?refinement:(G.V.t * int * G.V.t -> bool) ->
    ?depth:int ->
    t ->
    G.t ->
    unit

  val sample : t -> State.t -> op Program.t

  (* val validate : ?k:int -> t -> unit *)
end
