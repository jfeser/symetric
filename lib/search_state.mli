open Ast

type t

module Is_fresh : sig
  type 'a t = Fresh of 'a | Stale of 'a

  val is_fresh : 'a t -> bool

  val unwrap : 'a t -> 'a
end

module Args : sig
  type t [@@deriving compare, hash, sexp_of]

  include Comparator.S with type t := t

  type ctx

  val create : ctx -> Offset.t Op.t -> t

  val id : t -> int

  val op : ctx -> t -> Offset.t Op.t

  val graphviz_pp : ctx -> t Fmt.t

  val output_type : ctx -> t -> Type.t
end
with type ctx := t

module State : sig
  type t [@@deriving compare, hash, sexp_of]

  include Comparator.S with type t := t

  type ctx

  val create : ctx -> Abs.t -> int -> Type.t -> t Is_fresh.t

  val id : t -> int

  val state : ctx -> t -> Abs.t

  val cost : ctx -> t -> int

  val type_ : ctx -> t -> Type.t

  val graphviz_pp : ctx -> t Fmt.t
end
with type ctx := t

module Node : sig
  type t [@@deriving compare, hash, sexp_of]

  include Comparator.S with type t := t

  type ctx

  val match_ : args:(Args.t -> 'a) -> state:(State.t -> 'a) -> t -> 'a

  val pp : t Fmt.t

  val id : t -> int

  val is_state : t -> bool

  val is_args : t -> bool

  val of_args : Args.t -> t

  val of_state : State.t -> t

  val to_args : t -> Args.t option

  val to_state : t -> State.t option

  val to_args_exn : t -> Args.t

  val to_state_exn : t -> State.t

  val type_ : ctx -> t -> Type.t
end
with type ctx := t

module G : sig
  include Graph_ext.LABELED_GRAPH with type vertex = Node.t and type label = int

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

val create : Params.t -> t

val params : t -> Params.t

val graph : t -> G.t

val states_of_cost : t -> int -> State.t list

val filter : t -> f:(G.V.t -> bool) -> unit

val nb_vertex : t -> int

val inputs : t -> Args.t -> State.t list

val fix_up : t -> unit

val insert_hyper_edge_if_not_exists :
  ?state:Abs.t -> t -> State.t list -> Offset.t Op.t -> int -> State.t option

val pp : t Fmt.t

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

val validate : ?k:int -> t -> unit
