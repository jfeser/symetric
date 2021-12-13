module type GRAPH = sig
  include Graph.Sig.I

  module V : sig
    type t [@@deriving compare, hash, sexp_of]

    include Comparator.S with type t := t
    include Graph.Sig.VERTEX with type t := t
  end
  with type t = vertex

  module E : sig
    type t [@@deriving compare, hash, sexp_of]

    include Comparator.S with type t := t
    include Graph.Sig.EDGE with type t := t and type vertex = V.t
  end
  with type t = edge
end

module type LABELED_GRAPH = sig
  type vertex
  type label

  include GRAPH with type vertex := vertex and type edge = vertex * label * vertex
end

module Make (V' : sig
  type t [@@deriving compare, hash, sexp_of]
end) (E' : sig
  type t [@@deriving compare, hash, sexp_of]

  val default : t
end) =
struct
  type label = E'.t

  include
    Graph.Imperative.Digraph.ConcreteBidirectionalLabeled
      (struct
        type t = V'.t

        let compare = [%compare: V'.t]
        let hash = [%hash: V'.t]
        let equal = [%compare.equal: V'.t]
      end)
      (struct
        type t = E'.t

        let compare = [%compare: E'.t]
        let default = E'.default
      end)

  module V = struct
    include V'
    include Comparator.Make (V')
    include (V : Graph.Sig.VERTEX with type t := t)
  end

  module E = struct
    module T = struct
      type t = V.t * E'.t * V.t [@@deriving compare, hash, sexp_of]
    end

    include T
    include Comparator.Make (T)
    include (E : Graph.Sig.EDGE with type t := t and type vertex = V.t)
  end
end

module type FOLDS = sig
  type graph
  type edge
  type vertex

  module E : sig
    type t = edge [@@deriving compare, hash, sexp_of]

    include Container.S0 with type t := graph and type elt := t
  end

  module V : sig
    type t = vertex [@@deriving compare, hash, sexp_of]

    include Container.S0 with type t := graph and type elt := t

    val filter_map : graph -> f:(vertex -> 'a option) -> 'a list
    val filter : graph -> f:(vertex -> bool) -> vertex list
  end

  module Pred : Container.S0 with type t := graph * vertex and type elt := vertex
  module Succ : Container.S0 with type t := graph * vertex and type elt := vertex
end

module Folds (G : GRAPH) = struct
  type graph = G.t
  type edge = G.E.t
  type vertex = G.V.t

  module V = struct
    include Container.Make0 (struct
      type nonrec t = G.t

      module Elt = struct
        type t = G.V.t [@@deriving equal]
      end

      let fold g ~init ~f = G.fold_vertex (fun v acc -> f acc v) g init
      let iter = `Custom (fun g ~f -> G.iter_vertex f g)
      let length = `Custom G.nb_vertex
    end)

    let filter g ~f = fold ~f:(fun xs x -> if f x then x :: xs else xs) g ~init:[]
    let filter_map g ~f = fold ~f:(fun xs x -> match f x with Some x' -> x' :: xs | None -> xs) g ~init:[]

    include G.V
  end

  module E = struct
    include Container.Make0 (struct
      type nonrec t = G.t

      module Elt = struct
        type t = G.E.t

        let equal e e' = G.E.compare e e' = 0
      end

      let fold g ~init ~f = G.fold_edges_e (fun v acc -> f acc v) g init
      let iter = `Custom (fun g ~f -> G.iter_edges_e f g)
      let length = `Custom G.nb_edges
    end)

    include G.E
  end

  module Pred = struct
    include Container.Make0 (struct
      type nonrec t = G.t * G.V.t

      module Elt = struct
        type t = G.V.t [@@deriving equal]
      end

      let fold (g, v) ~init ~f = G.fold_pred (fun v acc -> f acc v) g v init
      let iter = `Custom (fun (g, v) ~f -> G.iter_pred f g v)
      let length = `Custom (fun (g, v) -> G.in_degree g v)
    end)
  end

  module Succ = struct
    include Container.Make0 (struct
      type nonrec t = G.t * G.V.t

      module Elt = struct
        type t = G.V.t [@@deriving equal]
      end

      let fold (g, v) ~init ~f = G.fold_succ (fun v acc -> f acc v) g v init
      let iter = `Custom (fun (g, v) ~f -> G.iter_succ f g v)
      let length = `Custom (fun (g, v) -> G.out_degree g v)
    end)
  end
end

module type CHANGED = sig
  type graph
  type vertex
  type edge

  val add_vertex : graph -> vertex -> unit
  val remove_vertex : graph -> vertex -> unit
  val add_edge : graph -> vertex -> vertex -> unit
  val add_edge_e : graph -> edge -> unit
  val remove_edge : graph -> vertex -> vertex -> unit
  val remove_edge_e : graph -> edge -> unit
  val has_changed : unit -> bool
  val reset_changed : unit -> unit
end

module Changed (G : GRAPH) () = struct
  open G

  type graph = G.t
  type vertex = G.V.t
  type edge = G.E.t

  let changed = ref false

  let add_vertex g v =
    changed := !changed || not (mem_vertex g v);
    add_vertex g v

  let remove_vertex g v =
    changed := !changed || mem_vertex g v;
    remove_vertex g v

  let add_edge g v v' =
    changed := !changed || not (mem_edge g v v');
    add_edge g v v'

  let add_edge_e g e =
    changed := !changed || not (mem_edge_e g e);
    add_edge_e g e

  let remove_edge g v v' =
    changed := !changed || mem_edge g v v';
    remove_edge g v v'

  let remove_edge_e g e =
    changed := !changed || mem_edge_e g e;
    remove_edge_e g e

  let has_changed () = !changed
  let reset_changed () = changed := false
end
