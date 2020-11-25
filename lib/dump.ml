open Params

let step = ref 0

module Make
    (G : Graph_ext.LABELED_GRAPH with type label = int) (Attr : sig
      val vertex_name : G.V.t -> string

      val vertex_attributes : G.V.t -> Graph.Graphviz.DotAttributes.vertex list
    end) =
struct
  module F = Graph_ext.Folds (G)

  let make_output_graph ?(refinement = fun _ -> false) cone separator =
    let module Viz =
      Graph.Graphviz.Dot
        (G)
        (struct
          let graph_attributes _ = []

          let default_vertex_attributes _ = []

          let vertex_name = Attr.vertex_name

          let vertex_attributes n =
            let attrs = if cone n then [ `Style `Filled ] else [] in
            let attrs =
              if separator n then `Style `Dotted :: attrs else attrs
            in
            let attrs' = Attr.vertex_attributes n in
            attrs @ attrs'

          let get_subgraph _ = None

          let default_edge_attributes _ = []

          let edge_attributes ((_, i, _) as e) =
            let attrs = if i >= 0 then [ `Label (sprintf "%d" i) ] else [] in
            let attrs =
              if refinement e then `Style `Dotted :: attrs else attrs
            in
            attrs
        end)
    in
    Viz.output_graph

  let filter_depth g d =
    let roots = F.V.filter g ~f:(fun v -> List.is_empty @@ G.succ g v) in
    let module Inv_reachable =
      Graph.Fixpoint.Make
        (G)
        (struct
          type vertex = G.V.t

          type edge = G.E.t

          type g = G.t

          type data = int

          let direction = Graph.Fixpoint.Backward

          let equal = Int.( = )

          let join = Int.min

          let analyze _ x = x + 1
        end)
    in
    let g' = G.copy g in
    let f =
      Inv_reachable.analyze
        (fun v ->
          if List.mem roots ~equal:[%compare.equal: G.V.t] v then 0 else 100)
        g'
    in
    F.V.iter g ~f:(fun v -> if f v > d then G.remove_vertex g' v);
    g'

  let dump_detailed ?suffix ?(cone = fun _ -> false)
      ?(separator = fun _ -> false) ?(refinement = fun _ -> false) ?depth params
      graph =
    if params.enable_dump then (
      let output_graph = make_output_graph ~refinement cone separator in
      let graph =
        Option.map depth ~f:(filter_depth graph) |> Option.value ~default:graph
      in
      let fn =
        let suffix =
          Option.map suffix ~f:(sprintf "-%s") |> Option.value ~default:""
        in
        sprintf "%04d-graph%s.dot" !step suffix
      in
      Out_channel.with_file fn ~f:(fun ch -> output_graph ch graph);
      incr step )
end
