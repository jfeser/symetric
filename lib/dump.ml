open Search_state

let make_output_graph ?(refinement = fun _ -> false) cone separator output =
  let module Viz =
    Graph.Graphviz.Dot
      (Search_state.G)
      (struct
        let graph_attributes _ = []

        let default_vertex_attributes _ = []

        let vertex_name n = Fmt.str "%d" @@ Node.id n

        let vertex_attributes n =
          let attrs = if cone n then [ `Style `Filled ] else [] in
          let attrs = if separator n then `Style `Dotted :: attrs else attrs in
          let attrs' =
            Node.match_ n
              ~args:(fun n ->
                [
                  `HtmlLabel (Fmt.str "%a" Args.graphviz_pp n);
                  `Shape `Box;
                ])
              ~state:(fun n ->
                [ `HtmlLabel (Fmt.str "%a" State.graphviz_pp n) ]
                @
                if
                  Option.map output ~f:(Abs.contains @@ State.state n)
                  |> Option.value ~default:false
                then [ `Style `Bold ]
                else [])
          in
          attrs @ attrs'

        let get_subgraph _ = None

        let default_edge_attributes _ = []

        let edge_attributes ((_, i, _) as e) =
          let attrs = if i >= 0 then [ `Label (sprintf "%d" i) ] else [] in
          let attrs = if refinement e then `Style `Dotted :: attrs else attrs in
          attrs
      end)
  in
  Viz.output_graph

let step = ref 0

let dump_detailed ?suffix ?output ?(cone = fun _ -> false)
    ?(separator = fun _ -> false) ?(refinement = fun _ -> false) graph =
  if !Global.enable_dump then (
    let output_graph = make_output_graph ~refinement cone separator output in
    let fn =
      let suffix =
        Option.map suffix ~f:(sprintf "-%s") |> Option.value ~default:""
      in
      sprintf "%04d-graph%s.dot" !step suffix
    in
    Out_channel.with_file fn ~f:(fun ch ->
        output_graph ch graph.Search_state.graph);
    incr step )
