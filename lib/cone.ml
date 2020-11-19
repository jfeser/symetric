open Graph_ext

module Make (G : LABELED_GRAPH) = struct
  let cone graph target_node separator =
    let in_separator =
      let sep = Set.of_list (module G.V) separator in
      Set.mem sep
    in

    let graph' = G.create () in
    let work = Queue.create () in
    Queue.enqueue work target_node;
    let rec loop () =
      match Queue.dequeue work with
      | Some v ->
          G.add_vertex graph' v;
          let succ = G.succ_e graph v in
          (* Add edges to the filtered graph. *)
          List.iter succ ~f:(G.add_edge_e graph');
          (* Add new nodes to the queue. *)
          if not (in_separator v) then
            List.iter succ ~f:(fun (_, _, v') -> Queue.enqueue work v');
          loop ()
      | None -> ()
    in
    loop ();
    graph'
end
