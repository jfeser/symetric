open Graph_ext

module Make (G : LABELED_GRAPH) = struct
  (** Return the subset of `graph` that is reachable from `target`. *)
  let cone graph target =
    let graph' = G.create () in
    let work = Queue.create () in
    Queue.enqueue work target;
    let rec loop () =
      match Queue.dequeue work with
      | Some v ->
          G.add_vertex graph' v;
          let succ = G.succ_e graph v in
          (* Add edges to the filtered graph. *)
          List.iter succ ~f:(G.add_edge_e graph');
          (* Add new nodes to the queue. *)
          List.iter succ ~f:(fun (_, _, v') -> Queue.enqueue work v');
          loop ()
      | None -> ()
    in
    loop ();
    graph'
end
