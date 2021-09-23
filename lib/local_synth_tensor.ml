open Std

let kendall_tau x x' =
  let idx_of = List.mapi x ~f:(fun i v -> (v, i)) |> Map.of_alist_exn (module Int) in
  let x = List.map x ~f:(Map.find_exn idx_of) and x' = List.map x' ~f:(Map.find_exn idx_of) in
  Owl.Stats.kendall_tau (Array.of_list @@ List.map x ~f:Float.of_int) (Array.of_list @@ List.map x' ~f:Float.of_int)

let sequence_distance m s1 s2 =
  let n = List.length s1 in
  let h = Hashtbl.create m in
  let q =
    List.fold s1 ~init:0 ~f:(fun q x ->
        if Hashtbl.mem h x then q
        else (
          Hashtbl.set h ~key:x ~data:q;
          q + 1))
  in
  let b1 = Array.init n ~f:(fun _ -> Queue.create ()) and b2 = Array.init n ~f:(fun _ -> Queue.create ()) in
  List.zip_exn s1 s2
  |> List.iteri ~f:(fun i (x1, x2) ->
         let j = Hashtbl.find_exn h x1 and k = Hashtbl.find_exn h x2 in
         Queue.enqueue b1.(j) i;
         Queue.enqueue b2.(k) i);
  let p = Array.create ~len:n 0 in
  for i = 0 to q - 1 do
    while not (Queue.is_empty b1.(i)) do
      let h1 = Queue.dequeue_exn b1.(i) and h2 = Queue.dequeue_exn b2.(i) in
      p.(h1) <- h2
    done
  done;
  Array.inversions [%compare: int] p

let%expect_test "" =
  print_s [%message (sequence_distance (module Int) [ 1; 2; 3; 4; 5 ] [ 1; 3; 2; 5; 4 ] : int)];
  [%expect {| ("sequence_distance (module Int) [1; 2; 3; 4; 5] [1; 3; 2; 5; 4]" (2)) |}]

module Tensor = struct
  include Tensor

  module Value = struct
    include Value

    let dist () v v' =
      match (v, v') with
      | Tensor t, Tensor t' ->
          let value_dist = if [%compare.equal: Tensor.t] t t' then 0.0 else 1.0 in

          let shape_dist =
            let s = Tensor.shape t and s' = Tensor.shape t' in
            let n = List.length s in
            if n = List.length s' && Set.equal (Set.of_list (module Int) s) (Set.of_list (module Int) s') then
              (Float.of_int @@ sequence_distance (module Int) s s') /. Float.(of_int n * (of_int n - 1.0) / 2.0)
            else 1.0
          in
          (* print_s [%message (Tensor.shape t : int list) (Tensor.shape t' : int list) (shape_kt : float)]; *)
          (value_dist +. shape_dist) /. 2.0
      | _ -> Float.infinity
    (* match (t, t') with
       | Tensor t, Tensor t' ->
           let n_elems_dist = if Tensor.n_elems t = Tensor.n_elems t' then 0.0 else 1.0 in
           let shape_iou =
             let shape_set t = Set.of_list (module Int) @@ Tensor.shape t in
             let s = shape_set t and s' = shape_set t' in
             (Float.of_int @@ Set.length @@ Set.inter s s') /. (Float.of_int @@ Set.length @@ Set.union s s')
           in
           () *)
  end
end

module Synth = Local_search_diverse.Make (Tensor)

let synth ?(use_rules = true) cost (target : Tensor.Value.t) (ops : Tensor.Op.t list) =
  let open Tensor in
  let rules =
    let open Local_search.Pattern in
    let open Op in
    if use_rules then
      [
        (Apply (Cons, [ Var 0; Apply (Vec, [ Var 1 ]) ]), Apply (Cons, [ Var 1; Apply (Vec, [ Var 0 ]) ]));
        ( Apply (Cons, [ Var 0; Apply (Cons, [ Var 1; Var 2 ]) ]),
          Apply (Cons, [ Var 1; Apply (Cons, [ Var 0; Var 2 ]) ]) );
      ]
    else []
  in

  let ectx = () in
  let ctx =
    Synth.Ctx.create ~verbose:true ~distance:(Value.dist ectx) ~max_cost:cost ~rules ~search_thresh:(Top_k 3) ectx ops
      target
  in
  match (new Synth.synthesizer ctx)#run with
  | Some p -> print_s [%message (p : Op.t Program.t)]
  | None -> failwith "synthesis failed"
