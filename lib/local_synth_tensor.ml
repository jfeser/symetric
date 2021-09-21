let kendall_tau x x' =
  let idx_of = List.mapi x ~f:(fun i v -> (v, i)) |> Map.of_alist_exn (module Int) in
  let x = List.map x ~f:(Map.find_exn idx_of) and x' = List.map x' ~f:(Map.find_exn idx_of) in
  Owl.Stats.kendall_tau (Array.of_list @@ List.map x ~f:Float.of_int) (Array.of_list @@ List.map x' ~f:Float.of_int)

module Tensor = struct
  include Tensor

  module Value = struct
    include Value

    let dist () v v' =
      match (v, v') with
      | Tensor t, Tensor t' ->
          let value_dist = if [%compare.equal: Tensor.t] t t' then 0.0 else 1.0 in

          let shape_kt =
            let s = Set.of_list (module Int) @@ Tensor.shape t and s' = Set.of_list (module Int) @@ Tensor.shape t' in
            if Set.equal s s' && List.length @@ Tensor.shape t = List.length @@ Tensor.shape t' then
              kendall_tau (Tensor.shape t) (Tensor.shape t')
            else 1.0
          in
          print_s [%message (Tensor.shape t : int list) (Tensor.shape t' : int list) (shape_kt : float)];
          (value_dist +. shape_kt) /. 2.0
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

let synth (target : Tensor.Value.t) (ops : Tensor.Op.t list) =
  let open Tensor in
  let rules =
    let open Local_search.Pattern in
    let open Op in
    [
      (Apply (Cons, [ Var 0; Apply (Vec, [ Var 1 ]) ]), Apply (Cons, [ Var 1; Apply (Vec, [ Var 0 ]) ]));
      (Apply (Cons, [ Var 0; Apply (Cons, [ Var 1; Var 2 ]) ]), Apply (Cons, [ Var 1; Apply (Cons, [ Var 0; Var 2 ]) ]));
    ]
  in

  let ectx = () in
  let ctx =
    Synth.Ctx.create ~verbose:true ~distance:(Value.dist ectx) ~max_cost:25 ~rules ~search_thresh:(Top_k 100) ectx ops
      target
  in
  match (new Synth.synthesizer ctx)#run with
  | Some p -> print_s [%message (p : Op.t Program.t)]
  | None -> failwith "synthesis failed"