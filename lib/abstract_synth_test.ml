module Synth = Abstract_synth_tensor_v2.Synth

let%expect_test "" =
  let open Synth.Abs_value in
  let args = [ true_; true_ ] in
  let ctx = Ctx.create (Tensor.Value.Ctx.create ()) in
  let args' =
    _strengthen ~k:3
      [ Tensor { elems = [ 1; 2; 3; 4 ]; shape = [ 1; 4 ] }; Vector [ 2; 2 ] ]
      args
      (fun args -> implies (eval ctx Reshape args) (Preds (Set.of_list (module Pred) [ `Pred (N_dims 2) ])))
  in
  print_s [%message (args' : Synth.Abs_value.t list)];
  [%expect
    {|
    (ops
     (Args (And 0) (Pred 0 True)
      (Pred 0 (Concrete (Tensor ((elems (1 2 3 4)) (shape (1 4))))))
      (Pred 0 (Pred (N_dims 2))) (Pred 0 (Pred (N_elems 4))) (And 1)
      (Pred 1 True) (Pred 1 (Concrete (Vector (2 2)))) (Pred 1 (Pred (Len 2)))
      (Pred 1 (Pred (Elems (2))))))
    (args' ((Preds ()) (Preds ((Pred (Len 2)))))) |}]
