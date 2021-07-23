open Owl

module Lego = struct
  type t = { dist : Mat.mat; (* Distance matrix (DxD) *) eta : float (* Regularization parameter (> 0) *) }

  let create ?(eta = 1.0) n_features = { dist = Mat.eye n_features; eta }

  let dist x u v =
    let z = Mat.(u - v) in
    Mat.(transpose z *@ x.dist *@ z)

  let update x u v y =
    let z = Mat.(u - v) in
    let y_est = Mat.(transpose z *@ x.dist *@ z) in
    let y_bar =
      Mat.(
        let b = x.eta $* y_est in
        let a = (y * b) -$ 1.0 in
        let num = a + sqrt ((a * a) + (4.0 $* (x.eta $* y_est * y_est))) in
        let dem = 2.0 $* b in
        num / dem)
    in
    let y_diff =
      Mat.(
        let d = y_bar - y in
        x.eta $* d)
    in
    let dist' =
      Mat.(
        let num = y_diff * (x.dist *@ z *@ transpose z *@ x.dist) in
        let dem = 1.0 $+ y_diff * y_est in
        x.dist - (num / dem))
    in
    { x with dist = dist' }

  let%expect_test "" =
    try
      let x = create 4 in
      let c1 = Mat.of_array [| 0.0; 1.0; 0.0; 0.0 |] 4 1
      and n1 = Mat.of_array [| 1.0; 0.0; 0.0; 0.0 |] 4 1
      and c2 = Mat.of_array [| 0.0; 0.0; 0.0; 1.0 |] 4 1
      and n2 = Mat.of_array [| 0.0; 0.0; 1.0; 0.0 |] 4 1 in

      let x = update x c1 n1 (Mat.of_array [| 0.0 |] 1 1) in
      let x = update x c2 n2 (Mat.of_array [| 0.0 |] 1 1) in
      let x = update x c1 c2 (Mat.of_array [| 1.0 |] 1 1) in

      Fmt.pr "c1 n1\n%a\n" Owl_pretty.pp_dsnda @@ dist x c1 n1;
      Fmt.pr "c1 n2\n%a\n" Owl_pretty.pp_dsnda @@ dist x c1 n2;
      Fmt.pr "c1 c2\n%a\n" Owl_pretty.pp_dsnda @@ dist x c1 c2;
      Fmt.pr "n1 n2\n%a\n" Owl_pretty.pp_dsnda @@ dist x n1 n2;
      Fmt.pr "n1 c2\n%a\n" Owl_pretty.pp_dsnda @@ dist x n1 c2;
      Fmt.pr "n2 c2\n%a\n" Owl_pretty.pp_dsnda @@ dist x n2 c2
    with exn -> print_string (Owl_exception.to_string exn)
end
