open Owl

module Mat = struct
  include Mat

  let sexp_of_mat m = [%sexp_of: float array array] @@ to_arrays m
end

module Lego = struct
  type t = { dist : Mat.mat; (* Distance matrix (DxD) *) eta : float (* Regularization parameter (> 0) *) }

  let create ?(eta = 1.0) n_features = { dist = Mat.eye n_features; eta }

  (** Compute the distance between feature vectors.
      @param u, v : F x 1
      @return scalar distance *)
  let dist x u v =
    let z = Mat.(u - v) in
    Mat.((transpose z *@ x.dist *@ z).%{(0, 0)})

  (** Update the distance metric
      @param u, v : Fx1
      @param y : scalar
      @return New distance metric *)
  let update x u v y =
    let z = Mat.(u - v) in
    let y_est = Mat.((transpose z *@ x.dist *@ z).%{(0, 0)}) in
    let y_bar =
      Float.(
        let b = x.eta * y_est in
        let a = (y * b) - 1.0 in
        let num = a + sqrt ((a * a) + (4.0 * (x.eta * y_est * y_est))) in
        let dem = 2.0 * b in
        num / dem)
    in
    let y_diff = Float.(x.eta * (y_bar - y)) in
    let dist' =
      Mat.(
        let num = y_diff $* x.dist *@ z *@ transpose z *@ x.dist in
        let dem = Float.(1.0 + (y_diff * y_est)) in
        x.dist - (num /$ dem))
    in
    if Mat.not_nan dist' && Mat.not_inf dist' then { x with dist = dist' }
    else (
      Mat.print ~max_row:100 ~max_col:20 x.dist;
      failwith "bad update")

  let%expect_test "" =
    try
      let x = create 4 in
      Mat.print x.dist;
      print_s [%message (Mat.is_normal x.dist : bool)];

      let c1 = Mat.of_array [| 0.0; 1.0; 0.0; 0.0 |] 4 1
      and n1 = Mat.of_array [| 1.0; 0.0; 0.0; 0.0 |] 4 1
      and c2 = Mat.of_array [| 0.0; 0.0; 0.0; 1.0 |] 4 1
      and n2 = Mat.of_array [| 0.0; 0.0; 1.0; 0.0 |] 4 1 in

      let x = update x c1 n1 0.0 in
      Mat.print x.dist;
      print_s [%message (Mat.is_normal x.dist : bool)];
      let x = update x c2 n2 0.0 in
      Mat.print x.dist;
      print_s [%message (Mat.is_normal x.dist : bool)];
      let x = update x c1 c2 1.0 in
      Mat.print x.dist;
      print_s [%message (Mat.is_normal x.dist : bool)];

      Fmt.pr "c1 n1 %f\n" @@ dist x c1 n1;
      Fmt.pr "c1 n2 %f\n" @@ dist x c1 n2;
      Fmt.pr "c1 c2 %f\n" @@ dist x c1 c2;
      Fmt.pr "n1 n2 %f\n" @@ dist x n1 n2;
      Fmt.pr "n1 c2 %f\n" @@ dist x n1 c2;
      Fmt.pr "n2 c2 %f\n" @@ dist x n2 c2
    with exn ->
      print_string (Owl_exception.to_string exn);
      [%expect
        {|
         C0 C1 C2 C3
      R0  1  0  0  0
      R1  0  1  0  0
      R2  0  0  1  0
      R3  0  0  0  1
      ("Mat.is_normal x.dist" false)


               C0       C1 C2 C3
      R0 0.695194 0.304806  0  0
      R1 0.304806 0.695194  0  0
      R2        0        0  1  0
      R3        0        0  0  1
      ("Mat.is_normal x.dist" false)


               C0       C1       C2       C3
      R0 0.695194 0.304806        0        0
      R1 0.304806 0.695194        0        0
      R2        0        0 0.695194 0.304806
      R3        0        0 0.304806 0.695194
      ("Mat.is_normal x.dist" false)


                C0        C1        C2        C3
      R0  0.683651  0.278478 0.0115435 0.0263281
      R1  0.278478  0.635146 0.0263281 0.0600484
      R2 0.0115435 0.0263281  0.683651  0.278478
      R3 0.0263281 0.0600484  0.278478  0.635146
      ("Mat.is_normal x.dist" true)
      c1 n1 0.761841
      c1 n2 1.266140
      c1 c2 1.150195
      n1 n2 1.344214
      n1 c2 1.266140
      n2 c2 0.761841 |}]
end
