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
end
