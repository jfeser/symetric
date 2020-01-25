open! Core
module Seq = Sequence

module Code () : Sigs.CODE = struct
  let sexp_of_t _ = [%sexp_of: expr]

  let type_of e = e.etype

  let cast x = x

  let let_global v b =
    let g = fresh_global v.etype in
    let x = seq (assign v ~to_:g) g in
    let y = b { x with ebody = "" } in
    {
      y with
      ebody = x.ebody ^ y.ebody;
      efree = x.efree @ y.efree;
      eeffect = x.eeffect || y.eeffect;
    }
end
