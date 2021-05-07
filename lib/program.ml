type 'o t = Apply of 'o * 'o t list [@@deriving compare, hash, sexp]

let apply ?(args = []) op = Apply (op, args)

let rec eval oeval (Apply (op, args)) = oeval op (List.map args ~f:(eval oeval))

let rec size (Apply (_, args)) = 1 + List.sum (module Int) args ~f:size

module Make (Op : sig
  type t [@@deriving compare, hash, sexp]
end) =
struct
  module T = struct
    type nonrec t = Op.t t [@@deriving compare, hash, sexp]
  end

  include T

  let eval_memoized oeval =
    let table = Hashtbl.create (module T) in
    let rec evalm p =
      match Hashtbl.find table p with
      | Some v -> v
      | None ->
          let (Apply (op, args)) = p in
          let v = oeval op @@ List.map args ~f:evalm in
          Hashtbl.set table ~key:p ~data:v;
          v
    in
    evalm
end
