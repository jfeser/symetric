type 'o t = Apply of 'o * 'o t list [@@deriving compare, hash, sexp]

let apply ?(args=[]) op = Apply (op, args)

let rec eval oeval (Apply (op, args)) = oeval op (List.map args ~f:(eval oeval))

let rec size (Apply (_, args)) = 1 + List.sum (module Int) args ~f:size
