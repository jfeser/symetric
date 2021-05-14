exception Eval_error of Sexp.t [@@deriving sexp]

type 'o t = Apply of 'o * 'o t list [@@deriving compare, hash, sexp]

let apply ?(args = []) op = Apply (op, args)

let rec eval oeval (Apply (op, args)) = oeval op (List.map args ~f:(eval oeval))

let rec size (Apply (_, args)) = 1 + List.sum (module Int) args ~f:size

let rec map ~f (Apply (op, args)) =
  let op' = f op in
  let args' = List.map args ~f:(map ~f) in
  Apply (op', args')

let rec iter ~f (Apply (op, args)) =
  f op;
  List.iter args ~f:(iter ~f)

let mapi p ~f =
  let idx = ref 0 in
  map
    ~f:(fun op ->
      let ret = f !idx op in
      incr idx;
      ret)
    p

let iteri p ~f =
  let idx = ref 0 in
  iter p ~f:(fun op ->
      f !idx op;
      incr idx)

let test_mapi_iteri p =
  let p' = mapi p ~f:(fun i _ -> i) in
  print_s @@ [%sexp_of: int t] p';
  iteri p' ~f:(fun i j -> [%test_result: int] ~expect:i j)

let%expect_test "" =
  test_mapi_iteri
    (Apply ((), [ Apply ((), []); Apply ((), [ Apply ((), []) ]) ]));
  [%expect {| (Apply 0 ((Apply 1 ()) (Apply 2 ((Apply 3 ()))))) |}]

let%expect_test "" =
  test_mapi_iteri
    (apply ()
       ~args:[ apply () ~args:[ apply () ~args:[ apply () ] ]; apply () ]);
  [%expect {| (Apply 0 ((Apply 1 ((Apply 2 ((Apply 3 ()))))) (Apply 4 ()))) |}]

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
