open Std

module T = struct
  type 'o t = Apply of 'o * 'o t list
end

type 'o t = 'o T.t = Apply of 'o * 'o t list
[@@deriving compare, equal, hash, sexp, yojson]

let rec pp pp_op fmt (Apply (op, args)) =
  if List.is_empty args then pp_op fmt op
  else Fmt.pf fmt "@[<hov 2>%a(@,%a@,)@]" pp_op op Fmt.(list ~sep:comma @@ pp pp_op) args

let apply ?(args = []) op = Apply (op, args)
let rec eval oeval (Apply (op, args)) = oeval op (List.map args ~f:(eval oeval))
let rec size (Apply (_, args)) = 1 + List.sum (module Int) args ~f:size
let rec ops (Apply (op, args)) = op :: List.concat_map args ~f:ops

let rec map ~f (Apply (op, args)) =
  let op' = f op args in
  let args' = List.map args ~f:(map ~f) in
  Apply (op', args')

let[@specialize] rec iter (Apply (op, args)) f =
  f (op, args);
  List.iter args ~f:(fun p -> iter p f)

let mapi ~f p =
  let idx = ref 0 in
  map
    ~f:(fun op _ ->
      let ret = f !idx op in
      incr idx;
      ret)
    p

let iteri ~f p =
  let idx = ref 0 in
  iter p (fun (op, _) ->
      f !idx op;
      incr idx)

let test_mapi_iteri p =
  let p' = mapi p ~f:(fun i _ -> i) in
  print_s @@ [%sexp_of: int t] p';
  iteri p' ~f:(fun i j -> [%test_result: int] ~expect:i j)

let%expect_test "" =
  test_mapi_iteri (Apply ((), [ Apply ((), []); Apply ((), [ Apply ((), []) ]) ]));
  [%expect {| (Apply 0 ((Apply 1 ()) (Apply 2 ((Apply 3 ()))))) |}]

let%expect_test "" =
  test_mapi_iteri
    (apply () ~args:[ apply () ~args:[ apply () ~args:[ apply () ] ]; apply () ]);
  [%expect {| (Apply 0 ((Apply 1 ((Apply 2 ((Apply 3 ()))))) (Apply 4 ()))) |}]

let rec subprograms p k =
  k p;
  let (Apply (_, args)) = p in
  List.iter args ~f:(fun p' -> subprograms p' k)

let rec commutative_closure ~is_commutative (Apply (op, args)) =
  let args_iter =
    let iters = List.map args ~f:(commutative_closure ~is_commutative) in
    if is_commutative op then
      Iter.append (Iter.list_product iters) (Iter.list_product @@ List.rev iters)
    else Iter.list_product iters
  in
  Iter.map (fun args -> Apply (op, args)) args_iter

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
