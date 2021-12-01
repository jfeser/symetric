exception Eval_error of Sexp.t [@@deriving sexp]

module T = struct
  type 'o t = Apply of 'o * 'o t list
end

type 'o t = 'o T.t = Apply of 'o * 'o t list [@@deriving compare, equal, hash, sexp]

let rec pp pp_op fmt (Apply (op, args)) =
  if List.is_empty args then pp_op fmt op
  else Fmt.pf fmt "@[<hov 2>%a(@,%a@,)@]" pp_op op Fmt.(list ~sep:comma @@ pp pp_op) args

let apply ?(args = []) op = Apply (op, args)

let rec eval oeval (Apply (op, args)) = oeval op (List.map args ~f:(eval oeval))

let eval_parts oeval p =
  let parts = Queue.create () in
  let rec eval oeval (Apply (op, args)) =
    let part = oeval op (List.map args ~f:(eval oeval)) in
    Queue.enqueue parts part;
    part
  in
  ignore (eval oeval p : _);
  Queue.to_list parts

let rec size (Apply (_, args)) = 1 + List.sum (module Int) args ~f:size

let rec count ~f (Apply (op, args)) = (if f op then 1 else 0) + List.sum (module Int) args ~f:(count ~f)

let rec ops (Apply (op, args)) = op :: List.concat_map args ~f:ops

let rec ops_iter (Apply (op, args)) f =
  f op;
  List.iter args ~f:(fun a -> ops_iter a f)

let rec map_preorder ~f (Apply (op, args)) =
  let op' = f op in
  let args' = List.map args ~f:(map_preorder ~f) in
  Apply (op', args')

let rec map_postorder ~f (Apply (op, args)) =
  let args' = List.map args ~f:(map_postorder ~f) in
  let op' = f op in
  Apply (op', args')

let map ?(order = `Pre) = match order with `Pre -> map_preorder | `Post -> map_postorder

let rec iter_preorder ~f (Apply (op, args)) =
  f op;
  List.iter args ~f:(iter_preorder ~f)

let rec iter_postorder ~f (Apply (op, args)) =
  List.iter args ~f:(iter_postorder ~f);
  f op

let iter ?(order = `Pre) = match order with `Pre -> iter_preorder | `Post -> iter_postorder

let mapi ?(order = `Pre) ~f p =
  let idx = ref 0 in
  map ~order
    ~f:(fun op ->
      let ret = f !idx op in
      incr idx;
      ret)
    p

let annotate ?(order = `Pre) p = mapi ~order p ~f:(fun i op -> (i, op))

let iteri ?(order = `Pre) ~f p =
  let idx = ref 0 in
  iter ~order
    ~f:(fun op ->
      f !idx op;
      incr idx)
    p

let test_mapi_iteri p =
  let p' = mapi p ~f:(fun i _ -> i) in
  print_s @@ [%sexp_of: int t] p';
  iteri p' ~f:(fun i j -> [%test_result: int] ~expect:i j)

let%expect_test "" =
  test_mapi_iteri (Apply ((), [ Apply ((), []); Apply ((), [ Apply ((), []) ]) ]));
  [%expect {| (Apply 0 ((Apply 1 ()) (Apply 2 ((Apply 3 ()))))) |}]

let%expect_test "" =
  test_mapi_iteri (apply () ~args:[ apply () ~args:[ apply () ~args:[ apply () ] ]; apply () ]);
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
