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

let rec height (Apply (_, args)) =
  let args_height =
    Option.value ~default:0
    @@ List.max_elt ~compare:[%compare: int]
    @@ List.map args ~f:height
  in
  1 + args_height

let%test_unit "" =
  [%test_result: int] ~expect:1 (height (Apply (0, [])));
  [%test_result: int] ~expect:2 (height (Apply (0, [ Apply (0, []); Apply (0, []) ])));
  [%test_result: int] ~expect:3
    (height (Apply (0, [ Apply (0, []); Apply (0, [ Apply (0, []) ]) ])))

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

module type Generate_lang_intf = sig
  module Type : sig
    type t [@@deriving compare]
  end

  module Op : sig
    type t [@@deriving sexp]

    val args_type : t -> Type.t list
    val ret_type : t -> Type.t
    val arity : t -> int
  end
end

let generate (type op type_)
    (module L : Generate_lang_intf with type Op.t = op and type Type.t = type_)
    ?(filter = fun _ -> true) (type_ : type_) ?(min_height = 1) ~max_height
    (ops : op list) =
  let open L in
  let open Option.Let_syntax in
  let rec gen min_height max_height type_ =
    assert (0 < min_height && min_height <= max_height);

    let valid_ops =
      List.filter ops ~f:(fun op ->
          [%compare.equal: Type.t] (Op.ret_type op) type_
          && (min_height <= 1 || Op.arity op > 0)
          && (max_height > 1 || Op.arity op = 0))
    in
    let%bind op = List.random_element valid_ops in
    let%bind args, _ =
      let args_min_height = Int.max 1 (min_height - 1) in
      let args_max_height = Int.max args_min_height (max_height - 1) in
      Op.args_type op
      |> List.mapi ~f:(fun i t -> (i, t))
      |> List.permute
      |> List.fold
           ~init:(Some ([], false))
           ~f:(fun acc (idx, type_) ->
             let%bind progs, min_sat = acc in
             let min = if min_sat then 1 else args_min_height in
             let%bind p = gen min args_max_height type_ in
             let min_sat = min_sat || height p >= args_min_height in
             return ((idx, p) :: progs, min_sat))
    in
    let args =
      List.sort args ~compare:(fun (i, _) (i', _) -> [%compare: int] i i')
      |> List.map ~f:Tuple.T2.get2
    in
    let prog = Apply (op, args) in
    if filter prog then return prog else None
  in
  let%map prog = gen min_height max_height type_ in
  let h = height prog in
  assert (min_height <= h && h <= max_height);
  prog

let%expect_test "" =
  let module L = struct
    module Type = struct
      type t = Int | Bool [@@deriving compare]
    end

    module Op = struct
      type t = Bool | Int | Plus | Eq [@@deriving sexp]

      let arity = function Bool | Int -> 0 | Plus | Eq -> 2
      let ret_type : _ -> Type.t = function Bool | Eq -> Bool | Plus | Int -> Int

      let args_type : _ -> Type.t list = function
        | Bool | Int -> []
        | Plus | Eq -> [ Int; Int ]
    end
  end in
  print_s
    [%message
      (generate (module L) L.Type.Int ~min_height:4 ~max_height:4 [ Bool; Int; Plus; Eq ]
        : L.Op.t t option)];
  [%expect
    {|
    ( "generate (module L) L.Type.Int ~min_height:4 ~max_height:4\
     \n  [Bool; Int; Plus; Eq]"
     ((Apply Plus
       ((Apply Int ())
        (Apply Plus
         ((Apply Plus ((Apply Int ()) (Apply Int ())))
          (Apply Plus ((Apply Int ()) (Apply Int ()))))))))) |}]
