open Base_quickcheck

type is_open = Open | Closed [@@deriving compare, hash, sexp, quickcheck]

let is_closed = function Closed -> true | Open -> false
let is_open = Fun.negate is_closed

type bound = float [@@deriving compare, hash, sexp, quickcheck]

let quickcheck_generator_bound = Generator.float_inclusive (-50.0) 50.0

type box = {
  xmin : bound * is_open;
  xmax : bound * is_open;
  ymin : bound * is_open;
  ymax : bound * is_open;
}
[@@deriving compare, hash, sexp, quickcheck]

type t = Bot | Box of box [@@deriving compare, hash, sexp, quickcheck]
type conc = Vector2.t [@@deriving compare, hash, sexp]

let top =
  Box
    {
      xmin = Float.(-infinity, Closed);
      xmax = Float.(infinity, Closed);
      ymin = Float.(-infinity, Closed);
      ymax = Float.(infinity, Closed);
    }

let bot = Bot
let lo_lt (v, c) (v', c') = Float.(v < v' || (v = v' && (is_closed c || is_open c')))
let hi_lt (v, c) (v', c') = Float.(v < v' || (v = v' && (is_open c || is_closed c')))
let lo_le p p' = lo_lt p p' || [%compare.equal: bound * is_open] p p'
let hi_le p p' = hi_lt p p' || [%compare.equal: bound * is_open] p p'

let is_empty b =
  let is_empty_int (v, c) (v', c') =
    Float.(v > v' || (v = v' && is_open c && is_open c'))
  in
  is_empty_int b.xmin b.xmax || is_empty_int b.ymin b.ymax

let create ~xmin ~xmax ~ymin ~ymax =
  let ret = { xmin; xmax; ymin; ymax } in
  if is_empty ret then bot else Box ret

let quickcheck_generator =
  [%quickcheck.generator: box]
  |> Generator.map ~f:(fun a ->
         create ~xmin:a.xmin ~xmax:a.xmax ~ymin:a.ymin ~ymax:a.ymax)

let translate v = function
  | Bot -> Bot
  | Box b ->
      let add (x, c) x' = (x +. x', c) in
      create ~xmin:(add b.xmin v.Vector2.x) ~xmax:(add b.xmax v.Vector2.x)
        ~ymin:(add b.ymin v.Vector2.y) ~ymax:(add b.ymax v.Vector2.y)

let leq b b' =
  match (b, b') with
  | Bot, _ -> true
  | _, Bot -> false
  | Box b, Box b' ->
      lo_le b'.xmin b.xmin && hi_le b.xmax b'.xmax && lo_le b'.ymin b.ymin
      && hi_le b.ymax b'.ymax

let lo_min p p' = if lo_lt p p' then p else p'
let hi_min p p' = if hi_lt p p' then p else p'
let lo_max p p' = if lo_lt p p' then p' else p
let hi_max p p' = if hi_lt p p' then p' else p
let test_le x x' exp = [%test_result: bool] ~expect:exp @@ lo_le x x'
let test_lt x x' exp = [%test_result: bool] ~expect:exp (x < x')

let%test_unit "" = test_le (0.0, Closed) (0.0, Open) true
let%test_unit "" = test_le (0.0, Closed) (0.0, Closed) true
let%test_unit "" = test_le (0.0, Open) (0.0, Open) true
let%test_unit "" = test_le (0.0, Open) (0.0, Closed) false

let%test_unit "" =
  List.iter [ Closed; Open ] ~f:(fun c ->
      List.iter [ Closed; Open ] ~f:(fun c' ->
          test_le (0.0, c) (1.0, c') true;
          test_le (1.0, c) (0.0, c') false))

let create_closed ~xmin ~xmax ~ymin ~ymax =
  create ~xmin:(xmin, Closed) ~xmax:(xmax, Closed) ~ymin:(ymin, Closed)
    ~ymax:(ymax, Closed)

let is_subset a ~of_:b = leq a b

let lub a b =
  match (a, b) with
  | Bot, x | x, Bot -> x
  | Box a, Box b ->
      create ~xmin:(lo_min a.xmin b.xmin) ~xmax:(hi_max a.xmax b.xmax)
        ~ymin:(lo_min a.ymin b.ymin) ~ymax:(hi_max a.ymax b.ymax)

let glb a b =
  match (a, b) with
  | Bot, _ | _, Bot -> bot
  | Box a, Box b ->
      create ~xmin:(lo_max a.xmin b.xmin) ~xmax:(hi_min a.xmax b.xmax)
        ~ymin:(lo_max a.ymin b.ymin) ~ymax:(hi_min a.ymax b.ymax)

let%expect_test "" =
  glb
    (Box
       {
         xmin = (16.0, Closed);
         xmax = (18.0, Closed);
         ymin = (0.0, Closed);
         ymax = (4.0, Closed);
       })
    (Box
       {
         xmin = (8.0, Closed);
         xmax = (10.0, Closed);
         ymin = (0.0, Closed);
         ymax = (4.0, Closed);
       })
  |> [%sexp_of: t] |> print_s;
  [%expect {| Bot |}]

let contains a (v : conc) =
  match a with
  | Bot -> false
  | Box a ->
      let ( <. ) (v, c) v' = Float.(match c with Open -> v < v' | Closed -> v <= v')
      and ( >. ) (v, c) v' = Float.(match c with Open -> v > v' | Closed -> v >= v') in
      a.xmin <. v.x && a.xmax >. v.x && a.ymin <. v.y && a.ymax >. v.y

let graphviz_pp fmt =
  let pp_c fmt = function Open -> Fmt.pf fmt "&lt;" | Closed -> Fmt.pf fmt "≤" in
  function
  | Bot -> Fmt.pf fmt "⊥"
  | Box
      {
        xmin = xminv, xminc;
        xmax = xmaxv, xmaxc;
        ymin = yminv, yminc;
        ymax = ymaxv, ymaxc;
      } ->
      Fmt.pf fmt "(%.0f%ax%a%.0f &amp; %.0f%ay%a%.0f)" xminv pp_c xminc pp_c xmaxc xmaxv
        yminv pp_c yminc pp_c ymaxc ymaxv

let split = function
  | Bot -> failwith "cannot split"
  | Box ({ xmin = xminv, _; xmax = xmaxv, _; ymin = yminv, _; ymax = ymaxv, _ } as b) ->
      let open Float in
      let l = xmaxv - xminv and h = ymaxv - yminv in
      let xsplit = xminv + (l / 2.0) |> round_up
      and ysplit = yminv + (h / 2.0) |> round_up in
      [
        Box { b with xmax = (xsplit, Open); ymax = (ysplit, Open) };
        Box { b with xmin = (xsplit, Closed); ymax = (ysplit, Open) };
        Box { b with xmax = (xsplit, Open); ymin = (ysplit, Closed) };
        Box { b with xmin = (xsplit, Closed); ymin = (ysplit, Closed) };
      ]

let generate_between ((v, _) as p) ((v', _) as p') =
  let open Generator.Let_syntax in
  let gen =
    let%bind c'' = [%quickcheck.generator: is_open] in
    let%bind v'' = if Float.(v = v') then return v else Generator.float_inclusive v v' in
    return (v'', c'')
  in
  Generator.filter gen ~f:(fun p'' -> lo_le p p'' && hi_le p'' p')

(** Given a, return generator of b s.t. b <= a. *)
let quickcheck_generator_leq =
  Option.return @@ function
  | Bot -> Generator.return bot
  | Box a ->
      let open Generator.Let_syntax in
      let%bind xmin = generate_between a.xmin a.xmax in
      let%bind xmax = generate_between xmin a.xmax in
      let%bind ymin = generate_between a.ymin a.ymax in
      let%bind ymax = generate_between ymin a.ymax in
      return @@ create ~xmin ~xmax ~ymin ~ymax
