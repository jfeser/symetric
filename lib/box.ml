open Base_quickcheck

type is_open = Open | Closed [@@deriving compare, hash, sexp, quickcheck]

let is_closed = function Closed -> true | Open -> false

let is_open = Fun.negate is_closed

type bound = float [@@deriving compare, hash, sexp, quickcheck]

let quickcheck_generator_bound = Generator.float_inclusive (-50.0) 50.0

type t = {
  xmin : bound * is_open;
  xmax : bound * is_open;
  ymin : bound * is_open;
  ymax : bound * is_open;
}
[@@deriving compare, hash, sexp, quickcheck]

type conc = Vector2.t [@@deriving compare, hash, sexp]

let top =
  {
    xmin = Float.(-infinity, Closed);
    xmax = Float.(infinity, Closed);
    ymin = Float.(-infinity, Closed);
    ymax = Float.(infinity, Closed);
  }

let bot =
  {
    xmin = (0.0, Open);
    xmax = (0.0, Open);
    ymin = (0.0, Open);
    ymax = (0.0, Open);
  }

let ( <= ) (v, c) (v', c') =
  Float.(v < v' || (v = v' && (is_closed c || is_open c')))

let ( < ) (v, c) (v', c') =
  Float.(v < v' || (v = v' && (is_open c || is_open c')))

let is_empty b = b.xmax < b.xmin || b.ymax < b.ymin

let create ~xmin ~xmax ~ymin ~ymax =
  let ret = { xmin; xmax; ymin; ymax } in
  if is_empty ret then bot else ret

let quickcheck_generator =
  [%quickcheck.generator: t]
  |> Generator.map ~f:(fun a ->
         create ~xmin:a.xmin ~xmax:a.xmax ~ymin:a.ymin ~ymax:a.ymax)

let leq b b' =
  let ( <= ) (v, c) (v', c') =
    Float.(v < v' || (v = v' && (is_closed c || is_open c')))
  in
  [%compare.equal: t] b bot
  || b'.xmin <= b.xmin && b.xmax <= b'.xmax && b'.ymin <= b.ymin
     && b.ymax <= b'.ymax

let min (v, c) (v', c') =
  let open Float in
  ( min v v',
    if v < v' then c
    else if v' < v then c'
    else if is_closed c || is_closed c' then Closed
    else Open )

let max (v, c) (v', c') =
  let open Float in
  ( max v v',
    if v > v' then c
    else if v' > v then c'
    else if is_closed c || is_closed c' then Closed
    else Open )

let test_le x x' exp = [%test_result: bool] ~expect:exp (x <= x')

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

let is_subset a ~of_:b =
  let ( <= ) (v, c) (v', c') =
    Float.(v < v' || (v = v' && (is_closed c || is_open c')))
  in
  b.xmin <= a.xmin && a.xmax <= b.xmax && b.ymin <= a.ymin && a.ymax <= b.ymax

let lub a b =
  create ~xmin:(min a.xmin b.xmin) ~xmax:(max a.xmax b.xmax)
    ~ymin:(min a.ymin b.ymin) ~ymax:(max a.ymax b.ymax)

let glb a b =
  create ~xmin:(max a.xmin b.xmin) ~xmax:(min a.xmax b.xmax)
    ~ymin:(max a.ymin b.ymin) ~ymax:(min a.ymax b.ymax)

let contains a (v : conc) =
  let ( <. ) (v, c) v' =
    Float.(match c with Open -> v < v' | Closed -> v <= v')
  and ( >. ) (v, c) v' =
    Float.(match c with Open -> v > v' | Closed -> v >= v')
  in
  a.xmin <. v.x && a.xmax >. v.x && a.ymin <. v.y && a.ymax >. v.y

let graphviz_pp fmt
    ( {
        xmin = xminv, xminc;
        xmax = xmaxv, xmaxc;
        ymin = yminv, yminc;
        ymax = ymaxv, ymaxc;
      } as b ) =
  let pp_c fmt = function
    | Open -> Fmt.pf fmt "&lt;"
    | Closed -> Fmt.pf fmt "&lt;="
  in
  if [%compare.equal: t] b bot then Fmt.pf fmt "⊥"
  else
    Fmt.pf fmt "(%.0f%ax%a%.0f &amp; %.0f%ay%a%.0f)" xminv pp_c xminc pp_c xmaxc
      xmaxv yminv pp_c yminc pp_c ymaxc ymaxv

let split
    ({ xmin = xminv, _; xmax = xmaxv, _; ymin = yminv, _; ymax = ymaxv, _ } as b)
    =
  let open Float in
  let l = xmaxv - xminv and h = ymaxv - yminv in
  let xsplit = xminv + (l / 2.0) |> round_up
  and ysplit = yminv + (h / 2.0) |> round_up in
  [
    { b with xmax = (xsplit, Open); ymax = (ysplit, Open) };
    { b with xmin = (xsplit, Closed); ymax = (ysplit, Open) };
    { b with xmax = (xsplit, Open); ymin = (ysplit, Closed) };
    { b with xmin = (xsplit, Closed); ymin = (ysplit, Closed) };
  ]

let generate_between ((v, _) as p) ((v', _) as p') =
  let open Generator.Let_syntax in
  let gen =
    let%bind c'' = [%quickcheck.generator: is_open] in
    let%bind v'' =
      if Float.(v = v') then return v else Generator.float_inclusive v v'
    in
    return (v'', c'')
  in
  Generator.filter gen ~f:(fun p'' -> p <= p'' && p'' <= p')

(** Given a, return generator of b s.t. b <= a. *)
let quickcheck_generator_leq a =
  let open Generator.Let_syntax in
  if [%compare.equal: t] a bot then return bot
  else
    let%bind xmin = generate_between a.xmin a.xmax in
    let%bind xmax = generate_between xmin a.xmax in
    let%bind ymin = generate_between a.ymin a.ymax in
    let%bind ymax = generate_between ymin a.ymax in
    return @@ create ~xmin ~xmax ~ymin ~ymax
