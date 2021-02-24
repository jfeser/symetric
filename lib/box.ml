type t = { xmin : float; xmax : float; ymin : float; ymax : float }
[@@deriving compare, hash, sexp]

let top =
  {
    xmin = Float.(-infinity);
    xmax = Float.(infinity);
    ymin = Float.(-infinity);
    ymax = Float.(infinity);
  }

let bot = { xmin = 1.0; xmax = 0.0; ymin = 1.0; ymax = 0.0 }

let create ~xmin ~xmax ~ymin ~ymax =
  if Float.(xmin > xmax || ymin > ymax) then bot else { xmin; xmax; ymin; ymax }

let is_subset a ~of_:b =
  Float.(
    a.xmin >= b.xmin && a.xmax <= b.xmax && a.ymin >= b.ymin && a.ymax <= b.ymax)

let lub a b =
  create ~xmin:(Float.min a.xmin b.xmin) ~xmax:(Float.max a.xmax b.xmax)
    ~ymin:(Float.min a.ymin b.ymin) ~ymax:(Float.max a.ymax b.ymax)

let glb a b =
  create ~xmin:(Float.max a.xmin b.xmin) ~xmax:(Float.min a.xmax b.xmax)
    ~ymin:(Float.max a.ymin b.ymin) ~ymax:(Float.min a.ymax b.ymax)

let contains a (v : Vector2.t) =
  Float.(a.xmin <= v.x && v.x <= a.xmax && a.ymin <= v.y && v.y <= a.ymax)
