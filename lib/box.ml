type t = { xmin : float; xmax : float; ymin : float; ymax : float }
[@@deriving compare, hash, sexp]

let create ~xmin ~xmax ~ymin ~ymax =
  assert (Float.(xmin <= xmax && ymin <= ymax));
  { xmin; xmax; ymin; ymax }

let is_subset a ~of_:b =
  Float.(
    a.xmin >= b.xmin && a.xmax <= b.xmax && a.ymin >= b.ymin && a.ymax <= b.ymax)

let top =
  {
    xmin = Float.(-infinity);
    xmax = Float.(infinity);
    ymin = Float.(-infinity);
    ymax = Float.(infinity);
  }

let bot = create ~xmin:0.0 ~xmax:0.0 ~ymin:0.0 ~ymax:0.0

let lub a b =
  create ~xmin:(Float.min a.xmin b.xmin) ~xmax:(Float.max a.xmax b.xmax)
    ~ymin:(Float.min a.ymin b.ymin) ~ymax:(Float.max a.ymax b.ymax)

let glb a b =
  create ~xmin:(Float.max a.xmin b.xmin) ~xmax:(Float.min a.xmax b.xmax)
    ~ymin:(Float.max a.ymin b.ymin) ~ymax:(Float.min a.ymax b.ymax)

let contains a (x, y) =
  Float.(a.xmin <= x && x <= a.xmax && a.ymin <= y && y <= a.ymax)
