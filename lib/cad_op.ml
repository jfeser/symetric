type circle = { id : int; center : Vector2.t; radius : float } [@@deriving compare, equal, hash, sexp]
type rect = { id : int; lo_left : Vector2.t; hi_right : Vector2.t } [@@deriving compare, equal, hash, sexp]
type replicate = { id : int; count : int; v : Vector2.t } [@@deriving compare, equal, hash, sexp]

type op = Union | Inter | Circle of circle | Rect of rect | Replicate of replicate
[@@deriving compare, equal, hash, sexp]

module T = Hash_cached.Make (struct
  type t = op [@@deriving compare, equal, hash, sexp]
end)

include T
include Comparator.Make (T)

type type_ = Cad_type.t

let match_ ~union ~inter ~circle ~rect ~replicate x =
  match value x with
  | Union -> union ()
  | Inter -> inter ()
  | Circle x -> circle x
  | Rect x -> rect x
  | Replicate x -> replicate x

let pp fmt =
  match_
    ~union:(fun () -> Fmt.pf fmt "or")
    ~inter:(fun () -> Fmt.pf fmt "and")
    ~circle:(fun x -> Fmt.pf fmt "circle<%.1f, %.1f, %.1f>" x.center.x x.center.y x.radius)
    ~rect:(fun x -> Fmt.pf fmt "rect<%.1f, %.1f, %.1f, %.1f>" x.lo_left.x x.lo_left.y x.hi_right.x x.hi_right.y)
    ~replicate:(fun x -> Fmt.pf fmt "replicate<%d, %.1f, %.1f>" x.count x.v.x x.v.y)

let union = create Union
let inter = create Inter
let circle ~id ~center ~radius = create (Circle { id; center; radius })
let rect ~id ~lo_left ~hi_right = create (Rect { id; lo_left; hi_right })
let replicate ~id ~count ~v = create (Replicate { id; count; v })
let to_string = Fmt.to_to_string pp
let arity x = match value x with Circle _ | Rect _ -> 0 | Replicate _ -> 1 | Union | Inter -> 2
let cost _ = 1
let ret_type _ = Cad_type.Scene

let args_type x =
  match value x with Circle _ | Rect _ -> [] | Replicate _ -> [ Cad_type.Scene ] | Union | Inter -> [ Scene; Scene ]

let type_ x = (args_type x, ret_type x)

let center =
  let none _ = None in
  match_ ~union:none ~inter:none ~replicate:none
    ~circle:(fun c -> Some c.center)
    ~rect:(fun r ->
      Some
        Vector2.
          {
            x = r.lo_left.x +. ((r.hi_right.x -. r.lo_left.x) /. 2.0);
            y = r.lo_left.y +. ((r.hi_right.y -. r.lo_left.y) /. 2.0);
          })
