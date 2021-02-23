type circle = { id : int; center : Vector2.t; radius : float }
[@@deriving compare, hash, sexp]

module T = struct
  type t = Union | Inter | Circle of circle [@@deriving compare, hash, sexp]
end

include T
include Comparator.Make (T)

let pp fmt = function
  | Union -> Fmt.pf fmt "or"
  | Inter -> Fmt.pf fmt "and"
  | Circle x -> Fmt.pf fmt "circle_x%f_y%f_r%f" x.center.x x.center.y x.radius

let to_string = Fmt.to_to_string pp

let arity = function Circle _ -> 0 | Union | Inter -> 2

let ret_type _ = Cad_type.Scene

let args_type = function
  | Union | Inter -> [ Cad_type.Scene; Scene ]
  | Circle _ -> []

let type_ x = (args_type x, ret_type x)
