type color = [ `Red | `Green | `Blue ] [@@deriving compare, equal, hash, sexp]

let colors = [ `Red; `Green; `Blue ]

module Type = struct
  module T = struct
    type t = Scene | Sides | Color | Position [@@deriving compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  let output = Scene
end

module Op = struct
  type type_ = Type.t

  module T = struct
    type t = Draw | Empty | Sides of int | Color of color | Position of int [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  let arity = function Draw -> 4 | Empty | Sides _ | Color _ | Position _ -> 0

  let args_type : _ -> Type.t list = function
    | Draw -> [ Scene; Sides; Color; Position ]
    | Empty | Sides _ | Color _ | Position _ -> []

  let ret_type : _ -> Type.t = function
    | Draw | Empty -> Scene
    | Sides _ -> Sides
    | Color _ -> Color
    | Position _ -> Position

  let type_ x = (args_type x, ret_type x)
end

module Value = struct
  module T = struct
    type t = Scene of (color * int) option Cow_array.t | Int of int | Color of color
    [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  module Ctx = struct
    type t = { n_pos : int }

    let of_params _ = { n_pos = 5 }
  end

  let eval Ctx.{ n_pos } (op : Op.t) args =
    match (op, args) with
    | Draw, [ Scene s; Int sides; Color color; Int pos ] -> Scene Cow_array.(set s pos (Some (color, sides)))
    | Empty, [] -> Scene (Cow_array.of_array @@ Array.create ~len:n_pos None)
    | (Sides x | Position x), [] -> Int x
    | Color c, [] -> Color c
    | _ -> failwith "unexpected eval"

  let dist Ctx.{ n_pos } s s' =
    match (s, s') with
    | Scene s, Scene s' ->
        if Cow_array.count s ~f:Option.is_some = Cow_array.count s' ~f:Option.is_some then
          let inter =
            Iter.(0 -- (n_pos - 1))
            |> Iter.map (fun i ->
                   let v = Cow_array.get s i and v' = Cow_array.get s' i in
                   match (v, v') with
                   | Some (c, n), Some (c', n') ->
                       (if n = n' then 1 else 0) + if [%compare.equal: color] c c' then 1 else 0
                   | None, Some _ | Some _, None -> 0
                   | None, None -> 2)
            |> Iter.sum
          in
          let union = 2 * n_pos in
          Float.(1.0 - (of_int inter / of_int union))
        else Float.infinity
    | _ -> Float.infinity

  let embed _ = failwith ""
end

let name = "shape"

module Bench = Bench.Make (Op) (Value)

let spec = Dumb_params.Spec.create ()

let bench = Dumb_params.Spec.add spec @@ Bench.param
