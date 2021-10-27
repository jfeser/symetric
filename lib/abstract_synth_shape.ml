module Simple_abstract_value = struct
  module T = struct
    type t = Scene of (Shape.color option * int option) option Cow_array.t | Int of int | Color of Shape.color
    [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  module Ctx = struct
    type t = { n_pos : int; show_color : bool; show_sides : bool }

    let create n_pos = { n_pos; show_color = false; show_sides = false }

    let of_params _ = { n_pos = 5; show_color = false; show_sides = false }
  end

  let eval Ctx.{ n_pos; show_color; show_sides } (op : Shape.Op.t) args =
    match (op, args) with
    | Draw, [ Scene s; Int sides; Color color; Int pos ] ->
        let color = if show_color then Some color else None in
        let sides = if show_sides then Some sides else None in
        Scene Cow_array.(set s pos (Some (color, sides)))
    | Empty, [] -> Scene (Cow_array.of_array @@ Array.create ~len:n_pos None)
    | (Sides x | Position x), [] -> Int x
    | Color c, [] -> Color c
    | _ -> failwith "unexpected eval"

  let lift (ctx : Ctx.t) = function
    | Shape.Value.Color c -> Color c
    | Int x -> Int x
    | Scene s ->
        Scene
          (Array.init ctx.n_pos ~f:(fun i ->
               Cow_array.get s i
               |> Option.map ~f:(fun (c, s) ->
                      let c = Option.some_if ctx.show_color c and s = Option.some_if ctx.show_sides s in
                      (c, s)))
          |> Cow_array.of_array)

  let refine (ctx : Ctx.t) (target : Shape.Value.t) (v : Shape.Value.t) =
    match (target, v) with
    | Scene target, Scene v ->
        Iter.(0 -- (ctx.n_pos - 1))
        |> Iter.find_map (fun i ->
               match (Cow_array.get target i, Cow_array.get v i) with
               | Some (c, s), Some (c', s') ->
                   if not ([%compare.equal: Shape.color] c c') then Some { ctx with show_color = true }
                   else if s <> s' then Some { ctx with show_sides = true }
                   else None
               | _ -> None)
    | _ -> None

  let is_error _ = false
end

module Mask_abstract_value = struct
  module T = struct
    type t = Scene of (Shape.color option * int option) option Cow_array.t | Int of int | Color of Shape.color
    [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  module Ctx = struct
    type t = { n_pos : int; mask : (bool * bool) array }

    let create n_pos = { n_pos; mask = Array.create ~len:n_pos (false, false) }

    let of_params _ = create 5
  end

  let eval Ctx.{ n_pos; mask } (op : Shape.Op.t) args =
    match (op, args) with
    | Draw, [ Scene s; Int sides; Color color; Int pos ] ->
        let show_color, show_sides = mask.(pos) in
        let color = if show_color then Some color else None in
        let sides = if show_sides then Some sides else None in
        Scene Cow_array.(set s pos (Some (color, sides)))
    | Empty, [] -> Scene (Cow_array.of_array @@ Array.create ~len:n_pos None)
    | (Sides x | Position x), [] -> Int x
    | Color c, [] -> Color c
    | _ -> failwith "unexpected eval"

  let lift (ctx : Ctx.t) = function
    | Shape.Value.Color c -> Color c
    | Int x -> Int x
    | Scene s ->
        Scene
          (Array.init ctx.n_pos ~f:(fun i ->
               Cow_array.get s i
               |> Option.map ~f:(fun (c, s) ->
                      let show_color, show_sides = ctx.mask.(i) in
                      let c = Option.some_if show_color c and s = Option.some_if show_sides s in
                      (c, s)))
          |> Cow_array.of_array)

  let refine (ctx : Ctx.t) (target : Shape.Value.t) (v : Shape.Value.t) =
    match (target, v) with
    | Scene target, Scene v ->
        Iter.(0 -- (ctx.n_pos - 1))
        |> Iter.find_map (fun i ->
               match (Cow_array.get target i, Cow_array.get v i) with
               | Some (c, s), Some (c', s') ->
                   if not ([%compare.equal: Shape.color] c c') then (
                     let mask = Array.copy ctx.mask in
                     let _, show_sides = mask.(i) in
                     mask.(i) <- (true, show_sides);
                     Some { ctx with mask })
                   else if s <> s' then (
                     let mask = Array.copy ctx.mask in
                     let show_color, _ = mask.(i) in
                     mask.(i) <- (show_color, true);
                     Some { ctx with mask })
                   else None
               | _ -> None)
    | _ -> None

  let is_error _ = false
end

module Correct_mask_abstract_value = struct
  module T = struct
    type t = Scene of bool Cow_array.t | Int of int | Color of Shape.color [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  module Ctx = struct
    type t = { scene : Shape.Value.scene }

    let create scene = { scene }

    let of_params _ = failwith "unimplemented"
  end

  let eval Ctx.{ scene } (op : Shape.Op.t) args =
    match (op, args) with
    | Draw, [ Scene s; Int sides; Color color; Int pos ] ->
        Scene
          Cow_array.(
            set s pos ([%compare.equal: (Shape.color * int) option] (Some (color, sides)) (Cow_array.get scene pos)))
    | Empty, [] ->
        Scene
          (Cow_array.of_array
          @@ Array.init (Cow_array.length scene) ~f:(fun i -> Option.is_none @@ Cow_array.get scene i))
    | (Sides x | Position x), [] -> Int x
    | Color c, [] -> Color c
    | _ -> failwith "unexpected eval"

  let lift (ctx : Ctx.t) = function
    | Shape.Value.Color c -> Color c
    | Int x -> Int x
    | Scene s ->
        Scene (Cow_array.map2_exn s ctx.scene ~f:(fun v v' -> [%compare.equal: (Shape.color * int) option] v v'))

  let refine _ _ _ = None

  let is_error _ = false
end

let synth (target : Shape.Value.t) ops n_pos =
  let module Abs_value = Correct_mask_abstract_value in
  let module Abs_shape = struct
    include Shape
    module Value = Abs_value
  end in
  let exception Done in
  let ectx = Shape.Value.Ctx.{ n_pos } in
  let module Synth = Baseline.Make (Abs_shape) in
  let rec loop ctx =
    let ctx' =
      let abs_scene = Abs_value.lift ctx target in
      print_s [%message "abstract target" (abs_scene : Abs_value.t)];
      let sctx = Synth.Ctx.create ~max_cost:100 ctx ops (`Value abs_scene) in
      let synth = new Synth.synthesizer sctx in
      match synth#run with
      | Some p ->
          let v = Program.eval (Shape.Value.eval ectx) p in
          if [%compare.equal: Shape.Value.t] v target then (
            print_s [%message (p : Shape.Op.t Program.t)];
            raise Done)
          else Abs_value.refine ctx target v
      | None -> failwith "synthesis failed"
    in
    match ctx' with
    | Some ctx' ->
        print_s [%message "refined"];
        loop ctx'
    | None -> failwith "refinement failed"
  in
  let ctx = Abs_value.Ctx.create (match target with Scene s -> s | _ -> assert false) in
  try ignore (loop ctx : Abs_value.Ctx.t) with Done -> ()
