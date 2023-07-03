open Cad_ext

module Params = struct
  type t = {
    dim : Scene2d.Dim.t;
    max_repeat_count : int;
    distance : [ `Relative | `Jaccard ];
  }
  [@@deriving yojson]

  let default_max_repeat_count = 4
  let default_dim = Scene2d.Dim.create ()
  let default_distance = `Relative

  let create ?(max_repeat_count = default_max_repeat_count) ?(dim = default_dim)
      ?(distance = default_distance) () =
    { dim; max_repeat_count; distance }

  let param =
    let open Command.Let_syntax in
    let distance =
      Command.Arg_type.create @@ function
      | "relative" -> `Relative
      | "jaccard" -> `Jaccard
      | _ -> failwith "invalid distance type"
    in
    [%map_open
      let max_repeat_count =
        flag "max-repeat-count"
          (optional_with_default default_max_repeat_count int)
          ~doc:"maximum value of repeat count"
      and dim = Scene2d.Dim.param
      and distance =
        flag "distance"
          (optional_with_default default_distance distance)
          ~doc:"distance type"
      in
      create ~max_repeat_count ~dim ~distance ()]
end

let max_repeat_count = 4

let relative_distance ~target (v : Value.t) (v' : Value.t) =
  match (v, v') with
  | Scene x, Scene x' ->
      let target_scene = match target with Value.Scene t -> t | _ -> assert false in
      let n = Scene2d.(pixels @@ sub target_scene x)
      and n' = Scene2d.(pixels @@ sub target_scene x') in
      let p = Scene2d.(pixels @@ sub x target_scene)
      and p' = Scene2d.(pixels @@ sub x' target_scene) in
      let union = Bitarray.(O.(hamming_weight (n lor n') + hamming_weight (p lor p')))
      and inter =
        Bitarray.(O.(hamming_weight (n land n') + hamming_weight (p land p')))
      in
      assert (union >= inter && inter >= 0);
      if union = 0 then 0.0 else 1.0 -. (Float.of_int inter /. Float.of_int union)
  | v, v' -> if [%equal: Value.t] v v' then 0.0 else Float.infinity

let jaccard_distance = Value.distance

let rewrite dim =
  let res = max (Scene2d.Dim.xres dim) (Scene2d.Dim.yres dim) in
  function
  | Program.Apply (Op.Int x, []) ->
      if x <= 0 then [ Program.Apply (Op.Int (x + 1), []) ]
      else if x >= res then [ Apply (Int (x - 1), []) ]
      else [ Apply (Int (x + 1), []); Apply (Int (x - 1), []) ]
  | Apply (Rep_count x, []) ->
      if x <= 1 then [ Apply (Rep_count (x + 1), []) ]
      else if x >= max_repeat_count then [ Apply (Rep_count (x - 1), []) ]
      else [ Apply (Rep_count (x + 1), []); Apply (Rep_count (x - 1), []) ]
  | Apply (Circle, [ Apply (Int x, []); Apply (Int y, []); Apply (Int r, []) ]) ->
      [
        Apply
          ( Rect,
            [
              Apply (Int (x - r), []);
              Apply (Int (y - r), []);
              Apply (Int (x + r), []);
              Apply (Int (y + r), []);
            ] );
      ]
  | Apply
      ( Rect,
        [ Apply (Int lx, []); Apply (Int ly, []); Apply (Int hx, []); Apply (Int hy, []) ]
      )
    when hx - lx = hy - ly ->
      let r = (hx - lx) / 2 in
      [
        Apply
          ( Circle,
            [ Apply (Int (lx + r), []); Apply (Int (ly + r), []); Apply (Int r, []) ] );
      ]
  | _ -> []

let synthesize (metric_params : Metric_synth.Params.t) (dsl_params : Params.t) target =
  let dim = dsl_params.dim in
  let value_distance =
    match dsl_params.distance with
    | `Relative -> relative_distance ~target
    | `Jaccard -> jaccard_distance
  in
  let dsl =
    (module struct
      module Type = Type
      module Op = Op

      module Value = struct
        include Value

        let eval = eval ~dim ~error_on_trivial:true
        let distance = value_distance
        let target_distance ~target = Value.distance target
      end

      let operators = Op.default_operators ~xres:dim.xres ~yres:dim.yres
      let parse = parse
      let serialize = serialize
      let rewrite = rewrite dim
    end : Metric_synth.DSL
      with type Value.t = _
       and type Op.t = _)
  in
  Metric_synth.synthesize metric_params dsl target
