open Core
open Staged_synth
open Std

let () =
  Random.init 1;
  let n_pos = 6 and n_sides = 10 in
  let scene =
    Array.init n_pos ~f:(fun _ ->
        let has_shape = Float.(Random.float 1.0 < 0.3) in
        if has_shape then
          let color = Random.list_elem_exn [ `Red; `Green; `Blue ] in
          let sides = 1 + Random.int 10 in
          Some (color, sides)
        else None)
    |> Cow_array.of_array
  in
  let scene = Shape.Value.Scene scene in
  print_s [%message "concrete target" (scene : Shape.Value.t)];
  let ops =
    Shape.Op.[ Empty; Draw; Color `Red; Color `Green; Color `Blue ]
    @ List.init n_pos ~f:(fun p -> Shape.Op.Position p)
    @ List.init n_sides ~f:(fun s -> Shape.Op.Sides (s + 1))
  in
  print_s [%message (ops : Shape.Op.t list)];

  let (), local_time = Synth_utils.timed (fun () -> Local_synth_shape.synth scene ops n_pos) in
  let (), abs_time = Synth_utils.timed (fun () -> Abstract_synth_shape.synth scene ops n_pos) in
  Fmt.pr "Local: %a, Abs: %a" Time.Span.pp local_time Time.Span.pp abs_time
