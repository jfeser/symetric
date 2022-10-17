open! Core
open Symetric
open Std
open Cad_ext

let ctx =
  Value.Ctx.create ~unsound_pruning:true
    (Scene2d.Dim.create ~xres:16 ~yres:16 ~scaling:2 ())

let eval = Program.eval (Value.eval ctx)

let mean_primitive_distance prog =
  let target = eval prog in
  Program.subprograms prog |> Iter.drop 1
  |> Iter.filter_map (fun p ->
         match Program.eval (Value.eval ctx) p with
         | Scene _ as v ->
             let d = Value.distance v target in
             let size = Program.size p in
             Some (d, size)
         | _ -> None)
  |> Iter.filter (fun (_, size) -> size <= 5)
  |> Iter.map (fun (d, _) -> d)
  |> Iter.mean

let filter p =
  match eval p with Error -> false | Scene s -> Scene2d.hamming_weight s > 0 | _ -> true

let filter_visual_weight p =
  match eval p with
  | Scene s ->
      let w =
        Float.of_int (Scene2d.hamming_weight s) /. Float.of_int (Scene2d.npixels s)
      in
      w >. 0.5 && w <. 0.7
  | _ -> true

let main ~n ~out_dir =
  let open Option.Let_syntax in
  Random.init 0;
  let ops = Cad_ext.Op.default_operators ~xres:16 ~yres:16 in
  Iter.forever (fun () ->
      Program.generate
        (module Cad_ext)
        ~filter Cad_ext.Type.Scene ~min_height:7 ~max_height:10 ops)
  |> Iter.filter_map (fun p_opt ->
         let%bind p = p_opt in
         let s = Program.size p in
         if s < 35 || s > 45 then (
           Fmt.epr "Too small: %d\n%!" s;
           None)
         else
           let%bind d = mean_primitive_distance p in
           return (p, d))
  |> Iter.take n
  |> Iter.iteri (fun i (p, d) ->
         print_s
           [%message (p : Cad_ext.Op.t Program.t) (d : float) (Program.size p : int)];
         Fmt.pr "%a@." Value.pp (eval p);

         Sexp.save_hum (sprintf "%s/bench_%d" out_dir i) (Cad_ext.serialize p))

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Print a CAD benchmark."
    [%map_open
      let n = flag "n" ~doc:" number of benchmarks" (required int)
      and out_dir = flag "o" ~doc:" output directory" (required string) in
      fun () -> main ~n ~out_dir]
  |> Command_unix.run
