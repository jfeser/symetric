open! Core
open Symetric
open Std
open Cad_ext

let dim = Scene2d.Dim.create ~xres:16 ~yres:16 ~scaling:2 ()
let eval = Program.eval (Value.eval ~dim ~error_on_trivial:true)

let mean_primitive_distance prog =
  let target = eval prog in
  Program.subprograms prog |> Iter.drop 1
  |> Iter.filter_map (fun p ->
         match eval p with
         | Scene _ as v ->
             let d = Value.distance v target in
             let size = Program.size p in
             Some (d, size)
         | _ -> None)
  |> Iter.filter (fun (_, size) -> size <= 5)
  |> Iter.map (fun (d, _) -> d)
  |> Iter.mean

let filter out _ args =
  match out with
  | Value.Error -> false
  | Scene s -> Scene2d.hamming_weight s > 0
  | _ -> not (List.mem ~equal:Value.equal args out)

(* let filter_visual_weight = function *)
(*   | Scene s -> *)
(*       let w = *)
(*         Float.of_int (Scene2d.hamming_weight s) /. Float.of_int (Scene2d.npixels s) *)
(*       in *)
(*       w >. 0.5 && w <. 0.7 *)
(*   | _ -> true *)

let main ~n ~out_dir ?(min_nodes = Int.min_value) ?(max_nodes = Int.max_value) () =
  Random.init 0;
  let module Dsl = struct
    include Cad_ext

    module Value = struct
      include Value

      let eval = Value.eval ~dim ~error_on_trivial:true
    end

    let operators = Op.default_operators ~xres:16 ~yres:16
  end in
  (* Iter.forever (fun () -> *)
  (*     Program.generate *)
  (*       (module Cad_ext) *)
  (*       ~filter Cad_ext.Type.Scene ~min_height ~max_height ops) *)
  (* |> Iter.filter_map (fun p_opt -> *)
  (*        let%bind p = p_opt in *)
  (*        let s = Program.size p in *)
  (*        if s < min_nodes then ( *)
  (*          Fmt.epr "Too small: %d\n%!" s; *)
  (*          None) *)
  (*        else if s > max_nodes then ( *)
  (*          Fmt.epr "Too large: %d\n%!" s; *)
  (*          None) *)
  (*        else *)
  (*          let%bind d = mean_primitive_distance p in *)
  (*          return (p, d)) *)
  Iter.forever (fun () ->
      Program_generate.generate
        (module Dsl)
        ~min_cost:min_nodes ~max_cost:max_nodes ~n_per_level:25 ~filter)
  |> Iter.filter_map Fun.id |> Iter.take n
  |> Iter.iteri (fun i p ->
         print_s [%message (p : Cad_ext.Op.t Program.t) (Program.size p : int)];
         Fmt.pr "%a@." Value.pp (eval p);

         Sexp.save_hum (sprintf "%s/bench_%d" out_dir i) (Cad_ext.serialize p))

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Print a CAD benchmark."
    [%map_open
      let n = flag "n" ~doc:" number of benchmarks" (required int)
      and min_nodes = flag "min-nodes" ~doc:" minimum number of nodes" (optional int)
      and max_nodes = flag "max-nodes" ~doc:" maximum number of nodes" (optional int)
      and out_dir = flag "o" ~doc:" output directory" (required string) in
      main ~n ~out_dir ?min_nodes ?max_nodes]
  |> Command_unix.run
