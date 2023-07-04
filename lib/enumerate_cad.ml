open Std

(* let print_output m_prog = *)
(*   let program_size = *)
(*     Option.map m_prog ~f:(fun p -> Float.of_int @@ Program.size p) *)
(*     |> Option.value ~default:Float.nan *)
(*   in *)
(*   let program_json = *)
(*     Option.map m_prog ~f:(fun p -> `String (Sexp.to_string @@ Cad_ext.serialize p)) *)
(*     |> Option.value ~default:`Null *)
(*   in *)
(*   let open Yojson in *)
(*   Basic.to_channel Out_channel.stdout *)
(*     (`Assoc *)
(*       [ *)
(*         ("method", `String "enumeration"); *)
(*         ("scene_width", `Int (size ()).xres); *)
(*         ("scene_height", `Int (size ()).yres); *)
(*         ("program_size", `Float program_size); *)
(*         ("runtime", `Float (Time.Span.to_sec !runtime)); *)
(*         ("program", program_json); *)
(*       ]) *)

let synthesize (synth_params : Baseline.Params.t) (dsl_params : Cad_ext.Params.t) target =
  let module Dsl = struct
    include Cad_ext

    module Value = struct
      include Value

      let eval = eval ~error_on_trivial:true ~dim:dsl_params.dim
    end

    let operators =
      Cad_ext.Op.default_operators ~xres:dsl_params.dim.xres ~yres:dsl_params.dim.yres
  end in
  Baseline.synthesize (module Dsl) synth_params (`Value target)

(* let cmd = *)
(*   let open Command.Let_syntax in *)
(*   Command.basic ~summary:"Solve CAD problems with enumeration." *)
(*     [%map_open *)
(*       let synth_params = Baseline.Params.param and dsl_params = Cad_ext.Params.param in *)
(*       fun () -> *)
(*         let prog = Cad_ext.parse @@ Sexp.input_sexp In_channel.stdin in *)
(*         set_params ~dim ~verbose ?max_cost prog; *)
(*         synthesize () |> print_output] *)
