open Std
open Cad_ext
module Synth = Baseline.Make (Cad_ext)

module Params = struct
  type t = {
    size : Scene2d.Dim.t;
    ectx : Value.Ctx.t;
    target : Scene2d.t;
    operators : Op.t list;
    verbose : bool;
  }
end

let params = Set_once.create ()
let[@inline] size () = (Set_once.get_exn params [%here]).Params.size
let[@inline] ectx () = (Set_once.get_exn params [%here]).Params.ectx
let[@inline] target () = (Set_once.get_exn params [%here]).Params.target
let[@inline] operators () = (Set_once.get_exn params [%here]).Params.operators
let[@inline] verbose () = (Set_once.get_exn params [%here]).Params.verbose
let runtime = ref Time.Span.zero

let synthesize () =
  let start_time = Time.now () in

  let ectx = ectx ()
  and operators = operators ()
  and target = target ()
  and verbose = verbose () in
  let ctx = Synth.Ctx.create ~verbose ectx operators (`Value (Value.Scene target)) in
  let synth = new Synth.synthesizer ctx in
  let ret = synth#run in
  runtime := Time.diff (Time.now ()) start_time;
  ret

let print_output m_prog =
  let program_size =
    Option.map m_prog ~f:(fun p -> Float.of_int @@ Program.size p)
    |> Option.value ~default:Float.nan
  in
  let program_json =
    Option.map m_prog ~f:(fun p -> `String (Sexp.to_string @@ Cad_ext.serialize p))
    |> Option.value ~default:`Null
  in
  let open Yojson in
  Basic.to_channel Out_channel.stdout
    (`Assoc
      [
        ("method", `String "enumeration");
        ("scene_width", `Int (size ()).xres);
        ("scene_height", `Int (size ()).yres);
        ("program_size", `Float program_size);
        ("runtime", `Float (Time.Span.to_sec !runtime));
        ("program", program_json);
      ])

let set_params ~dim:size ~verbose target =
  let ectx = Value.Ctx.create size in
  let target_value = Program.eval (Value.eval ectx) target in
  let target_scene = match target_value with Scene s -> s | _ -> assert false in
  let operators = Op.default_operators ~xres:size.xres ~yres:size.yres in

  Set_once.set_exn params [%here]
    Params.{ size; ectx; target = target_scene; operators; verbose }

let cmd =
  let open Command.Let_syntax in
  Command.basic ~summary:"Solve CAD problems with enumeration."
    [%map_open
      let dim = Scene2d.Dim.param
      and verbose = flag "-verbose" no_arg ~doc:" increase verbosity" in
      fun () ->
        let prog = Cad_ext.parse @@ Sexp.input_sexp In_channel.stdin in
        set_params ~dim ~verbose prog;
        synthesize () |> print_output]
