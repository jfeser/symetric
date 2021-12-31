open Std
open Cad_ext
module Synth = Baseline.Make (Cad_ext)

module Params = struct
  type t = {
    size : Scene.Size.t;
    ectx : Value.Ctx.t;
    target : Scene.t;
    operators : Op.t list;
    filter : bool;
    max_cost : int;
    verbose : bool;
  }
end

let params = Set_once.create ()
let[@inline] size () = (Set_once.get_exn params [%here]).Params.size
let[@inline] ectx () = (Set_once.get_exn params [%here]).Params.ectx
let[@inline] target () = (Set_once.get_exn params [%here]).Params.target
let[@inline] operators () = (Set_once.get_exn params [%here]).Params.operators
let[@inline] filter () = (Set_once.get_exn params [%here]).Params.filter
let[@inline] max_cost () = (Set_once.get_exn params [%here]).Params.max_cost
let[@inline] verbose () = (Set_once.get_exn params [%here]).Params.verbose
let runtime = ref Time.Span.zero

let synthesize () =
  let start_time = Time.now () in

  let ectx = ectx ()
  and operators = operators ()
  and target = target ()
  and verbose = verbose ()
  and max_cost = max_cost () in
  let ctx =
    Synth.Ctx.create ~verbose ~max_cost ectx operators (`Value (Value.Scene target))
  in
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
        ("filter", `Bool (filter ()));
        ("max_cost", `Int (max_cost ()));
        ("program_size", `Float program_size);
        ("runtime", `Float (Time.Span.to_sec !runtime));
        ("program", program_json);
      ])

let set_params ~scene_width ~scene_height ~max_cost ~filter ~verbose target =
  let size = Scene.Size.create ~xres:scene_width ~yres:scene_height () in
  let ectx = Value.Ctx.create size in
  let target_value = Program.eval (Value.eval ectx) target in
  let target_scene = match target_value with Scene s -> s | _ -> assert false in
  let operators =
    Op.[ Union; Circle; Rect; Repl; Sub ]
    @ (List.range 0 (max size.xres size.yres) |> List.map ~f:(fun i -> Op.Int i))
    @ (List.range 2 5 |> List.map ~f:(fun i -> Op.Rep_count i))
  in

  Set_once.set_exn params [%here]
    Params.{ size; ectx; target = target_scene; operators; max_cost; filter; verbose }

let cmd =
  let open Command.Let_syntax in
  Command.basic ~summary:"Solve CAD problems with enumeration."
    [%map_open
      let max_cost =
        flag "-cost"
          (optional_with_default 22 int)
          ~doc:" the maximum size of program to evaluate"
      and filter =
        flag "-use-filter"
          (optional_with_default true bool)
          ~doc:" use heuristic filtering"
      and scene_width =
        flag "-scene-width" (optional_with_default 12 int) ~doc:" scene width in pixels"
      and scene_height =
        flag "-scene-height" (optional_with_default 20 int) ~doc:" scene height in pixels"
      and verbose = flag "-verbose" no_arg ~doc:" increase verbosity" in
      fun () ->
        let prog = Cad_ext.parse @@ Sexp.input_sexp In_channel.stdin in
        set_params ~scene_width ~scene_height ~max_cost ~filter ~verbose prog;
        synthesize () |> print_output]
