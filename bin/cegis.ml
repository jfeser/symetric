open! Core
open Staged_synth
module Gr = Grammar

module Sketch = struct
  let background = [ "S"; "V"; "C"; "CO"; "U"; "UOX"; "UOY"; "UOZ" ]

  let input = "V"

  let output = "E0"
end

module Code = Mlstage.Code
module Cad = Cad.Make (Code)

module Recon = struct
  type t =
    | Choose of t list
    | App of string * t list
    | Nonterm of string * Sexp.t
    | Input
  [@@deriving sexp]

  let rec to_program = function
    | Choose [] -> failwith "Empty choice."
    | Choose (x :: _) -> to_program x
    | App (f, args) -> Gr.App (f, List.map args ~f:to_program)
    | Nonterm (n, v) ->
        Nonterm (`Background (n, Cad.Lang.Value.of_sexp n (Code.Sexp.sexp v)))
    | Input -> Nonterm `Input
end

let examples_of_bench = function
  | Sexp.List (_ :: List [ List inputs ] :: rest) -> (
      match List.last_exn rest with
      | List outputs -> List.zip_exn inputs outputs
      | _ -> assert false )
  | _ -> assert false

let droplast l = List.rev l |> List.tl_exn |> List.rev

let bench_with_examples bench exs =
  let inputs, outputs = List.unzip exs in
  match bench with
  | Sexp.List (a :: List [ List _ ] :: b) ->
      Sexp.List (a :: List [ List inputs ] :: (droplast b @ [ List outputs ]))
  | _ -> assert false

let failing_examples examples prog =
  let prog, ctx = Gr.with_holes prog in
  List.filter examples ~f:(fun (inp, out) ->
      let ctx =
        List.map ctx ~f:(fun (value, _, name) ->
            match value with
            | `Background (_, v) -> (name, v)
            | `Input ->
                ( name,
                  Cad.Lang.Value.of_sexp "V"
                    (Code.Sexp.sexp (Sexp.List [ inp ])) ))
        |> Map.of_alist_exn (module String)
      in
      let out =
        Cad.Lang.Value.of_sexp "E" (Code.Sexp.sexp (Sexp.List [ out ]))
      in
      let out' = Cad.Lang.eval ctx prog in
      match Cad.Lang.Value.(out = out') with
      | `Static b -> not b
      | `Dyn b -> not (Code.to_bool b))

let main ~search_exe ~bench_fn =
  Synth.Log.set_level (Some Debug);
  let full_bench = In_channel.with_file bench_fn ~f:Sexp.input_sexp in
  let full_examples = examples_of_bench full_bench in

  let initial = List.take full_examples 10 in
  let rec loop examples =
    let bench = bench_with_examples full_bench examples in
    printf "Running synth with %d examples\n" (List.length examples);
    let Unix.Process_info.{ pid; stdin; stdout; _ } =
      Unix.create_process ~prog:search_exe ~args:[]
    in
    let stdin = Unix.out_channel_of_descr stdin
    and stdout = Unix.in_channel_of_descr stdout in
    Sexp.output stdin bench;
    Out_channel.close stdin;
    let prog = Lexing.from_channel stdout |> Sexp.scan_sexp in
    Unix.system @@ sprintf "kill %d" (Pid.to_int pid) |> ignore;
    print_endline "Got output...";
    let failing =
      failing_examples full_examples
      @@ Recon.to_program @@ [%of_sexp: Recon.t] @@ prog
    in
    printf "%d failing\n" (List.length failing);
    if List.length failing = 0 then print_s prog
    else loop (List.hd_exn failing :: examples)
  in
  loop initial

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Generate a CAD synthesizer."
    [%map_open
      let () = Log.param
      and search_exe = anon ("EXE" %: string)
      and bench_fn = anon ("BENCH" %: string) in
      fun () -> main ~search_exe ~bench_fn]
  |> Command.run
