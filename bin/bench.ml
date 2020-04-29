open! Core
open Staged_synth

module type LANG = sig
  type 'a code

  module Value : sig
    type t

    type value

    val sexp_of : t -> Sexp.t code

    val random : ?state:Random.State.t -> Grammar.nonterm -> int -> t
  end

  val grammar : (Value.t, bool code) Semantics.t Grammar.t

  val eval : Value.t Map.M(String).t -> [ `Closed ] Grammar.Term.t -> Value.t
end

module Bench (Dsl : LANG with type 'a code = 'a Mlstage.Code.t) = struct
  module Code = Mlstage.Code

  type t = {
    min : int;
    max : int;
    examples : int;
    sketch : (module Sigs.SKETCH);
  }

  let generate_bench ?(state = Random.State.default) spec =
    let module Sketch = (val spec.sketch) in
    let grammar =
      List.mapi Sketch.background ~f:(fun i kind ->
          Grammar.(Rule.of_tuple (kind, Term.app (sprintf "input%d" i) [])))
      @ Dsl.grammar
    in

    Grammar.sample_seq ~state Sketch.output grammar
    |> Sequence.filter_map ~f:(fun t ->
           try
             let inputs =
               List.mapi Sketch.background ~f:(fun i kind ->
                   (sprintf "input%d" i, Dsl.Value.random ~state kind 5))
             in
             let ctx = Map.of_alist_exn (module String) inputs in
             let output = Dsl.eval ctx t in
             let sexps =
               List.map inputs ~f:(fun (_, v) -> v) @ [ output ]
               |> List.map ~f:(fun v -> Dsl.Value.sexp_of v |> Code.to_sexp)
             in
             Some sexps
           with _ -> None)
end

let main ~seed ~sketch ~min ~max ~examples ~no_identity =
  let sketch = In_channel.with_file sketch ~f:Util.input_sketch in
  let state =
    match seed with
    | Some s -> Random.State.make [| s |]
    | None -> Random.State.make_self_init ()
  in
  let module Deepcoder = Deepcoder.Make (Mlstage.Code.Array) (Mlstage.Code) in
  let module Bench = Bench (Deepcoder.Lang) in
  Bench.generate_bench ~state { min; max; examples; sketch }
  |> Sequence.filter ~f:(fun sexps ->
         if no_identity then
           match sexps with
           | [ s1; s2 ] -> not ([%compare.equal: Sexp.t] s1 s2)
           | _ -> true
         else true)
  |> Sequence.hd_exn |> List.iter ~f:print_s

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Generate a synthesizer."
    [%map_open
      let () = Log.param
      and seed = flag "-seed" (optional int) ~doc:" seed for the rng"
      and min =
        flag "-min" (optional_with_default 1 int) ~doc:" minimum term size"
      and max =
        flag "-max" (optional_with_default 10 int) ~doc:" maximum term size"
      and examples =
        flag "-examples"
          (optional_with_default 5 int)
          ~doc:" number of examples"
      and no_identity =
        flag "-no-identity" no_arg ~doc:" forbid the identity function"
      and sketch = anon ("SKETCH" %: string) in
      fun () -> main ~seed ~sketch ~min ~max ~examples ~no_identity]
  |> Command.run
