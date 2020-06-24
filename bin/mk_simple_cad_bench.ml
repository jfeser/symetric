open! Core
open Staged_synth

module Core = Cstage_core.Make ()

module Code = struct
  module Core = Cstage_core.Make ()

  include Core
  include Cstage.Code (Core)
  module Set = Cstage_set.Hash_set (Core)
  module Array = Cstage_array.Array (Core)
  module Float = Cstage_float.Make (Core)
  module Tuple_3 = Cstage_tuple.Tuple_3.Make (Core)
  module Tuple_4 = Cstage_tuple.Tuple_4.Make (Core)
end

module C = Code

let main () =
  Synth.Log.set_level (Some Debug);
  let module Cad = struct
    include Cad.Make (C)

    module Lang0 = struct
      include Lang

      let grammar =
        List.filter grammar ~f:(fun rule ->
            match (Grammar.Rule.rhs rule :> Grammar.Untyped_term.t) with
            | App (("union" | "inter" | "sub"), _) -> false
            | _ -> true)
    end

    module Lang = Lang0
  end in
  let module Sketch = struct
    let background = [ "S"; "V"; "C"; "CO"; "U"; "UOX"; "UOY"; "UOZ" ]

    let input = "V"

    let output = "E"
  end in
  let open Synth.Make (Sketch) (C) (Cad.Lang) (Cad.Cache) in
  let synth =
    enumerate
      ~k:(fun () ->
        Cad.Cache.iter ~sym:"E" ~size:(C.Int.int 1) (cache.value ())
          ~f:(fun v -> Cad.Lang.Value.sexp_of v |> C.Sexp.print))
      1
  in
  print_endline (Code.to_string synth)

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Generate a CAD synthesizer."
    [%map_open
      let () = Log.param and () = Synth.param in
      fun () -> main ()]
  |> Command.run
