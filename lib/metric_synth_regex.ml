open Regex

module Params = struct
  type t = { max_int : int; sketch : string } [@@deriving yojson]

  let default_max_int = 15
  let create ?(max_int = default_max_int) ~sketch () = { max_int; sketch }

  let param =
    let open Command.Let_syntax in
    [%map_open
      let max_int =
        flag "max-int"
          (optional_with_default default_max_int int)
          ~doc:"maximum integer constant in regex"
      and sketch = flag "-sketch" (required string) ~doc:" regex sketch" in
      create ~max_int ~sketch ()]
end

let rewrite : int -> Op.t P.t -> Op.t P.t list =
 fun max_int -> function
  | Apply (Int x, []) ->
      if x <= 1 then [ Apply (Int (x + 1), []) ]
      else if x >= max_int then [ Apply (Int (x - 1), []) ]
      else [ Apply (Int (x + 1), []); Apply (Int (x - 1), []) ]
  | Apply (Repeat, [ r; n ]) -> [ Apply (Repeat_range, [ r; n; n ]) ]
  | _ -> []

let synthesize (metric_params : Metric_synth.Params.t) (dsl_params : Params.t)
    (vctx, operators) =
  let dsl =
    (module struct
      module Type = Type
      module Op = Op

      module Value = struct
        include Value

        let eval = eval vctx
        let target_distance = target_distance vctx
      end

      let parse = parse
      let serialize = serialize
      let rewrite = rewrite dsl_params.max_int
      let operators = operators
    end : Metric_synth.DSL
      with type Op.t = _)
  in
  Metric_synth.synthesize metric_params dsl
