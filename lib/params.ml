open Dumb_params

type t = Dumb_params.t

let spec = Spec.create ~name:"global" ()

let seed = Spec.add spec @@ Param.int ~name:"seed" ~doc:" random seed" ~init:(`Cli (Some 0)) ()

let print_json =
  Spec.add spec @@ Param.bool ~name:"print-json" ~doc:" print json when done" ~init:(`Cli (Some false)) ()

let runtime = Spec.add spec @@ Param.span_ref ~name:"runtime" ()

let get = Dumb_params.get
