open Torch

let features fn =
  let model = Module.load fn in
  fun v -> Module.forward model [ v ]

let dist fn feat =
  let model = Module.load fn in
  fun v v' ->
    let x1 = Module.forward model [ feat v ] and x2 = Module.forward model [ feat v' ] in
    Tensor.(norm (x1 - x2) |> to_float0_exn)
