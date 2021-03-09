type t = { concrete : bool }

let create ~concrete = { concrete }

let cli =
  let open Command.Let_syntax in
  [%map_open
    let concrete = flag "cad-concrete" no_arg ~doc:" disable abstraction" in

    create ~concrete]
