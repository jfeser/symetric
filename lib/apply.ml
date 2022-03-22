let apply2 f args =
  match args with
  | [ x; x' ] -> f x x'
  | _ -> raise_s [%message "Unexpected args" (List.length args : int)]

let apply6 f args =
  match args with
  | [ x1; x2; x3; x4; x5; x6 ] -> f x1 x2 x3 x4 x5 x6
  | _ -> raise_s [%message "Unexpected args" (List.length args : int)]
