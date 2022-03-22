type 'a typ =
  | Int : int typ
  | Bool : bool typ
  | Float : float typ
  | String : string typ
  | Span : Time.Span.t typ
  | Opt : 'a typ -> 'a option typ

type field = Field : 'a typ * 'a -> field

let rec field_to_string (Field (t, x)) =
  match t with
  | Int -> sprintf "%d" x
  | Bool -> if x then "1" else "0"
  | Float -> sprintf "%f" x
  | String -> if String.contains x ',' then sprintf "\"%s\"" x else x
  | Span -> sprintf "%f" @@ Time.Span.to_ms x
  | Opt t' ->
      Option.map x ~f:(fun x' -> field_to_string (Field (t', x')))
      |> Option.value ~default:""

let to_string l = List.map l ~f:field_to_string |> String.concat ~sep:","
let print l = printf "%s\n" @@ to_string l
let string x = Field (String, x)
let int x = Field (Int, x)
let span x = Field (Span, x)
let float x = Field (Float, x)
let bool x = Field (Bool, x)
let optional_string x = Field (Opt String, x)
let optional_int x = Field (Opt Int, x)
let optional_bool x = Field (Opt Bool, x)
let optional_span x = Field (Opt Span, x)
