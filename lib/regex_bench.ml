let load_examples_str s =
  let open Option.Let_syntax in
  String.split ~on:'\n' s |> Iter.of_list |> Iter.map String.strip
  |> Iter.drop_while (fun line ->
         not ([%equal: string] line "// examples" || [%equal: string] line "// example"))
  |> Iter.drop 1
  |> Iter.filter_map (fun line ->
         let len = String.length line in
         if len > 0 then
           let%map is_pos =
             match line.[len - 1] with '+' -> Some true | '-' -> Some false | _ -> None
           in
           (String.drop_suffix (String.drop_prefix line 1) 3, is_pos)
         else None)
  |> Iter.to_list

let load_examples ch = In_channel.input_all ch |> load_examples_str
let parse_program s = Regex_parser.sketch_eof Regex_lexer.token (Lexing.from_string s)

let load_ground_truth ch =
  let buf =
    In_channel.input_lines ch |> Iter.of_list |> Iter.map String.strip
    |> Iter.drop_while (fun line -> not ([%equal: string] line "// gt"))
    |> Iter.drop 1 |> Iter.head_exn |> Lexing.from_string
  in
  try Regex_parser.sketch_eof Regex_lexer.token buf
  with _ -> raise_s [%message "parse error" (buf.lex_curr_p.pos_cnum : int)]

let load_prompt str =
  String.split ~on:'\n' str |> Iter.of_list |> Iter.map String.strip
  |> Iter.drop_while (fun line -> not ([%equal: string] line "// natural language"))
  |> Iter.drop 1 |> Iter.head_exn

type t = Regex.Value.Ctx.t * Regex.Op.t list

let load_sketch_bench sketch_str bench_ch =
  let sketch_ast =
    let buf = Lexing.from_string sketch_str in
    try Regex_parser.sketch_eof Regex_lexer.token buf
    with _ -> raise_s [%message "parse error" (buf.lex_curr_p.pos_cnum : int)]
  in
  let sketches, n_holes = Regex_sketch_ast.convert_sketch sketch_ast in
  let examples = load_examples bench_ch in
  ( Regex.Value.Ctx.create examples n_holes,
    List.map ~f:(fun sk -> Regex.Op.Sketch sk) sketches )
