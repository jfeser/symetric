let load_examples ch =
  let open Option.Let_syntax in
  In_channel.input_lines ch |> Iter.of_list |> Iter.map String.strip
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

let load_sketch_bench sketch_str bench_ch =
  let sketch_ast =
    let buf = Lexing.from_string sketch_str in
    Regex_parser.sketch_eof Regex_lexer.token buf
  in
  let sketches, n_holes = Regex_sketch_ast.convert_sketch sketch_ast in
  let examples = load_examples bench_ch in
  ( Regex.Value.Ctx.{ input = examples; n_holes },
    List.map ~f:(fun sk -> Regex.Op.Sketch sk) sketches )
