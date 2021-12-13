module Out = struct
  type t = { close : unit -> unit; flush : unit -> unit; output_substring : string -> pos:int -> len:int -> unit }

  let nop = { close = Fun.id; flush = Fun.id; output_substring = (fun _ ~pos:_ ~len:_ -> ()) }
  let close c = c.close ()
  let flush c = c.flush ()
  let output_substring c buf ~pos ~len = c.output_substring buf ~pos ~len
  let output_string c s = c.output_substring s ~pos:0 ~len:(String.length s)
  let newline c = output_string c "\n"

  let output_lines c lines =
    List.iter lines ~f:(fun line ->
        output_string c line;
        newline c)

  let of_out_channel ch =
    let close () = Out_channel.close ch in
    let flush () = Out_channel.flush ch in
    let output_substring buf ~pos ~len = Out_channel.output_substring ch ~buf ~pos ~len in
    { close; flush; output_substring }

  let of_buffer buf =
    let output_substring = Buffer.add_substring buf in
    { close = Fun.id; flush = Fun.id; output_substring }

  let to_formatter c =
    let out_string s pos len = output_substring c s ~pos ~len
    and out_flush () = flush c
    and out_newline () = newline c
    and out_spaces n = String.make n ' ' |> output_string c in
    let out_indent = out_spaces in
    Format.{ out_string; out_flush; out_newline; out_spaces; out_indent }

  let with_file ?binary ?append ?fail_if_exists ?perm fn f =
    Out_channel.with_file ?binary ?append ?fail_if_exists ?perm fn (fun ch -> f @@ of_out_channel ch)

  let tee c c' =
    let close () =
      close c;
      close c'
    in
    let flush () =
      flush c;
      flush c'
    in
    let output_substring s ~pos ~len =
      output_substring c s ~pos ~len;
      output_substring c' s ~pos ~len
    in
    { close; flush; output_substring }
end
