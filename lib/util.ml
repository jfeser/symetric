open! Core

let clang_format src =
  let read, write = Unix.open_process "clang-format" in
  Out_channel.output_string write src;
  Out_channel.close write;
  let ret = In_channel.input_all read in
  ignore (Unix.close_process (read, write));
  ret
