open! Core

let clang_format src =
  let read, write = Unix.open_process "clang-format-9" in
  Out_channel.output_string write src;
  Out_channel.close write;
  let ret = In_channel.input_all read in
  ignore (Unix.close_process (read, write) : _ result);
  ret

let clang_build ?(args = "-std=c++17 -Wall -Wextra -c") src =
  let read, write = Unix.open_process (sprintf "clang++-9 %s -x c++ -" args) in
  Out_channel.output_string write src;
  Out_channel.close write;
  let ret = In_channel.input_all read in
  ignore (Unix.close_process (read, write) : _ result);
  ret

let clang_exec ?(args = "-Wall -Wextra -fsanitize=undefined -fsanitize=address -std=c++17") ?input src =
  let main = "main.cpp" in
  Out_channel.with_file main ~f:(fun ch -> Out_channel.output_string ch src);

  let exe = Filename.temp_file "test" ".exe" in
  let compiler_output =
    let read, write = Unix.open_process (sprintf "clang++-9 %s -Ietc sexp.cpp %s -o %s" args main exe) in
    Out_channel.output_string write src;
    Out_channel.close write;
    let out = In_channel.input_all read in
    ignore (Unix.close_process (read, write) : _ result);
    out
  in

  let exe_output =
    let read, write = Unix.open_process exe in
    Option.iter input ~f:(Out_channel.output_string write);
    Out_channel.close write;
    let out = In_channel.input_all read in
    ignore (Unix.close_process (read, write) : _ result);
    out
  in

  object
    method compiler_output = compiler_output

    method exe_output = exe_output
  end

module Cont = struct
  module T = struct
    type ('a, 'r) t = { runCont : ('a -> 'r) -> 'r }

    let return a = { runCont = (fun k -> k a) }

    let bind { runCont = g } ~f = { runCont = (fun k -> g (fun a -> (f a).runCont k)) }

    let map = `Define_using_bind
  end

  include T
  include Monad.Make2 (T)

  let ( let* ) x f = bind x ~f
end

module OneShot = struct
  module T = struct
    type ('a, 'r) t = { runCont : ('a -> 'r) -> 'r }

    exception MultipleRuns

    let cont c =
      let has_run = ref false in
      {
        runCont =
          (fun k ->
            if !has_run then raise MultipleRuns
            else (
              has_run := true;
              c k));
      }

    let return a = { runCont = (fun k -> k a) }

    let bind { runCont = g } ~f = { runCont = (fun k -> g (fun a -> (f a).runCont k)) }

    let map = `Define_using_bind
  end

  include T
  include Monad.Make2 (T)

  let ( let* ) x f = bind x ~f
end

let input_sketch ch =
  let inputs, output = Sexp.input_sexp ch |> [%of_sexp: string list * string] in
  let input, background = match List.rev inputs with [] -> assert false | x :: xs -> (x, List.rev xs) in
  (module struct
    let background = background

    let input = input

    let output = output
  end : Sigs.SKETCH)

let apply2 f args =
  match args with [ x; x' ] -> f x x' | _ -> raise_s [%message "Unexpected args" (List.length args : int)]

let apply6 f args =
  match args with [ x1; x2; x3; x4; x5; x6 ] -> f x1 x2 x3 x4 x5 x6 | _ -> failwith "Unexpected args"
