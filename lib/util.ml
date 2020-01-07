open! Core

let clang_format src =
  let read, write = Unix.open_process "clang-format" in
  Out_channel.output_string write src;
  Out_channel.close write;
  let ret = In_channel.input_all read in
  ignore (Unix.close_process (read, write));
  ret

let clang_build ?(args = "-std=c++17 -c") src =
  let read, write = Unix.open_process (sprintf "clang %s -x c++ -" args) in
  Out_channel.output_string write src;
  Out_channel.close write;
  let ret = In_channel.input_all read in
  ignore (Unix.close_process (read, write));
  ret

module Cont = struct
  module T = struct
    type ('a, 'r) t = { runCont : ('a -> 'r) -> 'r }

    let return a = { runCont = (fun k -> k a) }

    let bind { runCont = g } ~f =
      { runCont = (fun k -> g (fun a -> (f a).runCont k)) }

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
              c k ));
      }

    let return a = { runCont = (fun k -> k a) }

    let bind { runCont = g } ~f =
      { runCont = (fun k -> g (fun a -> (f a).runCont k)) }

    let map = `Define_using_bind
  end

  include T
  include Monad.Make2 (T)

  let ( let* ) x f = bind x ~f
end
