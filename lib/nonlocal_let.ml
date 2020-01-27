open! Core

type ('a, 'b) t = { value : unit -> 'a; bind : (unit -> 'b) -> 'b }

let let_ let_ value =
  let open Delimcc in
  let p = new_prompt () in
  let value =
    let ret = lazy (shift0 p (fun k -> let_ (value ()) k)) in
    fun () -> Lazy.force ret
  in
  let bind = push_prompt p in
  { value; bind }
