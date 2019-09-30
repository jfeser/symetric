open! Core

module type CACHE = sig
  open Ppx_stage

  type t

  type value

  val empty : value -> (t -> 'a code) -> 'a code

  val put : sym:string -> size:int -> sizes:int list -> t -> value -> unit code

  val iter : sym:string -> size:int -> f:(value -> unit code) -> t -> unit code

  val print_size : t -> unit code
end

module type LANG = sig
  open Core
  open Ppx_stage

  type value

  val grammar : Grammar.t

  val eval : value Map.M(String).t -> Grammar.Term.t -> value
end
