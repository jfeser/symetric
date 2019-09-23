open! Core

module type CACHE = sig
  open Ppx_stage

  type t

  type value

  val empty : (t -> 'a code) -> 'a code

  val put : sym:string -> size:int -> t -> value code -> unit code

  val iter :
    sym:string -> size:int -> f:(value code -> unit code) -> t -> unit code

  val print_size : t -> unit code
end

module type LANG = sig
  open Core
  open Ppx_stage

  type value

  val grammar : Grammar.t

  val eval : value code Map.M(String).t -> Grammar.Term.t -> value code
end
