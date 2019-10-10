open! Core

module type CACHE = sig
  type t

  type value

  type 'a code

  val empty : (t -> 'a code) -> 'a code

  val put :
    sym:string -> size:int -> sizes:int array code -> t -> value -> unit code

  val iter :
    sym:string ->
    size:int code ->
    f:(value * int array code -> unit code) ->
    t ->
    unit code

  val print_size : t -> unit code
end

module type LANG = sig
  type value

  type 'a code

  val ( = ) : value -> value -> bool code

  val grammar : Grammar.t

  val eval : value Map.M(String).t -> Grammar.Term.t -> value
end

module type CODE = sig
  type 'a t

  type 'a set = Set

  type ntype = { name : string; elem_type : ctype }

  and ctype =
    | Unit
    | Int
    | Bool
    | Array of ntype
    | Set of ntype
    | Tuple of string * ctype * ctype
    | Func of ctype * ctype

  val to_string : 'a t -> string

  (* type 'a set *)

  (* Values *)
  val unit : unit t

  val int : int -> int t

  val bool : bool -> bool t

  (* Integer operations *)
  val ( ~- ) : int t -> int t

  val ( + ) : int t -> int t -> int t

  val ( - ) : int t -> int t -> int t

  val ( * ) : int t -> int t -> int t

  val ( / ) : int t -> int t -> int t

  val ( mod ) : int t -> int t -> int t

  val ( > ) : int t -> int t -> bool t

  val ( < ) : int t -> int t -> bool t

  val ( >= ) : int t -> int t -> bool t

  val ( <= ) : int t -> int t -> bool t

  val ( = ) : int t -> int t -> bool t

  (* Boolean operations *)
  val ( && ) : bool t -> bool t -> bool t

  val ( || ) : bool t -> bool t -> bool t

  val not : bool t -> bool t

  (* Array operations *)
  module Array : sig
    val mk_type : ctype -> ctype

    module O : sig
      val ( = ) : 'a array t -> 'a array t -> bool t
    end

    val const : ctype -> 'a t array -> 'a array t

    val get : 'a array t -> int t -> 'a t

    val set : 'a array t -> int t -> 'a t -> unit t

    val length : 'a array t -> int t

    val fold : 'a array t -> init:'b t -> f:('b t -> 'a t -> 'b t) -> 'b t

    val sub : 'a array t -> int t -> int t -> 'a array t

    val init : ctype -> int t -> (int t -> 'a t) -> 'a array t
  end

  (* Set operations *)
  module Set : sig
    val mk_type : ctype -> ctype

    val empty : ctype -> 'a set t

    val add : 'a set t -> 'a t -> unit t

    val iter : 'a set t -> ('a t -> unit t) -> unit t
  end

  (* Tuples *)
  module Tuple : sig
    val mk_type : ctype -> ctype -> ctype

    val create : 'a t -> 'b t -> ('a * 'b) t

    val fst : ('a * 'b) t -> 'a t

    val snd : ('a * 'b) t -> 'b t
  end

  (* Control flow *)
  val ite : bool t -> 'a t -> 'a t -> 'a t

  val let_ : 'a t -> ('a t -> 'b t) -> 'b t

  val seq : unit t -> unit t -> unit t

  (* Functions *)
  val func : string -> ctype -> ('a t -> 'b t) -> ('a -> 'b) t

  val apply : ('a -> 'b) t -> 'a t -> 'b t

  (* Utility *)
  val print : string -> unit t
end
