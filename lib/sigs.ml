open! Core

module type CACHE = sig
  type t

  type value

  type 'a code

  val empty : value -> (t -> 'a code) -> 'a code

  val put : sym:string -> size:int -> sizes:int list -> t -> value -> unit code

  val iter : sym:string -> size:int -> f:(value -> unit code) -> t -> unit code

  val print_size : t -> unit code
end

module type LANG = sig
  type value

  type 'a code

  val grammar : Grammar.t

  val eval : value Map.M(String).t -> Grammar.Term.t -> value
end

module type CODE = sig
  type 'a t

  type 'a set = Set

  type 'a ntype = { name : string; elem_type : 'a ctype }

  and 'a ctype =
    | Unit : unit ctype
    | Int : int ctype
    | Bool : bool ctype
    | Array : 'a ntype -> 'a array ctype
    | Set : 'a ntype -> 'a set ctype

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
    val mk_type : 'a ctype -> 'a array ctype

    module O : sig
      val ( = ) : 'a array t -> 'a array t -> bool t
    end

    val const : 'a array ctype -> 'a t array -> 'a array t

    val get : 'a array t -> int t -> 'a t

    val set : 'a array t -> int t -> 'a t -> unit t

    val length : 'a array t -> int t

    val fold : 'a array t -> init:'b t -> f:('b t -> 'a t -> 'b t) -> 'b t

    val sub : 'a array t -> int t -> int t -> 'a array t

    val init : 'a array ctype -> int t -> (int t -> 'a t) -> 'a array t
  end

  (* Set operations *)
  module Set : sig
    val mk_type : 'a ctype -> 'a set ctype

    val empty : 'a set ctype -> 'a set t

    val add : 'a set t -> 'a t -> unit t

    val iter : 'a set t -> ('a t -> unit t) -> unit t
  end

  (* Control flow *)
  val ite : bool t -> 'a t -> 'a t -> 'a t

  val let_ : 'a t -> ('a t -> 'b t) -> 'b t

  val seq : unit t -> unit t -> unit t
end
