type ('a, 'c) t = 'a array constraint 'c = [< `Read | `Write ] [@@deriving sexp]

type ('a, 'c) rw = ('a, 'c) t constraint 'c = [> `Read | `Write ]

external read_only : ('a, [> `Read ]) t -> ('a, [ `Read ]) t = "%identity"

external write_only : ('a, [> `Write ]) t -> ('a, [ `Write ]) t = "%identity"

external length : ('a, [> ]) t -> int = "%array_length"

external get : ('a, [> `Read ]) t -> int -> 'a = "%array_safe_get"

external set : ('a, [> `Write ]) t -> int -> 'a -> unit = "%array_safe_set"

let create = Array.create

let init = Array.init

let of_list = Array.of_list
