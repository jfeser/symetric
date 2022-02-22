type t [@@deriving yojson_of]

val create : unit -> t
val start : t -> unit
val stop : t -> unit
val time : t -> Time.Span.t
