module Dim : sig
  type t = { xres : int; yres : int; scaling : int }
  [@@deriving compare, hash, sexp, yojson]

  val xres : t -> int
  val yres : t -> int
  val scaled_xres : t -> int
  val scaled_yres : t -> int
  val create : ?scaling:int -> xres:int -> yres:int -> unit -> t
  val npixels : t -> int
  val offset : t -> int -> int -> int
  val pixels : t -> (int * int) Gen.t
  val ( = ) : t -> t -> bool
  val param : t Command.Param.t
end

type t [@@deriving compare, hash, sexp]

include Comparator.S with type t := t

val of_bitarray : Dim.t -> Bitarray.t -> t
val pixels : t -> Bitarray.t
val dim : t -> Dim.t
val init : Dim.t -> f:(int -> int -> int -> bool) -> t
val npixels : t -> int
val to_iter : Dim.t -> t -> ((int * int) * bool -> unit) -> unit
val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
val get : t -> int -> bool
val hamming : t -> t -> int
val hamming_weight : t -> int
val is_empty : t -> bool
val jaccard_canvas : t -> t -> float
val jaccard_pixels : t -> t -> float
val cosine : t -> t -> float
val circle : Dim.t -> int -> int -> int -> t
val rect : Dim.t -> int -> int -> int -> int -> t
val inter : t -> t -> t
val union : t -> t -> t
val sub : t -> t -> t
val repeat : t -> int -> int -> int -> t
val translate : t -> int -> int -> t
val ( = ) : t -> t -> bool
val corners : t -> t
val corner_overlap : t -> t -> bool
