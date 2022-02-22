type 'a t = unit -> 'a

let yojson_of_t yojson_of_a f = [%yojson_of: a] (f ())
