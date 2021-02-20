type 'a t = Fresh of 'a | Stale of 'a

let is_fresh = function Fresh _ -> true | _ -> false

let unwrap (Fresh x | Stale x) = x
