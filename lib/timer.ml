type time_span = Time.Span.t

let yojson_of_time_span t = `Float (Time.Span.to_sec t)

type time = Time.t

let yojson_of_time t = `String (Time.to_string_utc t)

type state = Not_started | Started of time | Stopped of time_span [@@deriving yojson_of]
type t = state ref [@@deriving yojson_of]

let create () = ref Not_started

let start x =
  x :=
    match !x with
    | Not_started -> Started (Time.now ())
    | Started _ as s -> s
    | Stopped _ -> failwith "timer has stopped"

let time x =
  match !x with
  | Not_started -> failwith "timer has not started"
  | Stopped t -> t
  | Started t -> Time.diff (Time.now ()) t

let stop x =
  x :=
    match !x with
    | Not_started -> failwith "timer has not started"
    | Stopped _ as s -> s
    | Started t -> Stopped (Time.diff (Time.now ()) t)
