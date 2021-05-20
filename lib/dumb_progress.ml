module Bar = struct
  type 'a bar = {
    id : int;
    update : 'a -> unit; [@ignore]
    render : Bigstring.t -> unit; [@ignore]
  }
  [@@deriving compare, hash, sexp]

  type t = Bar : 'a bar -> t

  let compare (Bar b) (Bar b') = compare b.id b'.id
end

module Display = struct
  type t = {
    bars : Bar.t Queue.t;
    mutable ch : Out_channel.t;
    mutable width : int;
    fresh : Fresh.t;
    buf : Bigstring.t;
    mutable render_interval : Time.Span.t;
    mutable last_render : Time.t;
  }

  let create ?(width = 80) ?(render_interval = Time.Span.of_ms 100.0)
      ?(ch = Out_channel.stderr) () =
    {
      ch;
      width;
      bars = Queue.create ();
      fresh = Fresh.create ();
      buf = Bigstring.create width;
      render_interval;
      last_render = Time.epoch;
    }

  let default = create ()
end

let add ?(display = Display.default) bar = Queue.enqueue display.bars (Bar bar)

let remove ?(display = Display.default) bar =
  Queue.filter_inplace display.bars ~f:(fun bar' ->
      not ([%compare.equal: Bar.t] (Bar bar) bar'))

module Ansi = struct
  let show_cursor = "\x1b[?25h"

  let hide_cursor = "\x1b[?25l"

  let erase_display_suffix = "\x1b[J"

  let erase_line = "\x1b[K"

  let move_up ch = function 0 -> () | n -> Out_channel.fprintf ch "\x1b[%dA" n

  let move_down ch = function
    | 0 -> ()
    | n -> Out_channel.fprintf ch "\x1b[%dB" n
end

let render_all (display : Display.t) =
  let now = Time.now () in
  if Time.Span.(Time.diff now display.last_render > display.render_interval)
  then (
    if Time.(display.last_render > Time.epoch) then
      Ansi.move_up display.ch (Queue.length display.bars)
    else Out_channel.print_string "\n";
    display.last_render <- now;
    Queue.iter display.Display.bars ~f:(fun (Bar b) ->
        b.render display.buf;
        Bigstring_unix.really_output display.ch display.buf;
        Out_channel.output_string display.ch "\n");
    Out_channel.flush display.ch)

let update ?(display = Display.default) bar state =
  bar.Bar.update state;
  render_all display

let format_counter ?tot ~n () =
  match tot with
  | Some tot -> sprintf "%d/%dit" n tot
  | None -> sprintf "%dit" n

let format_iters = sprintf "%0.2fit/s"

let draw_progress buf p =
  let width = Bigstring.length buf in
  [%test_pred: float] (fun p -> Float.O.(0.0 <= p && p <= 1.0)) p;
  let n_filled = Float.(to_int @@ round_up ((of_int width - 2.0) * p)) in
  let n_empty = width - 2 - n_filled in
  buf.{0} <- '[';
  Bigstring.memset buf ~pos:1 ~len:n_filled '#';
  Bigstring.memset buf ~pos:(1 + n_filled) ~len:n_empty ' ';
  buf.{width - 1} <- ']'

let draw_basic ?per_sec ?name ~tot ~n buf =
  let ips =
    Option.map per_sec ~f:format_iters
    |> Option.map ~f:(fun s -> " " ^ s)
    |> Option.value ~default:""
  in
  let counter = format_counter ~tot ~n () in
  let tail = " " ^ counter ^ ips in
  let head =
    Option.map name ~f:(fun n -> n ^ " ") |> Option.value ~default:""
  in
  let head_width = String.length head in
  let body_width = Bigstring.length buf - String.length tail - head_width in
  Bigstring.From_string.blito ~src:head ~dst:buf ();
  Bigstring.From_string.blito ~src:tail ~dst:buf
    ~dst_pos:(head_width + body_width) ();
  let head_buf = Bigstring.sub_shared ~pos:head_width ~len:body_width buf in
  let prog = Float.(of_int n / of_int tot) in
  draw_progress head_buf prog

let%expect_test "" =
  let buf = Bigstring.create 80 in
  draw_basic ~per_sec:3320.60 ~name:"sampling" ~tot:100 ~n:45 buf;
  printf "%S\n" @@ Bigstring.to_string buf;
  [%expect
    {| "sampling [######################                          ] 45/100it 3320.60it/s" |}]

let basic_bar ?(display = Display.default) ?name tot =
  let id = Fresh.int display.fresh in
  let state = ref 0 in
  let start_time = ref None in
  let update x =
    if Option.is_none !start_time then start_time := Some (Time.now ());
    state := x
  in
  let render buf =
    let per_sec =
      Option.map !start_time ~f:(fun start_time ->
          Float.of_int !state
          /. (Time.diff (Time.now ()) start_time |> Time.Span.to_sec))
    in
    draw_basic ?per_sec ?name ~tot ~n:!state buf
  in
  Bar.{ id; update; render }

let with_bar ?(display = Display.default) bar f =
  add ~display bar;
  Exn.protectx ~f bar ~finally:(fun bar -> remove ~display bar)

module List = struct
  let iter ?name l ~f =
    let bar = basic_bar ?name @@ List.length l in
    add bar;
    let f i x =
      update bar i;
      f x
    in
    Exn.protectx ~f:(List.iteri ~f) ~finally:(fun _ -> remove bar) l

  let map ?name l ~f =
    let bar = basic_bar ?name @@ List.length l in
    add bar;
    let f i x =
      update bar i;
      f x
    in
    Exn.protectx ~f:(List.mapi ~f) ~finally:(fun _ -> remove bar) l

  let concat_map ?name l ~f =
    let bar = basic_bar ?name @@ List.length l in
    add bar;
    let f i x =
      update bar i;
      f x
    in
    Exn.protectx ~f:(List.concat_mapi ~f) ~finally:(fun _ -> remove bar) l
end
