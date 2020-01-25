open! Core

module Log = Utils.Make_log (struct
  let src = Logs.Src.create "staged-synth.genlet"
end)

let () = Log.set_level None

(* Generic let-insertion: genlet code inserts a let expression to bind 'code' as
   high as possible -- as high in the scope as still safe (creating no scope
   extrusion).
*)
module Make (C : sig
  type 'a t

  val to_string : 'a t -> string

  (* Check if a piece of code contains free variables that cannot
     be bound. We try to incorporate the received piece of code
     into a larger piece of code. At this point, MetaOCaml does
     the scope extrusion check, throwing an exception if the check fails.
  *)
  val is_well_scoped : 'a t -> bool

  val let_ : 'a t -> ('a t -> 'b t) -> 'b t
end) =
struct
  open Delimcc

  (* Let-insertion request (with an existential) We ask to let-bind the code value
     and return the corresponding let-bound identifier. If the response is
     (c,true), the request was satisfied and c is the let-bound identifier. If the
     response is (c,false), the let-binding was unsuccessful and c is the original
     code from the request. *)
  type genlet_req =
    | Done : genlet_req
    | Req : 'a C.t * ('a C.t * bool -> genlet_req) -> genlet_req

  (* The single prompt for let-insertion *)
  let p = new_prompt ()

  (* Send the let-insertion request for a given code.
     If the prompt is not set, just return (c,false)
  *)

  let send_req c =
    if is_prompt_set p then shift0 p (fun k -> Req (c, k)) else (c, false)

  let genlet c =
    let orig_c = c in
    let code, success = send_req c in
    Log.debug (fun m ->
        m "Generating let %s for: %s"
          (if success then "succeeded" else "failed")
          (C.to_string orig_c));
    code

  (* We often use mutable variables as `communication channel', to appease
     the type-checker. The variable stores the `option' value --
     most of the time, None. One function writes a Some x value,
     and another function almost immediately reads the value -- exactly
     once. The protocol of using such a variable is a sequence of
     one write almost immediately followed by one read.
     We use the following helpers to access our `communication channel'.
  *)
  let read_answer r =
    let v = Option.value_exn !r in
    (* for safety *)
    r := None;
    v

  (* The point of inserting let: the handler for genlet_req
     Upon receiving the let-binding request and before satsifying
     it, we check if the request can be fulfilled by a handler
     upstream.
     That is, we try to insert let `as high as possible'.
   *)
  let let_locus body =
    let r = ref None in
    let rec loop = function
      | Done -> read_answer r
      | Req (c, k) -> (
          if not (C.is_well_scoped c) then loop (k (c, false))
          else
            (* try higher *)
            match send_req c with
            | c, false ->
                let ret = C.let_ c (fun t -> loop (k (t, true))) in
                Log.debug (fun m ->
                    m "Let insertion generated %s" (C.to_string ret));
                ret
            (* .<let t = .~c in .~(loop (k (.<t>.,true)))>. *)
            | x -> loop (k x) (* c is a variable, inserted higher *) )
    in
    loop @@ push_prompt p
    @@ fun () ->
    r := Some (body ());
    Done

  let with_stackmark body =
    let p = new_prompt () in
    push_prompt p (fun () -> body (fun () -> is_prompt_set p))
end
