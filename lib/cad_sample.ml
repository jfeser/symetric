(* let sample ?(state = Random.State.default) inputs =
 *   let open Grammar in
 *   let named_inputs = List.mapi inputs ~f:(fun i x -> (sprintf "i%d" i, x)) in
 *   let input_rules =
 *     List.map named_inputs ~f:(fun (n, _) -> Rule.create "p" (Term.app n []) [])
 *   in
 *   let g =
 *     input_rules
 *     @ [
 *         Rule.create "p"
 *           (Term.app "and" [ Term.nonterm "p"; Term.nonterm "p" ])
 *           [];
 *         Rule.create "p"
 *           (Term.app "or" [ Term.nonterm "p"; Term.nonterm "p" ])
 *           [];
 *         Rule.create "p"
 *           (Term.app "diff" [ Term.nonterm "p"; Term.nonterm "p" ])
 *           [];
 *       ]
 *   in
 *   let rec to_prog = function
 *     | App (func, args) ->
 *         let op =
 *           match func with
 *           | "and" -> Op.Inter
 *           | "or" -> Union
 *           | "diff" -> Sub
 *           | _ -> (
 *               match
 *                 List.Assoc.find ~equal:[%compare.equal: string] named_inputs
 *                   func
 *               with
 *               | Some i -> Input i
 *               | None -> failwith "unexpected function" )
 *         in
 *         let args = List.map args ~f:to_prog in
 *         `Apply (op, args)
 *     | _ -> failwith "unexpected term"
 *   in
 *   let rec sample_prog () =
 *     let p = to_prog (Grammar.sample ~state "p" g :> Untyped_term.t) in
 *     if Program.size p > !Global.max_cost then sample_prog () else p
 *   in
 *   sample_prog ()
 * 
 * let sample_big ?(state = Random.State.default) inputs =
 *   let open Grammar in
 *   let named_inputs = List.mapi inputs ~f:(fun i x -> (sprintf "i%d" i, x)) in
 *   let input_rules =
 *     List.map named_inputs ~f:(fun (n, _) -> Rule.create "p" (Term.app n []) [])
 *   in
 *   let g =
 *     input_rules
 *     @ [
 *         Rule.create "p"
 *           (Term.app "and" [ Term.nonterm "p"; Term.nonterm "p" ])
 *           [];
 *         Rule.create "p"
 *           (Term.app "or" [ Term.nonterm "p"; Term.nonterm "p" ])
 *           [];
 *         Rule.create "p"
 *           (Term.app "diff" [ Term.nonterm "p"; Term.nonterm "p" ])
 *           [];
 *       ]
 *   in
 *   let rec to_prog = function
 *     | App (func, args) ->
 *         let op =
 *           match func with
 *           | "and" -> Op.Inter
 *           | "or" -> Union
 *           | "diff" -> Sub
 *           | _ -> (
 *               match
 *                 List.Assoc.find ~equal:[%compare.equal: string] named_inputs
 *                   func
 *               with
 *               | Some i -> Input i
 *               | None -> failwith "unexpected function" )
 *         in
 *         let args = List.map args ~f:to_prog in
 *         `Apply (op, args)
 *     | _ -> failwith "unexpected term"
 *   in
 *   let rec sample_prog () =
 *     let p = to_prog (Grammar.sample ~state "p" g :> Untyped_term.t) in
 *     if Program.size p <> !Global.max_cost then sample_prog () else p
 *   in
 *   sample_prog ()
 * 
 * let check_search_space ?(n = 100_000) inputs graph =
 *   let rec loop i =
 *     if i > n then (
 *       Fmt.epr "Checked %d programs and found no counterexamples\n" n;
 *       Ok () )
 *     else
 *       let prog = sample inputs in
 *       let cstate = Program.ceval prog in
 *       match
 *         V.find_map graph ~f:(fun v ->
 *             match Node.to_state v with
 *             | Some v when Abs.contains (State.state v) cstate -> Some v
 *             | _ -> None)
 *       with
 *       | Some _ -> loop (i + 1)
 *       | None ->
 *           Fmt.epr "Missed program %a with size %d and state %a\n" Sexp.pp
 *             ([%sexp_of: Program.t] prog)
 *             (Program.size prog) Conc.pp cstate;
 *           Error cstate
 *   in
 *   loop 0
 * 
 * let random_likely_unsat ?(state = Random.State.default) n k =
 *   let inputs =
 *     List.init n ~f:(fun _ -> Array.init k ~f:(fun _ -> Random.State.bool state))
 *   in
 *   let output = Array.init k ~f:(fun _ -> Random.State.bool state) in
 *   (inputs, output)
 * 
 * let random_sat ?(state = Random.State.default) n k =
 *   let inputs =
 *     List.init n ~f:(fun _ -> Array.init k ~f:(fun _ -> Random.State.bool state))
 *   in
 *   let output = sample_big ~state inputs |> Program.ceval in
 *   (inputs, output)
 * 
 * let random_io ?(state = Random.State.default) ~n ~k =
 *   (\* if Random.State.bool state then random_sat ~state n k
 *    * else *\)
 *   random_sat ~state n k *)
