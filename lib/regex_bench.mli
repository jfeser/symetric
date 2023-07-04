val load_examples_str : string -> (string * bool) list
val load_examples : In_channel.t -> (string * bool) list
val parse_program : string -> Regex_sketch_ast.t
val load_ground_truth : In_channel.t -> Regex_sketch_ast.t
val load_prompt : string -> string
val load_sketch_bench : string -> In_channel.t -> Regex.Value.Ctx.t * Regex.Op.t list
