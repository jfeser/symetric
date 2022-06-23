{
  open Regex_parser
  open Regex_sketch_ast

  let keyword_tbl =
    let open Core in
    Hashtbl.of_alist_exn (module String) [
        "or", BINOP Or;
        "and", BINOP And;
        "not", UNOP Not;
        "repeat", REPEAT;
        "repeatrange", REPEATRANGE;
        "repeatatleast", REPEATATLEAST;
        "concat", BINOP Concat;
        "startwith", UNOP Startwith;
        "endwith", UNOP Endwith;
        "star", UNOP Star;
        "contain", UNOP Contain;
      ]
}

let num = ['0'-'9']+
let id = ['a'-'z' 'A'-'Z']+
let chars = '<' [' '-';' '=' '?'-'~']+ '>'

rule token = parse
  | '\n'       { Lexing.new_line lexbuf; token lexbuf }
  | '\t' '\r'  { token lexbuf }
  | chars as x { CLASS x }
  | ' '        { token lexbuf }
  | "("        { LPAREN }
  | ")"        { RPAREN }
  | ","        { COMMA }
  | "?{"       { LBRACK }
  | "?"        { QMARK }
  | "}"        { RBRACK }
  | num as x   { NUM (Core.Int.of_string x) }
  | id as x    {
      let open Core in
      Hashtbl.find_exn keyword_tbl (String.lowercase x)
    }
  | eof        { EOF }
  | _          {
      let open Core in
      failwith (sprintf "unexpected character '%c'"
                 (Lexing.lexeme_char lexbuf (Lexing.lexeme_start lexbuf)) )
    }
