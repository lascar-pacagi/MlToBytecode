{
  open Lexing
  open Parser

  exception Error of string

  let error lexbuf msg = 
    let pos = lexbuf.lex_curr_p in
    raise @@ Error (msg ^ Format.sprintf " at line %d@." pos.pos_lnum)

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

let digit = ['0'-'9']
let integer = digit+
let space = [' ' '\t' '\r']
let letter = ['a'-'z''A'-'Z''_']
let ident = letter (digit | letter)*

rule token = parse
  | "//" [^ '\n']* '\n'
  | '\n'      { newline lexbuf; token lexbuf }
  | space+    { token lexbuf }
  | "(*"      { comment lexbuf }
  | '+'       { PLUS }
  | '-'       { MINUS }
  | '*'       { TIMES }
  | '('       { LPAR }
  | ')'       { RPAR }
  | ','       { COMMA }
  | '='       { EQ }
  | '<'       { LT }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "fun"     { FUN }
  | "->"      { ARROW }
  | "let"     { LET }
  | "let"space+"rec" { LETREC }
  | "in"      { IN }
  | "true"    { TRUE }
  | "false"   { FALSE }
  | "fst"     { FST }
  | "snd"     { SND }
  | integer as i {
    try
      INT (int_of_string i)
    with Failure _ ->
      error lexbuf "Invalid integer constant"
  }
  | ident as id { IDENT id }
  | "//" [^ '\n']* eof
  | eof     { EOF }
  | _ as c  { error lexbuf @@ "Illegal character: " ^ String.make 1 c }

and comment = parse
  | "*)"    { token lexbuf }
  | '\n'    { newline lexbuf; comment lexbuf }
  | _       { comment lexbuf }
  | eof     { error lexbuf "Unterminated comment" }
