{
open Lexing
open Parser
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z''A'-'Z']
let num = ['0'-'9']+

rule read =
  parse
    | white { read lexbuf }
    | "\\" { LAMBDA }
    | "λ" { LAMBDA }
    | "*" { PROJ }
    | "π" { PROJ }
    | "." { DOT }
    | ";" { SEMICOLON }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "<" { LTUPLE }
    | ">" { RTUPLE }
    | id { ID (lexeme lexbuf) }
    | num { NUM (int_of_string (lexeme lexbuf)) }
    | newline { END }
    | eof { EOF }
