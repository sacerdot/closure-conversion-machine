{
open Lexing
open Parser
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z''A'-'Z']

rule read =
  parse
    | white { read lexbuf }
    | "\\" { LAMBDA }
    | "λ" { LAMBDA }
    | "." { DOT }
    | ";" { SEMICOLON }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "<" { LTUPLE }
    | ">" { RTUPLE }
    | id { ID (lexeme lexbuf) }
    | newline { END }
    | eof { EOF }
