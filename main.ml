open Lexing
open Term

let rec loop lexer =
 flush stdout;
 match Parser.prog Lexer.read lexer with
  | Some t ->
     Printf.printf "%s\n\n" (Source.show_term t);
     loop lexer
  | None -> ()

let () =
 Printf.printf "Enter the term to be evaluated in a single line.\n\n";
 loop (from_channel stdin)
