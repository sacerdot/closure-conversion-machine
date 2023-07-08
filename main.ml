open Lexing
open Term

let rec loop lexer =
 flush stdout;
 match Parser.prog Lexer.read lexer with
  | Some t ->
     Printf.printf "Source term: %s\n" (Source.show_term t);
     let t = Intermediate.of_source_term t in
     Printf.printf "Intermediate term: %s\n\n" (Intermediate.show_term t);
     loop lexer
  | None -> ()

let () =
 Printf.printf "Enter the term to be evaluated in a single line.\n\n";
 loop (from_channel stdin)
