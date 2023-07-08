open Lexing
open Term

let rec loop lexer =
 flush stdout;
 match Parser.prog Lexer.read lexer with
  | Some t ->
     Printf.printf "Source term:\n%s\n" (Source.show_term t);
     let t = Intermediate.of_source_term t in
     Printf.printf "Intermediate term:\n%s\n\n" (Intermediate.show_term t);
     let t = Target.of_intermediate_term t in
     Printf.printf "Target term:\n%s\n\n" (Target.show_term t);
     loop lexer
  | None -> ()

let () =
 Printf.printf "Enter the term to be evaluated in a single line.\n\n";
 loop (from_channel stdin)
