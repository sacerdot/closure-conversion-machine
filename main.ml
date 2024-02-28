open Lexing
open Term

let rec loop lexer =
 Printf.printf "Enter the next term to be evaluated in a single line:\n\n" ;
 flush stdout;
 match Parser.prog Lexer.read lexer with
  | Some t ->
     Printf.printf "Source term:\n%s\n\n" (Source.show_term t);
     let t = Intermediate.wrap t in
     Printf.printf "Intermediate term:\n%s\n\n" (Intermediate.show_term t);
     let t = Target.eliminate_names t in
     Printf.printf "Target term:\n%s\n\n" (Target.show_term t);
     Printf.printf "Reduction:\n";
     Machine.reduce t;
     Printf.printf "\n\n----------------------------------------\n\n";
     loop lexer
  | None -> ()

let () =
 Printf.printf
{|Enter the term to be evaluated in a single line, according to the
following syntax:

  t ::= var | λvars.t | tt | πn t | <ts>

where
 * variables var are single-letters only
 * vars is a possibly empty list of variables
 * ts is a possibly empty list of terms t
 * n is a natural number
 * the ASCII symbol "\" can be used in place of "λ", and "*" in
   place of "π"

As usual application is associative to the left and white spaces and parentheses can be used. Parentheses are mandatory around abstractions used in argument position in applications.

Remember that the arguments of abstractions must reduce to tuples that are matched against the vars list. For example "(λx.x) <y>" is a valid application that reduces to "y", whereas "(λx.x) y" is a stuck term.

The input term must be closed. Entering an open term will crash the REPL during translation to the target language.
Non clash-free terms (e.g. terms that reduce to a projection applied to an abstraction) will also crash the TTAM during reduction.

|};
 loop (from_channel stdin)
