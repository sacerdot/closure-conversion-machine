# closure-conversion-machine
Artifact for the Closure Conversion and Abstract Machines paper

# description of the artifact
The artifact implements a Read-Eval-Print-Loop (REPL) to evaluate terms of the Source Calculus by means of a preliminary translation to the Target Calculus via an Intermediate Calculus, followed by reduction performed by the Target Tupled Abstract Machine (TTAM). The internal representation of the term after each translation is printed, followed by every single step of the TTAM.

# compilation
Steps to compile the artifact on a Linux machine:
1) if you don't have already a working installation of ocaml+opam, follow the instructions at https://opam.ocaml.org/doc/Install.html. The code is compatible with any modern version of OCaml
2) run "opam install menhir ppx_show" to install the project dependencies
3) run "dune build" to compile the code

The code should run on other operating systems as well, but is has been tested only in Linux.

# running the REPL
To start the REPL type "dune exec main". It is also possible to feed multiple terms to reduce using stdin-redirection, e.g. "dune exec main < TESTS"

# input syntax
Terms to be reduced must fit a single line. The syntax for terms is:

t ::= var | λvars.t | tt | πn t | <ts>

where
* variables var are single-letters only
* vars is a possibly empty list of variables
* ts is a possibly empty list of terms t
* n is a natural number
* the ASCII symbol "\" can be used in place of "λ", and "*" in place of "π"

As usual application is associative to the left and white spaces and parentheses can be used. Parentheses are mandatory around abstractions used in argument position in applications.<

Remember that the arguments of abstractions must reduce to tuples that are matched against the vars list. For example "(λx.x) <y>" is a valid application that reduces to "y", whereas "(λx.x) y" is a stuck term.

The input term must be closed. Entering an open term will crash the REPL during translation to the target language.
Stuck terms (e.g. a projection applied to an abstraction) will also crash the TTAM during reduction.

# output syntax
The source, intermediate and target terms corresponding to the user input are first displayed according to the internal representation, without any pretty-printing or sugaring. The terms, stacks, environments and machine states of the TTAM are printed in compact syntax according to the following grammar that does not distinguish between terms and values:

p ::= πnl | πnr  
t ::= p | πn t | <ts> | [?t|b]_(n,m)  
b ::= ps | ts  
stack_item ::= ?t | !t | πn | i~<ts>  
env ::= ( ts, ts )  
ar ::= ( stack, env )  
flag ::= ? | !  
status ::= flag | t | stack | env | ars  

where
* a stack is a list of stack_items
* an ars is a list of ar (activation record)
* elements of lists are separated by ":" and terminated by "[]"
* "?" means "non-evaluated" and "!" evaluated; in the paper they correspond to the red and blue dots respectively

When printing TTAM transitions, the names of the transitions are printed inside an arrow (es. "-?sea1->"). Rule names are prefixed by "?" or "!" whereas in the paper the rule names where respectively red and blue.

# code organization
The code is organized in four files:
* lexer.mll: the lexer for the input language (in ocamllex syntax)
* parser.mly: the parser for the input language (in menhir syntax)
* term.ml: it contains three submodules Source/Intermediate/Target, one for each languge. Each module defines an algebraic data type "term" to capture the abstract syntax tree of terms of that language. Moreover the Intermeidate and Target modules export a function to build terms from the previous representation
* machine.ml: it implements the TTAM. Algebraic data types are first used to capture stacks, environments, ars and states of a TTAM. A pretty-printing module follows to pretty-print the above data structures. Finally the TTAM implementation follows: first the main loop and transitions (the "run") function, then the "reduce" function that takes a target term, builds the initial machine state and runs it to normal form.

A TEST file contains three small terms to show the input syntax.
