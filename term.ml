type term =
  | Var of string
  | App of term * term
  | Proj of int * term
  | Abs of string array * term
  | Tuple of term array
  [@@deriving show]
