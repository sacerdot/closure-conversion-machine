module Source =
struct
 type term =
   | Var of string
   | App of term * term
   | Proj of int * term
   | Abs of string array * term
   | Tuple of term array
   [@@deriving show {with_path = false}]
end

module Int =
struct
 type term =
   | Var of string
   | App of term * term
   | Proj of int * term
   | Clos of string array * string array * term * term array (* only vars and values allowed in the array *)
   | Tuple of term array
   [@@deriving show {with_path = false}]
end
