module Source =
struct
 type term =
   | Var of string
   | App of term * term
   | Proj of int * term
   | Abs of string array * term
   | Tuple of term array
   [@@deriving show {with_path = false}]
 
 module StringSet = Set.Make(String)

 let rec fv = StringSet.(
  function
    | Var x -> singleton x
    | App(t,u) -> union (fv t) (fv u)
    | Proj(_,t) -> fv t
    | Abs(vs,t) -> diff (fv t) (of_list (Array.to_list vs))
    | Tuple(ts) -> Array.fold_left (fun s t -> union s (fv t)) empty ts)
end

module Intermediate =
struct
 type term =
   | Var of string
   | App of term * term
   | Proj of int * term
   | Clos of string array * string array * term * term array (* only vars and values allowed in the array *)
   | Tuple of term array
   [@@deriving show {with_path = false}]

 let rec of_source_term =
  function
   | Source.Var x -> Var x
   | Source.App(t,u) -> App(of_source_term t, of_source_term u)
   | Source.Proj(i,t) -> Proj(i, of_source_term t)
   | Source.Tuple(ts) -> Tuple(Array.map of_source_term ts)
   | Source.Abs(xs,t) as i ->
      let ys = Array.of_list (Source.StringSet.elements (Source.fv i)) in
      Clos(ys,xs,of_source_term t,Array.map (fun v -> Var v) ys)
end
