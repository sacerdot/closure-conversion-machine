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
      Clos(ys, xs, of_source_term t, Array.map (fun v -> Var v) ys)
end

module Target =
struct
 type term =
   | L of int
   | S of int
   | App of term * term
   | Proj of int * term
   | Clos of int * int * term * term array (* only vars and values allowed in the array *)
   | Tuple of term array
   [@@deriving show {with_path = false}]
 
 let get_pos x a =
  let rec aux i = if i < 0 then None else if a.(i) = x then Some i else aux (i-1) in
  aux (Array.length a - 1)

 let rec of_intermediate_term ys xs =
  function
   | Intermediate.Var x ->
      (match get_pos x ys, get_pos x xs with
        | Some i, None -> L i
        | None, Some i -> S i
        | _, _ -> assert false)
   | Intermediate.App(t,u) -> App(of_intermediate_term ys xs t, of_intermediate_term ys xs u)
   | Intermediate.Proj(i,t) -> Proj(i, of_intermediate_term ys xs t)
   | Intermediate.Tuple(ts) -> Tuple(Array.map (of_intermediate_term ys xs) ts)
   | Intermediate.Clos(zs,ws,t,us) ->
      Clos(Array.length zs, Array.length ws, of_intermediate_term zs ws t, Array.map (of_intermediate_term ys xs) us)
 
 let of_intermediate_term = of_intermediate_term [||] [||]
end
