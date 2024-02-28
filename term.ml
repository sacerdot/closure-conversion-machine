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

 (* computes the set of free variables of a source term *)
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
   | Clos of string array * string array * term * bag
   | Tuple of term array
 and bag = term array (* only vars and values allowed in a bag *)
   [@@deriving show {with_path = false}]

 (* wrapping translates a source term to an intermediate term *)
 let rec wrap =
  function
   | Source.Var x -> Var x
   | Source.App(t,u) -> App(wrap t, wrap u)
   | Source.Proj(i,t) -> Proj(i, wrap t)
   | Source.Tuple(ts) -> Tuple(Array.map wrap ts)
   | Source.Abs(xs,t) as i ->
      let ys = Array.of_list (Source.StringSet.elements (Source.fv i)) in
      Clos(ys, xs, wrap t, Array.map (fun v -> Var v) ys)
end

module Target =
struct
 type projected_var =
  | W of int
  | S of int
  [@@deriving show {with_path = false}]

 type term =
  | P of projected_var
  | App of term * term
  | Proj of int * term
  | Clos of int * int * term * bag
  | Tuple of term array
 and bag = term array (* only vars and values allowed in a bag *)
  [@@deriving show {with_path = false}]
 
 let get_pos x a =
  let rec aux i = if i < 0 then None else if a.(i) = x then Some i else aux (i-1) in
  aux (Array.length a - 1)

 (* translates an (open) intermediate term to a target term *)
 let rec eliminate_names ys xs =
  function
   | Intermediate.Var x ->
      (match get_pos x ys, get_pos x xs with
        | Some i, None -> P (W i)
        | None, Some i -> P (S i)
        | _, _ -> assert false)
   | Intermediate.App(t,u) -> App(eliminate_names ys xs t, eliminate_names ys xs u)
   | Intermediate.Proj(i,t) -> Proj(i, eliminate_names ys xs t)
   | Intermediate.Tuple(ts) -> Tuple(Array.map (eliminate_names ys xs) ts)
   | Intermediate.Clos(zs,ws,t,us) ->
      Clos(Array.length zs, Array.length ws, eliminate_names zs ws t, Array.map (eliminate_names ys xs) us)
 
 (* translates a closed intermediate term to a target term *)
 let eliminate_names = eliminate_names [||] [||]
end
