open Term.Target

(* This module implements the Tupled Target Abstract Machine (TTAM).
   First the TTAM data structures are introduced. Then the PP module is
   used for pretty-printing. Finally the "run" function implements the
   machine main loop and transitions.
*)

(* the implementation does not distinguish values from terms *)
type value = term

type tuple = term array

type stack_item =
   Todo of term  (* white bullet t in the paper *)
 | Done of value (* black bullet v in the paper *)
 | SProj of int  (* π_i in the paper *)
 | STuple of int * tuple (* the index i is the position of the
                            downarrow ↓ in the paper; the array
                            holds in position i the unevaluated
                            term that was replaced by ↓ in the paper
                          *)

type stack = stack_item list (* constructor stack *)
type env = tuple * tuple
type ar = stack * env (* activation record *)
type ars = ar list (* activation stack *)

type status = bool * term * stack * env * ars

(* pretty-printing functions for terms, stacks, environments,
   ars and machine states *)
module PP =
struct

 (* Encodes a positive integer as a string made of UNICODE pedixes
    (e.g. "₃₁"). *)
 let rec pp_subscript = function
  | 0 -> "₀"
  | 1 -> "₁"
  | 2 -> "₂"
  | 3 -> "₃"
  | 4 -> "₄"
  | 5 -> "₅"
  | 6 -> "₆"
  | 7 -> "₇"
  | 8 -> "₈"
  | 9 -> "₉"
  | n -> pp_subscript (n / 10) ^ pp_subscript (n mod 10)

 let pp_array f a =
  let s = ref "<" in
  for i = 0 to Array.length a - 1 do
   s := !s ^ f a.(i) ^ if i < Array.length a - 1 then ";" else ""
  done ;
  !s ^ ">"
 
 let rec pp_term =
  function
    P (W i) -> "π" ^ pp_subscript i ^ "w"
  | P (S i) -> "π" ^ pp_subscript i ^ "s"
  | App(t,u) -> "(" ^ pp_term t ^ pp_term u ^ ")"
  | Proj(i,t) -> "π" ^ pp_subscript i ^ pp_term t
  | Tuple ts -> pp_array pp_term ts
  | Clos(f,n,t,us) ->
     "[◦" ^ pp_term t ^ "|" ^ pp_array pp_term us ^ "]_(" ^ string_of_int f ^ "," ^ string_of_int n ^ ")"
 
 let pp_stack_item =
  function
    Todo t -> "◦" ^ pp_term t
  | Done v -> "•" ^ pp_term v
  | SProj i -> "π" ^ pp_subscript i
  | STuple (i,ts) -> string_of_int i ^ "~" ^ pp_array pp_term ts
 
 let pp_stack k =
  let s = String.concat ":" (List.map pp_stack_item k) in
  if s = "" then "[]" else s ^ ":[]"
 let pp_env (l,s) = "(" ^ pp_array pp_term l ^ "," ^ pp_array pp_term s ^ ")"
 let pp_ar (k,e) = "(" ^ pp_stack k ^ "," ^ pp_env e ^ ")"
 let pp_ars a = String.concat ":" (List.map pp_ar a) ^ ":[]"
 let pp_status (b,t,k,e,a) =
  (if b then "•" else "◦") ^ " | " ^
  pp_term t ^ " | " ^
  pp_stack k ^ " | " ^
  pp_env e ^ " | " ^
  pp_ars a

end

(* auxiliary function to print rule names inside arrows *)
let (!!) s = Printf.printf "\n-%s->\n" s

(* TTAM main loop *)
let rec run : status -> value = function status ->
 Printf.printf "%s" (PP.pp_status status);
 match status with
   (false,App(t,u),k,e,a) -> !!"◦sea1" ; run (false,u,Todo t::k,e,a)
 | (false,Proj(n,t),k,e,a) -> !!"◦sea2" ; run (false,t,SProj n::k,e,a)
 | (false,(Tuple [||] as v),k,e,a) -> !!"◦sea4" ; run (true,v,k,e,a)
 | (false,Tuple ts,k,e,a) -> !!"◦sea3" ;
     let ts' = Array.copy ts in
     let i = Array.length ts' - 1 in
     run (false,ts'.(i),STuple(i,ts')::k,e,a)
 | (false,Clos(f,n,t,us),k,((l,s) as e),a) -> !!"◦subw" ;
    let us' =
     Array.map
      (function
          P (W n) when n < Array.length l -> l.(n)
        | P (S n) when n < Array.length s -> s.(n)
        | _ -> assert false) us in
    run (true,Clos(f,n,t,us'),k,e,a)
 | (false,P p,k,((l,s) as e),a) -> !!"◦subv" ;
     let t =
      match p with
       | W n -> assert (n < Array.length l) ; l.(n)
       | S n ->  assert (n < Array.length s) ; s.(n) in
     run (true,t,k,e,a)
 | (true,Clos(f,n,t,v),Done (Tuple v')::k,e,a)
     when f = Array.length v && n = Array.length v'-> !!"•beta_v" ;
     run (false,t,[],(v,v'),(k,e)::a)
 | (true,v,Todo t::k,e,a) -> !!"•sea1'" ; run (false,t,Done v::k,e,a)
 | (true,v,STuple(i,ts)::k,e,a) when i > 0 -> !!"•sea6'" ;
    ts.(i) <- v;
    let i' = i-1 in
    run (false,ts.(i'),STuple(i',ts)::k,e,a)
 | (true,v,STuple(0,ts)::k,e,a) -> !!"•sea3'" ;
    ts.(0) <- v;
    run (true,Tuple ts,k,e,a)
 | (true,Tuple vs,SProj i::k,e,a) when i < Array.length vs -> !!"•pi" ;
    run (true,vs.(i),k,e,a)
 | (true,v,[],_,(k,e)::a) -> !!"•sea7" ;
    run (true,v,k,e,a)
 | (true,v,[],_,[]) -> !!"extract_value_from_normal_form"; v
 | _ -> assert false

(* main function to reduce a target term to normal form *)
let reduce t = Printf.printf "%s" (PP.pp_term (run (false,t,[],([||],[||]),[])))
