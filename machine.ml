open Term.Target

type tuple = term array
type value = term

type stack_item =
   Todo of term
 | Done of value
 | SProj of int
 | STuple of int * tuple

type stack = stack_item list
type env = tuple * tuple
type ar = stack * env
type ars = ar list

type status = bool * term * stack * env * ars

let pp_array f a =
 let s = ref "<" in
 for i = 0 to Array.length a - 1 do
  s := !s ^ f a.(i) ^ if i < Array.length a - 1 then ";" else ""
 done ;
 !s ^ ">"

let rec pp_term =
 function
   L i -> "L" ^ string_of_int i
 | S i -> "S" ^ string_of_int i
 | App(t,u) -> "(" ^ pp_term t ^ " " ^ pp_term u ^ ")"
 | Proj(i,t) -> "π" ^ string_of_int i ^ pp_term t
 | Tuple ts -> pp_array pp_term ts
 | Clos(f,n,t,us) ->
    "[" ^ string_of_int f ^ "," ^ string_of_int n ^ "," ^
     pp_term t ^ "|" ^ pp_array pp_term us ^ "]"

let pp_stack_item =
 function
   Todo t -> "?" ^ pp_term t
 | Done v -> "!" ^ pp_term v
 | SProj i -> "P" ^ string_of_int i
 | STuple (i,ts) -> string_of_int i ^ "~" ^ pp_array pp_term ts

let pp_stack k =
 let s = String.concat ":" (List.map pp_stack_item k) in
 if s = "" then "[]" else s ^ ":[]"
let pp_env (fs,ns) = "(" ^ pp_array pp_term fs ^ "," ^ pp_array pp_term ns ^ ")"
let pp_ar (k,e) = pp_stack k ^ "," ^ pp_env e
let pp_ars a = String.concat ":" (List.map pp_ar a)
let pp_status (b,t,k,e,a) =
 (if b then "!" else "?") ^ "|" ^
 pp_term t ^ "|" ^
 pp_stack k ^ "|" ^
 pp_env e ^ "|" ^
 pp_ars a

let (!!) s = Printf.printf "\n-%s->\n" s

let rec run : status -> value = function status ->
 Printf.printf "%s" (pp_status status);
 match status with
   (false,App(t,u),k,e,a) -> !!"sea1" ; run (false,u,Todo t::k,e,a)
 | (false,Proj(n,t),k,e,a) -> !!"sea2" ; run (false,t,SProj n::k,e,a)
 | (false,(Tuple [||] as v),k,e,a) -> !!"sea4" ; run (true,v,k,e,a)
 | (false,Tuple ts,k,e,a) -> !!"sea3" ;
     let ts' = Array.copy ts in
     let i = Array.length ts' - 1 in
     run (false,ts'.(i),STuple(i,ts')::k,e,a)
 | (false,Clos(f,n,t,us),k,((fs,ns) as e),a) -> !!"subw" ;
    let us' =
     Array.map
      (function
          L n when n < Array.length fs -> fs.(n)
        | S n when n < Array.length ns -> ns.(n)
        | _ -> assert false) us in
    run (true,Clos(f,n,t,us'),k,e,a)
 | (false,L n,k,((fs,_) as e),a) when n < Array.length fs -> !!"subl" ;
     run (true,fs.(n),k,e,a)
 | (false,S n,k,((_,ns) as e),a) when n < Array.length ns -> !!"subs" ;
     run (true,ns.(n),k,e,a)
 | (true,Clos(f,n,t,v),Done (Tuple v')::k,e,a)
     when f = Array.length v && n = Array.length v'-> !!"beta" ;
     run (false,t,[],(v,v'),(k,e)::a)
 | (true,v,Todo t::k,e,a) -> !!"sea1'" ; run (false,t,Done v::k,e,a)
 | (true,v,STuple(i,ts)::k,e,a) when i > 0 -> !!"sea2'" ;
    ts.(i) <- v;
    let i' = i-1 in
    run (false,ts.(i'),STuple(i',ts)::k,e,a)
 | (true,v,STuple(0,ts)::k,e,a) -> !!"sea3'" ;
    ts.(0) <- v;
    run (true,Tuple ts,k,e,a)
 | (true,Tuple vs,SProj i::k,e,a) when i < Array.length vs -> !!"pi" ;
    run (true,vs.(i),k,e,a)
 | (true,v,[],_,(k,e)::a) -> !!"seaa" ;
    run (true,v,k,e,a)
 | (true,v,[],_,[]) -> !!"term_from_normal_form"; v
 | _ -> assert false

let test t = Printf.printf "%s" (pp_term (run (false,t,[],([||],[||]),[])))

(*
let ex1 =
 App
  (Clos(0,2,Proj(0,S 0),[||]),
   Tuple [|Tuple [|Tuple [||]; Tuple[||]|]; Tuple [||]|])

let _ = test ex1
*)
