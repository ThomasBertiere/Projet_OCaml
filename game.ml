open Gamebase

(* These types are abstract in game.mli *)

type state = { mutable cards_P1: List int*Player ;
					mutable cards_P2: List int*Player ;
					mutable pts_P1: int*Player ;
					mutable pts_P2: int*Player ;
					mutable p : Player ;
					mutable end_of_round : int ; }

type move = int (*joue une carte, donc un int*)

type result = Win of player


(* Printers *)
(*A modifier*)
let state2s (n,p) = Printf.sprintf "Current = %d  // %s to play" n (player2s p)

let move2s n = Printf.sprintf " Joue : %d" n

let result2s (Win p) = (player2s p) ^ " wins"


(* Reader *)
let readmove s = try Some (int_of_string s) with _ -> None

(* You have to provide these. *)
(* faire un truc random*)
let initial =  ;;

(*en cours*)
let turn s = match s with 
	| { end_of_round=1} -> { s with end_of_round=0}  
	| { end_of_round=0} -> { s with p = next (p), end_of_round=1}






















(*vérifier que m est dans la liste de s et score<=13*)
let is_valid s m = 
  let (a,b) = s in 
    if 0<=m && m<=3 && a<20 then true else false  ;; 

(*supprime m de la liste de s et change le joueur *)
let play s m = if is_valid s m then 
    let (a,b) = s in (a+m, (next b)) else
    failwith "move not possible";;

(*retourne la liste de s*)
let all_moves s = [1;2;3];;


let result s = let (a,b) = s in
    if a>=20 then Some (Win b) else None ;;

(* This type was given in game.mli.
 * We have to repeat it here. *)
type comparison = Equal | Greater | Smaller;;

let compare p r1 r2 = match (r1,r2) with 
| (Win (x),Win (z)) -> if x=z then Equal else if (x=p) then Greater else if (z=p) then Smaller else failwith "pb" ;;

let worst_for p = Win ((next p));;

