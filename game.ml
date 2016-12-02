open Gamebase

(* These types are abstract in game.mli *)

type state = int * player

type move = int

type result = Win of player

(* Printers *)
let state2s (n,p) = Printf.sprintf "Current = %d  // %s to play" n (player2s p)

let move2s n = Printf.sprintf " +%d" n

let result2s (Win p) = (player2s p) ^ " wins"

(* Reader *)
let readmove s = try Some (int_of_string s) with _ -> None

(* You have to provide these. *)
let initial = (0,Comput) ;;

let turn s = let (a,b) = s in b ;;  

let is_valid s m = 
  let (a,b) = s in 
    if 0<=m && m<=3 && a<20 then true else false  ;; 

let play s m = if is_valid s m then 
    let (a,b) = s in (a+m, (next b)) else
    failwith "move not possible";;

let all_moves s = [1;2;3];;

let result s = let (a,b) = s in
    if a>=20 then Some (Win b) else None ;;

(* This type was given in game.mli.
 * We have to repeat it here. *)
type comparison = Equal | Greater | Smaller;;

let compare p r1 r2 = match (r1,r2) with 
| (Win (x),Win (z)) -> if x=z then Equal else if (x=p) then Greater else if (z=p) then Smaller else failwith "pb" ;;

let worst_for p = Win ((next p));;

