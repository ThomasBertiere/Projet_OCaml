open Gamebase

(* These types are abstract in game.mli *)

type state = { cards_P1: List int*Player ;
               cards_P2: List int*Player ;
	       played_card_P1: int*Player;
	       played_card_P2: int*Player;
               pts_P1: int*Player        ;
               pts_P2: int*Player        ;
               p : Player                ;
               end_of_round : int        ; 
             };;

type move = int (*joue une carte, donc un int*) ;;

type result = Win of player | Egality ;;


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


let turn state = match state with 
  | { end_of_round=1} -> state.p 
  | { end_of_round=0} -> next (state.p)
  | _ -> failwith "Error turn"



let is_valid s m =

  let rec member x = function 
    | [] -> false 
    | [h|tl] -> if h=x then true else member x tl 
  in 
  let (liste_card_p1,p1)=s.cards_P1 in 
  let (liste_card_p2,p2)=s.cards_P2 in
  let list_pl = if p1=s.p then liste_card_p1 else liste_card_p2 in
  let in_hand=member m list_pl in 
  let sc_p1=s.score_P1 in
  let sc_p2=s.score_P2 in 
    not (liste_card_p1=[] and liste_card_p2=[]) and sc_p2<=13 and sc_p1<=13 and in_hand;;


(*supprime m de la liste de s et change le joueur *)
let play s m = 
	if is_valid s m then 
	(
		(*Remove a card from the P1 or P2 cards*)
		let rec remove x acc = function 
			| [] -> acc		
			| [h|tl] -> (if x=h then 
							remove x acc::tl tl
						else 
							remove x acc::h::tl tl
				     )
		in

		(*Get cards P1 and P2*)
		let (liste_card_p1,p1)=s.cards_P1 in 
	  	let (liste_card_p2,p2)=s.cards_P2 in
		(*Get the last card played by P1 or P2*)
		let (played_card_p1,a)=s.played_card_P1 in 
		let (played_card_p2,a)=s.played_card_P2 in 
		(*Get scores*)
		let (score_P1,p1)=s.pts_P1 in
		let (score_P2,p2)=s.pts_P2 in 
		(*Get the cards of the player P*)
		let list_pl = if p1=s.p then liste_card_p1 else liste_card_p2 in
		(*Get the score of the player P*)
		let score_pl = if p1=s.p then score_P1 else score_P2 in 



 		(*Remove the card played*)
    	let new_cards = ((remove m [] list_pl),s.p) in 
		(*Card played*)
		let played_card = (m,s.p) in

		let eor = if s.end_of_round=1 then 0 else 1 in 
		(*Get the next player*)
		let next_player = turn s in
		
		
		if p1=s.p then 
			(if s.end_of_round=1 then 
				(if played_card>played_card_p2 then 
					let new_score_P1=score_P1+1 in
					{cards_P1=new_cards;cards_P2=liste_card_p2;played_card_P1=played_card;played_card_P2=played_card_p2;
					pts_P1=new_score_P1;pts_P2=score_P2;p=next_player;end_of_round=eor}
				else
					let new_score_P2=score_P2+1 in
					{cards_P1=new_cards;cards_P2=liste_card_p2;played_card_P1=played_card;played_card_P2=played_card_p2;
					pts_P1=score_P1;pts_P2=new_score_P2;p=next_player;end_of_round=eor}
				)
			else
				{cards_P1=new_cards;cards_P2=liste_card_p2;played_card_P1=played_card;played_card_P2=played_card_p2;
				pts_P1=score_P1;pts_P2=score_P2;p=next_player;end_of_round=eor}
			)	
		else
			(if s.end_of_round=1 then 
				(if played_card>played_card_p1 then 
					let new_score_P2=score_P2+1 in 
					{cards_P1=liste_card_p1;cards_P2=new_cards;played_card_P1=played_card_p1;played_card=played_card;
					pts_P1=score_P1;pts_P2=new_score_P2;p=next_player;end_of_round=eor}
				else
					let new_score_P1=score_P1+1 in 
					{cards_P1=liste_card_p1;cards_P2=new_cards;played_card_P1=played_card_p1;played_card=played_card;
					pts_P1=new_score_P1;pts_P2=score_P2;p=next_player;end_of_round=eor}										
		
				)
			else
				{cards_P1=liste_card_p1;cards_P2=new_cards;played_card_P1=played_card_p1;played_card_P2=played_card;
				pts_P1=score_P1;pts_P2=score_P2;p=next_player;end_of_round=eor}
			)
			

	)
	else
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

