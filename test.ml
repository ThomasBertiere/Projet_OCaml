type player = Human | Comput

(* Next turn *)
let next = function
  | Human -> Comput
  | Comput -> Human

let player2s = function
  | Human -> "Human"
  | Comput -> "Comput"



(* Helper functions on matrices *)

type 'a matrix = 'a array array

let clone_matrix m =
  let cloned = Array.copy m in
    Array.iteri (fun i line -> cloned.(i) <- Array.copy line) cloned ;
    cloned

exception Found of int * int

let find_cell m p =
  try
    for row = 0 to Array.length m - 1 do
      let line = m.(row) in
        for col = 0 to Array.length line - 1 do
          if p line.(col) then raise (Found (row, col))
        done ;
    done;
    None

  with Found (r,c) -> Some (r,c)

let line2s v2s line = Array.fold_left (fun s v -> s ^ v2s v ^ "|") "|" line

let linesep = "-------\n"

(* Transforms a grid to a string. *)
let matrix2s m v2s =
  if Array.length m = 0 then "(empty matrix)"
  else
    let firstline = line2s v2s m.(0) in
    let linesep = (String.make (String.length firstline) '-') ^ "\n" in
      Array.fold_left (fun s line -> s ^ (line2s v2s line) ^ "\n" ^ linesep) linesep m




















(*####################### OUR FUNCTIONS ###############################*)
(* these types are abstract in game.mli *)

type state = { cards_P1: int list*player  ;
               cards_P2: int list*player  ;
               played_card_P1: int*player ;
               played_card_P2: int*player ;
               pts_P1: int*player         ;
               pts_P2: int*player         ;
               p : player                 ; 
               end_of_round : int         ; 
             };;

type move = int (*joue une carte, donc un int*) ;;

type result = Win of player*int | Egality ;;


(* Printers *)

let listcards2s l =
  let rec aux = function 
    | [] -> " ]"
    | [a] -> " "^string_of_int a^aux [] 
    | hd::tl ->" "^string_of_int hd^" , "^aux tl 
  in
    "["^aux l;;

let state2s s = 
  let (card_p1,p1)=s.cards_P1 in
  let (card_p2,p2)=s.cards_P2 in
  let (played_card_p1,p1)=s.played_card_P1 in
  let (played_card_p2,p2)=s.played_card_P2 in
  let (pts_p1,p1)=s.pts_P1 in
  let (pts_p2,p2)=s.pts_P2 in
    Printf.sprintf "Current state : \n   Player to play : %s\n   Player 1 : %s\n      Cards : %s\n      Played card : %d\n      Pts : %d\n   Player 2 : %s\n      Cards : %s\n      Played card : %d\n      Pts : %d\n   End of round : %d%!\n\n" (player2s s.p) (player2s p1) (listcards2s card_p1) played_card_p1 pts_p1 (player2s p2) (listcards2s card_p2) played_card_p2 pts_p2 s.end_of_round;;


let move2s n = Printf.sprintf " Joue : %d" n 

let result2s = function 
  | Win (pl,pts) -> (player2s pl) ^ " wins with "^string_of_int(pts)^" points" 
  | Egality -> "Egality"


let rec random_card = function 
  | [] -> [],[]
  | hd::tl -> 
      let (p1,p2)=random_card tl in
        if (Random.int 2)= 0 then 
          if (List.length p1)<27 then 
            (hd::p1),p2 
          else 
            p1,(hd::p2) 
        else
        if (List.length p2)<27 then 
          p1,(hd::p2)
        else 
          (hd::p1),p2 
;;

let initial =  
  Random.init (567727889) ;
  let card_p1,card_p2=random_card [1;1;1;1;2;2;2;2;3;3;3;3;4;4;4;4;5;5;5;5;6;6;6;6;7;7;7;7;8;8;8;8;9;9;9;9;10;10;10;10;11;11;11;11;12;12;12;12;13;13;13;13] in
  let p1= if (Random.int 2)= 0 then Human else Comput in 
  let p2= next p1 in 
    { cards_P1=card_p1,p1  ;
      cards_P2=card_p2,p2  ;
      played_card_P1=0,p1  ;
      played_card_P2=0,p2  ;
      pts_P1= 0,p1         ;
      pts_P2= 0,p2         ;
      p= p1                ; 
      end_of_round=0       ; 
    };;



let turn state =s.p(* match state with 
                      | { end_of_round=1} -> state.p 
                      | { end_of_round=0} -> next (state.p)
                      | _ -> failwith "Error turn"
                   *)


let is_valid s m =
  let rec member x = function 
    | [] -> false 
    | h::tl -> if h=x then true else member x tl 
  in 
  let (liste_card_p1,p1)=s.cards_P1 in 
  let (liste_card_p2,p2)=s.cards_P2 in
  let list_pl = if p1=s.p then liste_card_p1 else liste_card_p2 in
  let in_hand=member m list_pl in 
  let sc_p1,a=s.pts_P1 in
  let sc_p2,a=s.pts_P2 in 
    (not(liste_card_p1=[] && liste_card_p2=[])) && sc_p2<=13 && sc_p1<=13 && in_hand;;


let all_moves s =
  let (liste_card_p1,p1)=s.cards_P1 in 
  let (liste_card_p2,p2)=s.cards_P2 in
    if p1=s.p then liste_card_p1 else liste_card_p2;;

let result s = 
  let (pts_p1,p1)=s.pts_P1 in
  let (pts_p2,p2)=s.pts_P2 in 
    if pts_p1>=14 then Some (Win (p1,pts_p1)) else
    if pts_p2>=14 then Some (Win (p2,pts_p2)) else
    if pts_p1=13 && pts_p2=13 then Some (Egality) else None ;;

type comparison = Equal | Greater | Smaller ;;

let compare pl resul1 resul2 = match (resul1,resul2) with 
  | (Win (p1,pts1),Win (p2,pts2)) -> 
      if pts2=pts1 then Equal else 
      if p2=pl && pts2>pts1 then Greater else
      if p2=pl && pts2<pts1 then Smaller else 
      if p1=pl && pts1>pts2 then Greater else
      if p1=pl && pts1<pts2 then Smaller else
        failwith "Compare problem" 
  | (Egality,Egality) -> Equal 
  | (_,_) -> failwith "Compare problem"  


(* Play a move *)
let play s m = 
  if is_valid s m then 
    ((*Remove a card from the P1 or P2 cards*)
      let rec remove x = function 
        | [] -> []		
        | h::tl -> if x=h then tl else h::(remove (x) (tl))
      in
      (*Get cards of P1 and P2*)
      let (liste_card_p1,p1)=s.cards_P1 in 
      let (liste_card_p2,p2)=s.cards_P2 in
      (*Get the last card played by P1 and P2*)
      let (played_card_p1,p1)=s.played_card_P1 in 
      let (played_card_p2,p2)=s.played_card_P2 in 
      (*Get scores*)
      let (score_P1,p1)=s.pts_P1 in
      let (score_P2,p2)=s.pts_P2 in 



      let new_cards_p1 = if s.p=p1 then ((remove m liste_card_p1),p1) else s.cards_P1 in
      let new_cards_p2 = if s.p=p2 then ((remove m liste_card_p2),p2) else s.cards_P2 in
      let new_played_card_P1 = if s.p=p1 then (m,p1) else s.played_card_P1 in
      let new_played_card_P2 = if s.p=p2 then (m,p2) else s.played_card_P2 in
      let new_pts_P1 = if (s.end_of_round=1 && (played_card_p1>played_card_p2 || (played_card_p1=played_card_p2 && s.p=p2)))  then ((score_P1+1),p2) else s.pts_P1 in
      let new_pts_P2 = if (s.end_of_round=1 && (played_card_p2>played_card_p1 || (played_card_p1=played_card_p2 && s.p=p2)))  then ((score_P2+1),p2) else s.pts_P2 in
      let new_pl = if (s.end_of_round=1) then s.p else next s.p in 
      let new_end_of_round = if (s.end_of_round=1) then 0 else 1 in 

        {cards_P1=new_cards_p1;
         cards_P2=new_cards_p2;
         played_card_P1=new_played_card_P1;
         played_card_P2=new_played_card_P2;
         pts_P1=new_pts_P1;
         pts_P2=new_pts_P2;
         p=new_pl;
         end_of_round=new_end_of_round})
  else 
    	failwith "Play not possible"
 	(*
let play s m = 
if is_valid s m then 
((*Remove a card from the P1 or P2 cards*)
let rec remove x = function 
| [] -> []		
| h::tl -> if x=h then remove (x) (tl) else h::(remove (x) (tl))
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
let new_cards = ((remove m list_pl),s.p) in 
(*Card played*)
let played_card = (m,s.p) in
let eor = if s.end_of_round=1 then 0 else 1 in 
(*Get the next player*)
let next_player = turn s in

if p1=s.p then 
(if s.end_of_round=1 then 
(if m>played_card_p2 then 
let new_score_P1=score_P1+1 in
{cards_P1=new_cards;
cards_P2=liste_card_p2,p2;
played_card_P1=played_card;
played_card_P2=played_card_p2,p2;
pts_P1=new_score_P1,p1;
pts_P2=score_P2,p2;
p=next_player;
end_of_round=eor}
else
let new_score_P2=score_P2+1 in
{cards_P1=new_cards;
cards_P2=liste_card_p2,p2;
played_card_P1=played_card;
played_card_P2=played_card_p2,p2;
pts_P1=score_P1,p1;
pts_P2=new_score_P2,p2;
p=next_player;
end_of_round=eor})
else
{cards_P1=new_cards;
cards_P2=liste_card_p2,p2;
played_card_P1=played_card;
played_card_P2=played_card_p2,p2;
pts_P1=score_P1,p1;
pts_P2=score_P2,p2;
p=next_player;
end_of_round=eor})
else
(if s.end_of_round=1 then 
(if m>played_card_p1 then 
let new_score_P2=score_P2+1 in 
{cards_P1=liste_card_p1,p1;
cards_P2=new_cards;
played_card_P1=played_card_p1,p1;
played_card_P2=played_card;
pts_P1=score_P1,p1;
pts_P2=new_score_P2,p2;
p=next_player;
end_of_round=eor}
else
let new_score_P1=score_P1+1 in
{cards_P1=liste_card_p1,p1;
cards_P2=new_cards;
played_card_P1=played_card_p1,p1;
played_card_P2=played_card;
pts_P1=new_score_P1,p1;
pts_P2=score_P2,p1;
p=next_player;
end_of_round=eor})
else
{cards_P1=liste_card_p1,p1;
cards_P2=new_cards;
played_card_P1=played_card_p1,p1;
played_card_P2=played_card;
pts_P1=score_P1,p1;
pts_P2=score_P2,p2;
p=next_player;
end_of_round=eor}))
else
failwith "move not possible";;
*)
(* ########################### TESTS ##############################*)

let state_test={
  cards_P1=[1;2;3;4;5;6;7;8;9],Human       ;
  cards_P2=[3;4;6;8;9;10;11;3;2],Comput    ;
  played_card_P1=2,Human                   ;
  played_card_P2=3,Comput                  ;
  pts_P1=8,Human                           ;
  pts_P2=10,Comput                         ;
  p=Human                                  ; 
  end_of_round=0                           ; 
};;

let state_test2={
  cards_P1=[1;2;3;4;5;6;7;8;9],Human       ;
  cards_P2=[3;4;6;8;9;10;11;3;2],Comput    ;
  played_card_P1=2,Human                   ;
  played_card_P2=3,Comput                  ;
  pts_P1=14,Human                          ;
  pts_P2=10,Comput                         ;
  p=Human                                  ; 
  end_of_round=1                           ; 
};;

let state_test3={
  cards_P1=[1;2;3;4;5;6;7;8;9],Human       ;
  cards_P2=[],Comput                       ;
  played_card_P1=2,Human                   ;
  played_card_P2=3,Comput                  ;
  pts_P1=14,Human                          ;
  pts_P2=10,Comput                         ;
  p=Comput                                 ; 
  end_of_round=1                           ; 
};;

let state_test4={
  cards_P1=[1;2;3;4;5;6;7;8;9],Human       ;
  cards_P2=[],Comput                       ;
  played_card_P1=2,Human                   ;
  played_card_P2=3,Comput                  ;
  pts_P1=1,Human                           ;
  pts_P2=17,Comput                         ;
  p=Comput                                 ; 
  end_of_round=1                           ; 
};;

let state_test5={
  cards_P1=[1;2;3;4;5;6;7;8;9],Human       ;
  cards_P2=[],Comput                       ;
  played_card_P1=2,Human                   ;
  played_card_P2=3,Comput                  ;
  pts_P1=13,Human                          ;
  pts_P2=13,Comput                         ;
  p=Comput                                 ; 
  end_of_round=1                           ; 
};;

(* Print a state *)
Printf.printf "%s%!\n" (state2s state_test);;      (*########### OK ############*)
(* Print a move *)
Printf.printf "%s%!\n" (move2s 3);;                (*########### OK ############*)
(* Print a result *)
Printf.printf "%s%!\n" (result2s (Win (Human,15)));;  (*########### OK ############*)
Printf.printf "%s%!\n" (result2s Egality);;        (*########### OK ############*)
(* initialization test *)
Printf.printf "%s%!\n" (state2s initial);;         (*########### OK ############*)
(* turn test *)
Printf.printf "%s%!\n" (player2s (turn initial));; (*########### OK ############*)
Printf.printf "%s%!\n" (player2s (turn state_test));;(*######### OK ############*)
(* is_valid test *)
is_valid state_test 1;;                            (*########### OK ############*)
is_valid state_test 10;;                           (*########### OK ############*)
is_valid state_test2 1;;                           (*########### OK ############*)
is_valid state_test3 1;;                           (*########### OK ############*)
(* all_moves test *)
all_moves state_test;;                             (*########### OK ############*)
all_moves state_test3;;                            (*########### OK ############*)
(* result test *)
let test_result s= match result s with 
  | None -> Printf.printf "No result%!\n"
  | Some (x) -> Printf.printf "%s%!\n" (result2s x);;
test_result state_test5;                           (*########### OK ############*)
test_result state_test4;                           (*########### OK ############*)
test_result state_test3;                           (*########### OK ############*)
test_result state_test;;                           (*########### OK ############*)
(* compare test *)
compare Comput (Win (Comput,15)) (Win (Human,17));;(*########### OK ############*)
compare Comput (Win (Human,13)) (Win (Comput,13));;(*########### OK ############*)
compare Comput (Win (Human,1)) (Win (Comput,13));; (*########### OK ############*)
compare Human (Win (Comput,13)) (Win (Human,15));; (*########### OK ############*)
compare Human (Win (Comput,13)) (Win (Human,1));;  (*########### OK ############*)
compare Human (Egality) (Egality);;                (*########### OK ############*)

(* play test *)
(* At the end of these play successions the score should be p1:9 - 12:p2 *)
let play1=play state_test 4;;
let play2=play play1 8;;
let play3=play play2 9;;
let play4=play play3 9;;
let play5=play play4 6;;
let play6=play play5 6;;
(*let play5=play play4 10;;*)
Printf.printf "%s%!\n" (state2s state_test);;      (*########### OK ############*)
Printf.printf "%s%!\n" (state2s play1);;           (*########### OK ############*)
Printf.printf "%s%!\n" (state2s play2);;           (*########### OK ############*)
Printf.printf "%s%!\n" (state2s play3);;           (*########### OK ############*)
Printf.printf "%s%!\n" (state2s play4);;           (*########### OK ############*)
Printf.printf "%s%!\n" (state2s play5);;           (*########### OK ############*)
Printf.printf "%s%!\n" (state2s play6);;           (*########### OK ############*)

