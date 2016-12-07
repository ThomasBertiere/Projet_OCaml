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

type result = Win of player | Egality ;;


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
  | Win (p) -> (player2s p) ^ " wins"
  | Egality -> "Egality"


let random_card remaning_card =
  let rec selected_card card_list i = 
    if i = 0 then List.hd card_list else selected_card (List.tl card_list) (i-1) 
  in 
  let (*rand1 donne l'indice*) in
  let rand2
        if (selected_card card_list le_rand)!=-1 then 

let initial =  
  random_card [1;1;1;1;2;2;2;2;3;3;3;3;4;4;4;4;5;5;5;5;6;6;6;6;7;7;7;7;8;8;8;8;9;9;9;9;10;10;10;10;11;11;11;11;12;12;12;12;13;13;13;13];;


(* ########################### TEST #####################*)

let state_test={
  cards_P1=[1;2;3;4;5;6;7;8;9],Human       ;
  cards_P2=[3;4;6;8;9;10;11;3;2],Comput    ;
  played_card_P1=2,Human                   ;
  played_card_P2=3,Comput                  ;
  pts_P1=8,Human                           ;
  pts_P2=10,Comput                         ;
  p=Human                                  ; 
  end_of_round=1                           ; 
};;

Printf.printf "%s%!\n" (state2s state_test);;      (*########### OK ############*)
Printf.printf "%s%!\n" (move2s 3);;                (*########### OK ############*)
Printf.printf "%s%!\n" (result2s (Win (Human)));;  (*########### OK ############*)
Printf.printf "%s%!\n" (result2s Egality);;        (*########### OK ############*)










