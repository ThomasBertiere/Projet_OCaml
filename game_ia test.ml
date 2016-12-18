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

(* These types are abstract in game.mli *)


(* ####################### TYPES DEFINITION ########################## *)

(* The type state represents the game state. It contains the remaning cards of each players, the played card of each player, their points, the player who has to play and the end of the round*)
type state = { cards_P1: int list*player  ;
               cards_P2: int list*player  ;
               played_card_P1: int*player ;
               played_card_P2: int*player ;
               pts_P1: int*player         ;
               pts_P2: int*player         ;
               p : player                 ; 
               end_of_round : int         ; 
             }

(* This type reprensents a movee. Here a move is a card which is represented by a int *)
type move = int 

(*The type result is the player victory with his score or an egality *)
type result = Win of player | Egality 

(* Type used to compare results *)
type comparison = Equal | Greater | Smaller ;;



(*############################### PRINTERS ###############################*)

(*convert a intger list in string*)
let listcards2s l =
  let rec aux = function 
    | [] -> " ]"
    | [a] -> " "^string_of_int a^aux [] 
    | hd::tl ->" "^string_of_int hd^" , "^aux tl 
  in
    "["^aux l;;

(*convert a state in string*)
let state2s s = 
  let (card_p1,p1)=s.cards_P1 in
  let (card_p2,p2)=s.cards_P2 in
  let (played_card_p1,p1)=s.played_card_P1 in
  let (played_card_p2,p2)=s.played_card_P2 in
  let (pts_p1,p1)=s.pts_P1 in
  let (pts_p2,p2)=s.pts_P2 in
  let round=if s.end_of_round=0 then "\n################### NEW ROUND ###################\n\n" else "" in 
    Printf.sprintf "%s Current state : \n   Player to play : %s\n   Player 1 : %s\n      Cards : %s\n      Played card : %d\n      Pts : %d\n   Player 2 : %s\n      Cards : %s\n      Played card : %d\n      Pts : %d\n   End of round : %d%!\n\n" round (player2s s.p) (player2s p1) (listcards2s card_p1) played_card_p1 pts_p1 (player2s p2) (listcards2s card_p2) played_card_p2 pts_p2 s.end_of_round;;

(*convert a move in string*)
let move2s n = Printf.sprintf " Joue : %d" n 

(*convert a result in string*)
let result2s = function 
  | Win (pl) -> (player2s pl) ^ " wins" 
  | Egality -> "Egality"


(*############################### READERS ###############################*)

(*read a move*)
let readmove s = try Some (int_of_string s) with _ -> None







(*################################ FUNCTIONS #################################*)

(*return a random distribution between 2 players of a 52 cards game*)
let rec random_card = function 
  | [] -> [],[]
  | hd::tl -> 
      let (p1,p2)=random_card tl in
        if (Random.int 2)= 0 then 
          if (List.length p1)<6 then 
            (hd::p1),p2 
          else 
            p1,(hd::p2) 
        else
        if (List.length p2)<6 then 
          p1,(hd::p2)
        else 
          (hd::p1),p2 

(*return an initial state with a random distribution*)
let initial =  
  Random.self_init() ;
  let card_p1,card_p2=random_card [1;2;3;4;5;6;7;8;9;10;11;12] in
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


(* Indicates which player must play now. *)
let turn state = state.p

(*check if the move is valid or not. The played card should be in the player hand and the game should not be finished (score>14)*)
let is_valid s m =
  let rec member x = function 
    | [] -> false 
    | h::tl -> if h=x then true else member x tl 
  in 
  let (liste_card_p1,p1)=s.cards_P1 in 
  let (liste_card_p2,p2)=s.cards_P2 in
  let list_pl = if p1=s.p then liste_card_p1 else liste_card_p2 in
  let in_hand=(member m list_pl) in 
  let sc_p1,a=s.pts_P1 in
  let sc_p2,a=s.pts_P2 in 
    (*Printf.printf "%s%!\n" (listcards2s list_pl);*)
    (not(liste_card_p1=[] && liste_card_p2=[])) && sc_p2<=3 && sc_p1<=3 && in_hand;;


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
      (*faut modifier les 2 dernière ligne, prendre en compte la nouvelle carte jouée*)
      let new_pts_P1 = if (s.end_of_round=1 && ( (played_card_p1>m && s.p=p2) || (m>played_card_p2 && s.p=p1) || (played_card_p1=m && s.p=p2) ) ) then ((score_P1+1),p1) else s.pts_P1 in
      let new_pts_P2 = if (s.end_of_round=1 && ( (m>played_card_p1 && s.p=p2) || (played_card_p2>m && s.p=p1) || (played_card_p2=m && s.p=p1))) then ((score_P2+1),p2) else s.pts_P2 in
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

(* return all the possible move considering a state*)
let all_moves s =
  let (liste_card_p1,p1)=s.cards_P1 in 
  let (liste_card_p2,p2)=s.cards_P2 in
    if p1=s.p then liste_card_p1 else liste_card_p2;;

(* return a result or not (result option) for a considerated state*)
let result s = 
  let (pts_p1,p1)=s.pts_P1 in
  let (pts_p2,p2)=s.pts_P2 in 
    if pts_p1>=4 then Some (Win (p1)) else
    if pts_p2>=4 then Some (Win (p2)) else
    if pts_p1=3 && pts_p2=3 then Some (Egality) else None ;;

(*function which compare 2 results on the player pl point of view *)
let compare pl resul1 resul2 = match (resul1,resul2) with 
  | (Win (p1),Win (p2)) -> 
      (if p2=p1 then Equal else 
       if p2=pl then Greater else
       if p1=pl then Smaller else failwith "Compare problem" )
  | (Egality,Egality) -> Equal 
  | (Win(p1),Egality) -> if p1=pl then Smaller else Greater  
  | (Egality,Win(p1)) -> if p1=pl then Greater else Smaller  

(* Returns the worst possible score for the given player*)
let worst_for p = Win (next p);;




















































let compare_mv (mv1,res1) (mv2,res2) = if mv1>mv2 then (mv2,res2) else (mv1,res1);;

let find_max pl liste = 

  let rec aux acc pl = function 
    | [] -> acc
    | hd::tl -> let (mv,res)=hd in
        let (acc_mv,acc_res) = acc in 
        let rien = (Printf.printf "On est equal : r1 : %s  vs r2 : %s     -----      mv1 : %s  vs  mv2 : %s  ---- mv selec : \n%!" result2s(res) result2s(acc_res) move2s(mv) move2s(acc_mv)); 1 in 
          match compare pl acc_res res with 
            | Equal -> aux (compare_mv(hd) (acc)) pl tl;
            | Greater -> aux hd pl tl
            | Smaller -> aux acc pl tl 
  in
  let (a,b)=(List.hd liste) in
    aux (a,worst_for (pl)) (pl) (liste) ;;





let rec supr_occur  = function
  | [] -> []
  | [a] -> a::supr_occur []
  | h1::h2::tl -> 
      if h1=h2 then supr_occur (h2::tl) else h1::supr_occur (h2::tl) ;;


let memory = (*Printf.printf"creat\n%!";*)Hashtbl.create 20;;

let cache f =
  fun arg ->
    if Hashtbl.mem memory arg then 
      begin
        (*Printf.printf "dans htl\n%!";*)
        Hashtbl.find memory arg
      end 
    else
      begin
        let res = f arg in 
          Hashtbl.add memory arg res ;
          res
      end

let listcards2s l =
  let rec aux = function 
    | [] -> " ]"
    | [a] -> " "^move2s a^aux [] 
    | hd::tl ->" "^move2s hd^" , "^aux tl 
  in
    "["^aux l;;

let rec best_move state =
  let rec aux list_possible_mv = 
    let list_simplified=supr_occur list_possible_mv in 
      match list_simplified with 
        (*là on a la liste de mouvement possible valide à partir de state*)
        | [] -> []
        | mv :: tl -> 
            let state_mv =(play (state) (mv)) in 
              (match result state_mv with 
                | None -> let (a,b)=(cache (best_move) (state_mv)) in (Some mv,b)::aux tl 
                | Some res -> (Some mv,res)::aux tl)
  in 
  let l_possible_mv=List.filter (is_valid state) (all_moves state) in 
  let  (a,b)=(find_max (turn state) (aux l_possible_mv)) in (a,b);;





