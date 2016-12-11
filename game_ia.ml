open Game

(*Stupid IA: it take the first possible valid move.
  let best_move state =
  match List.filter (is_valid state) (all_moves state) with
  | [] -> assert false
  | m :: _ ->
  let player = turn state in
  (Some m, worst_for player)

*)

let find_max pl liste = 

  let rec aux acc pl = function 
    | [] -> acc
    | hd::tl -> let (mv,res)=hd in
        let (acc_mv,acc_res) = acc in 
          match compare pl acc_res res with 
            | Equal -> aux hd pl tl
            | Greater -> aux hd pl tl
            | Smaller -> aux acc pl tl 
  in
  let (a,b)=(List.hd liste) in
    aux (a,worst_for (pl)) (pl) (liste) ;;


(*
let rec best_move state =
let rec aux liste_mv_possible = match liste_mv_possible with 
(*là on a la liste de mouvement possible valide à partir de state*)
| [] -> []
| mv :: tl -> 
let state_mv =(play (state) (mv)) in 
(match result state_mv with 
| None -> Printf.printf "%s%!\n" (state2s state_mv);(best_move state_mv) :: aux tl 
| Some res -> (Some mv,res)::aux tl  )
in 
Printf.printf "IA%!";Printf.printf " State : %s%!\n" (state2s state);
let (a,b)=(find_max (turn state) (aux (List.filter (is_valid state) (all_moves state)))) in 
match a with 
| Some(m) -> Printf.printf "%s - %s\n%!" (move2s m) (result2s b);(a,b)  ;;



let rec best_move state =

let rec aux liste_mv_possible = match liste_mv_possible with 
(*là on a la liste de mouvement possible valide à partir de state*)
| [] -> []
| mv :: tl -> 
let state_mv =(play (state) (mv)) in 
(match result state_mv with 
| None -> (best_move state_mv) :: aux tl 
| Some res -> (Some mv,res)::aux tl  )
in 
let l_mv_possible=(List.filter (is_valid state) (all_moves state)) in 
if l_mv_possible=[] then (None,worst_for (turn state)) else 
find_max (turn state) (aux l_mv_possible) ;;

*)

let rec best_move state =

  let rec aux liste_mv_possible = match liste_mv_possible with 
    (*là on a la liste de mouvement possible valide à partir de state*)
    | [] -> []
    | mv :: tl -> 
        let state_mv =(play (state) (mv)) in 
          (match result state_mv with 
            | None -> let (a,b)=(best_move state_mv) in (Some mv,b)::aux tl 
            | Some res -> (Some mv,res)::aux tl)
  in 
  let l_mv_possible=List.filter (is_valid state) (all_moves state) in 
  (*if l_mv_possible=[] then  (None,worst_for (turn state)) else *)
  let  (a,b)=(find_max (turn state) (aux l_mv_possible)) in (a,b);;





