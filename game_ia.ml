open Game

(* Stupid IA: it take the first possible valid move.*)
let best_move state =
  match List.filter (is_valid state) (all_moves state) with
    | [] -> assert false
    | m :: _ ->
        let player = turn state in
          (Some m, worst_for player)






let find_max pl liste = 

  let rec aux acc pl = function 
    | [] -> acc_max
    | hd::tl -> 
        let (mv,res) = hd in 
          match compare pl acc res with 
            | Equal -> aux acc pl tl
            | Greater -> aux res tl
            | Smaller -> aux acc tl 
  in
    aux (worst_for (pl)) (pl) (liste) ;;




let best_move state =

  let rec aux liste_mv_possible = match liste_mv_possible with 
    (*là on a la liste de mouvement possible valide à partir de state*)
    | [] -> []
    | mv :: tl ->  
        (let state_mv = play mv in 
           (match result state_mv with 
             | None -> (find_max (turn state_mv) (aux (List.filter (is_valid state_mv) (all_moves state_mv)) ) )::aux tl (*pb de typage*)
             | Some res -> (Some mv,res)::aux tl  )

  in 
    find_max (turn state_mv) (aux (List.filter (is_valid state) (all_moves state))) ;;


(*
let (mv2,res) = best_move (play state) in 
if res = worst_for player then 
(Some mv2, res) else 
let (a,b)=state in 
	best_move ((mv1+1),b)	





| [mv1] -> (match (result (play mv1)) with 
| None -> [(Some mv1,0)]
| Some res -> (Some mv1,res))

*)
;;

