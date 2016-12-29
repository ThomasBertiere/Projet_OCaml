open Game
open Functory.Network
open Functory.Network.Same

(*Stupid IA: it take the first possible valid move.
  let best_move state =
  match List.filter (is_valid state) (all_moves state) with
  | [] -> assert false
  | m :: _ ->
  let player = turn state in
  (Some m, worst_for player)

*)



(* ############ CACHE ############ 

   functions used to cache IA algorihm

*)

let memory = Hashtbl.create 20;;

let cache f =
  fun arg ->
    if Hashtbl.mem memory arg then 
      begin
        Hashtbl.find memory arg
      end 
    else
      begin
        let res = f arg in 
          Hashtbl.add memory arg res ;
          res
      end


(* ############################# *)


(*This function is used to delete occurences in a list*)
let rec del_occur  = function
  | [] -> []
  | [a] -> a::del_occur []
  | h1::h2::tl -> 
      if h1=h2 then del_occur (h2::tl) else h1::del_occur (h2::tl) ;;


(*Among a (Move option * result ) list, this function return the element of the list which 
  contains the best result for the player pl.*)
let find_best_result pl liste = 

  (*Auxilary function to recursivly compute the max (the best result) of the list.
    acc is the max memorization, it is initialized with the worst result for the player pl*)
  let rec aux acc pl = function 
    | [] -> acc
    | hd::tl -> let (mv,res)=hd in
        let (acc_mv,acc_res) = acc in 
          match compare pl acc_res res with 
            (*| Equal ->  aux (compare_mv (hd) (acc) pl) pl tl*)
            (*if the list element is greater than or equal to the max (acc), the max (acc) becomes the list element*)
            | Equal -> aux hd pl tl
            | Greater -> aux hd pl tl
            | Smaller -> aux acc pl tl 
  in
  let (a,b)=(List.hd liste) in
    aux (a,worst_for (pl)) (pl) (liste) ;;


(*Function that returns the best result the current player can reach and the move 
  leading to this result 

  This function is not distibuted 

  This is the f function of the map_fold_ac uses for distribution
*)
let rec f_best_move state =

  (*This auxilary function return a list of reachable results and the moves leading
    to theses result from the initial state*)
  let rec aux list_possible_mv = 
    (*as we do not consider cards color, the move 1 and 1 are the same for instance, to avoid 
      recalculation we delete multiple occurences in the list of possible moves*)  
    let list_simplified=del_occur list_possible_mv in 
      match list_simplified with 
        | [] -> []
        | mv :: tl -> 
            (*we play the move*)
            let state_mv =(play (state) (mv)) in 
              (match result state_mv with
                (*if after playing the move we do not have result, the game is not finished so 
                  we recursivly call f_best_move. Here f_best_move is cached to avoid recalculation*)
                | None -> let res = (match cache (f_best_move) (state_mv) with
                                      (* here f_best_move return the best result reachable from state_mv. So we 
                                         have to return this result and mv, mv is the move from where we can reach this best 
                                         result*)
                                      | [(_,result)] -> (Some mv,result) 
                                      | _ -> failwith "error" )
                (* Then we concatene this result with results given by the other possible move from state*)
                    in res::aux tl 
                (* if after playing we have a result, the game is finished so we return this 
                   result and the move from where we can reach this result *)
                | Some result -> (Some mv,result)::aux tl)
  in 
  (*from the current state we build a list of all possible moves*)
  let l_possible_mv=List.filter (is_valid state) (all_moves state) in 
  (*We find the best result among all the reachable result from state*)
  let _,result=find_best_result (turn state) (aux l_possible_mv) in 
    (*We return the move option associates to the best reachable result, the move is the one that allow us to reach result*)
    [(Some (last_played_card state),result)];;


(*Fold function used in map_fold_ac for distribution

  This function only concatenes two lists
*)
let rec fold acc x = match x with 
  | [] -> acc 
  | [res] -> res::acc 
  | hd::tl -> hd::fold acc tl


(*Function that returns the best move the current player can make to win

  This function uses distribution to calculate the best move : 

  To do so we build a list of reachable state from the current state and we distribute 
  each reachable state of that list to different workers. Each worker will run 
  a best move algorithm on this state to return the best move he can do. The worker 
  algortihm isn't distributed. 
  When every workers finish their work, we build a list of worker's results. Theses results
  are the best result worker can reach from the inital move (the one used to reach the state
  given at the distribution). Then we find the best result among that list, the move
  associates to this best result becomes the best move and we return its value.

*)
let best_move state = 

  let rec move_list_to_state_list = function 
    | [] -> []
    | mv :: tl -> (play state mv)::(move_list_to_state_list tl)           
  in  
  (*from the current state we build a list of all possible moves*)
  let l_possible_mv=List.filter (is_valid state) (all_moves state) in
  (*as we do not consider cards color, the move 1 and 1 are the same for instance, to avoid 
    recalculation we delete multiple occurences in the list of possible moves*)  
  let list_simplified= del_occur l_possible_mv in 
  (*we play each move of the possible move list. In that way we obtain a
    list of reachable states from the inital one*)
  let list_state_possible = move_list_to_state_list list_simplified in
  (*Calcul distribution, return a list of (Move option * result). Each element of that list has 
    been compute on different workers*)
  let reachable_result_list = map_fold_ac ~f:f_best_move ~fold:fold [] list_state_possible in
    (* We sleep the system as he can launch best_move algorithms in row*)
    Unix.select [] [] [] 1.0  ;
    (*We return the move option associates to the best reachable result*)
    find_best_result (turn state) reachable_result_list;;







(* ############ CACHE WITH DEPTH ############ 

   functions used to cache IA algorihm

*)

let memory_depth = Hashtbl.create 20;;

let cache_depth f (state,depth)=
  if Hashtbl.mem memory_depth state then 
    begin
      Hashtbl.find memory_depth state
    end 
  else
    begin
      let res = f (state,depth) in 
        Hashtbl.add memory_depth state res ;
        res
    end

(* ######################################### *)


(*Same function as before but the recusive depth is limited by the value depth*)
let rec f_best_move_with_depth (state,depth) =

  (*This auxilary function return a list of reachable results and the moves leading
    to theses result from the initial state*)
  let rec aux list_possible_mv = 
    (*as we do not consider cards color, the move 1 and 1 are the same for instance, to avoid 
      recalculation we delete multiple occurences in the list of possible moves*)  
    let list_simplified=del_occur list_possible_mv in 
      match list_simplified with 
        | [] -> []
        | mv :: tl -> 
            (*we play the move*)
            let state_mv =(play (state) (mv)) in 
              (match result state_mv with
                (*if after playing the move we do not have result, the game is not finished so 
                  we recursivly call f_best_move if the depth is not reach. Here f_best_move is cached 
                  to avoid recalculation. If the depth is reach we compute a result thanks to the 
                  actual score (result_score) *)
                | None -> let res = if depth=0 
                            then (Some mv,result_socre state_mv) 
                            else (match cache_depth (f_best_move_with_depth) (state_mv,(depth-1)) with
                                   (* here f_best_move return the best result reachable from state_mv. So we 
                                      have to return this result and mv, mv is the move from where we can reach this best 
                                      result*)
                                   | [(_,result)] -> (Some mv,result) 
                                   | _ -> failwith "error" )
                            (* Then we concatene this result with results given by the other possible move from state*)
                    in res::aux tl 
                (* if after playing we have a result, the game is finished so we return this 
                   result and the move from where we can reach this result *)
                | Some result -> (Some mv,result)::aux tl)
  in 
  (*from the current state we build a list of all possible moves*)
  let l_possible_mv=List.filter (is_valid state) (all_moves state) in 
  (*We find the best result among all the reachable result from state*)
  let _,result=find_best_result (turn state) (aux l_possible_mv) in 
    (*We return the move option associates to the best reachable result, the move is the one that allow us to reach result*)
    [(Some (last_played_card state),result)];;



(*same algorithm that the former one but the recursion depth is now 
  limited by the value depth*)
let best_move_with_depth (state,depth)  = 

  (*the depth is add into the list as the f function will know the recursive depth*)
  let rec move_list_to_state_list = function 
    | [] -> []
    | mv :: tl -> ((play state mv),depth)::(move_list_to_state_list tl)           
  in  
  (*from the current state we build a list of all possible moves*)
  let l_possible_mv=List.filter (is_valid state) (all_moves state) in
  (*as we do not consider cards color, the move 1 and 1 are the same for instance, to avoid 
    recalculation we delete multiple occurences in the list of possible moves*)  
  let list_simplified= del_occur l_possible_mv in 
  (*we play each move of the possible move list. In that way we obtain a
    list of reachable states from the inital one*)
  let list_state_possible = move_list_to_state_list list_simplified in
  (*Calcul distribution, return a list of (Move option * result). Each element of that list has 
    been compute on different workers*)
  let reachable_result_list = map_fold_ac ~f:f_best_move_with_depth ~fold:fold [] list_state_possible in
    (* We sleep the system as he can launch best_move algorithms in row*)
    Unix.select [] [] [] 1.0  ;
    (*We return the move option associates to the best reachable result*)
    find_best_result (turn state) reachable_result_list;;




