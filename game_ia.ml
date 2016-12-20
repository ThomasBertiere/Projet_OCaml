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


let find_max pl liste = 

  let rec aux acc pl = function 
    | [] -> acc
    | hd::tl -> let (mv,res)=hd in
        let (acc_mv,acc_res) = acc in 
          match compare pl acc_res res with 
            (*| Equal ->  aux (compare_mv (hd) (acc) pl) pl tl*)
            | Equal -> aux hd pl tl
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


let memory = Hashtbl.create 20;;

let cache f =
  fun arg ->
    if Hashtbl.mem memory arg then 
      begin
        Hashtbl.find memory arg
      end 
    else
      begin
        let res = match f arg with | [a] -> a | _ -> failwith "error" in 
          Hashtbl.add memory arg res ;
          res
      end


let rec f_best_move state =
  let rec aux list_possible_mv = 
    let list_simplified=supr_occur list_possible_mv in 
      match list_simplified with 
        (*là on a la liste de mouvement possible valide à partir de state*)
        | [] -> []
        | mv :: tl -> 
            let state_mv =(play (state) (mv)) in 
              (match result state_mv with 
                | None -> let res = match f_best_move state_mv with | [a] -> a | _ -> failwith "error" in res::aux tl 
                | Some res -> (Some mv,res)::aux tl)
  in 
  let l_possible_mv=List.filter (is_valid state) (all_moves state) in 
    [find_max (turn state) (aux l_possible_mv)];;

let fold acc x = Printf.printf "===GO===\n%!" ; match x with | [] -> acc | [res] -> res::acc | _ -> failwith "error"


let best_move state = 
	let rec aux = function 
		| [] -> []
    | mv :: tl -> (play state mv)::(aux tl)           
  in  
	let l_possible_mv=List.filter (is_valid state) (all_moves state) in
	let list_simplified= supr_occur l_possible_mv in 
	let list_state_possible = aux list_simplified in 
	Printf.printf "******OKK******\n%!" ; 
  let result = map_fold_ac ~f:f_best_move ~fold:fold [] list_state_possible in
	Printf.printf "******OKK******\n%!" ; 

	find_max (turn state) result
	

	 










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


let rec best_move_with_depth (state,depth)=
  let rec aux list_possible_mv depth= 
    let list_simplified=supr_occur list_possible_mv in 
      match list_simplified with 
        (*là on a la liste de mouvement possible valide à partir de state*)
        | [] -> []
        | mv :: tl -> 
            let state_mv =(play (state) (mv)) in 
              (match result state_mv with 
                | None -> let (a,b)= if depth=0 then (Some mv,result_socre state_mv) else (cache_depth (best_move_with_depth) (state_mv,(depth-1))) in (Some mv,b)::aux tl depth
                | Some res -> (Some mv,res)::aux tl depth)
  in 
  let l_possible_mv=List.filter (is_valid state) (all_moves state) in 
    find_max (turn state) (aux l_possible_mv depth);;





