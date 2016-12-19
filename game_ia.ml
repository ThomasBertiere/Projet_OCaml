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
        let res = f arg in 
          Hashtbl.add memory arg res ;
          res
      end


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
    (find_max (turn state) (aux l_possible_mv));;





(*Pour guillaume *) 

let rec best_move state =

  (*convert a mv list into a state list (play each move)*)
  let rec mv_to_state state = function 
    | [] -> []
    | hd::tl -> (play state hd)::(mv_to_state state tl)
  in

  let rec aux list_possible_mv = 
    let list_simplified=supr_occur list_possible_mv in 
    (* là on a la liste simplifé des mv possible, on cree donc la liste des state possible*)
    let state_list = mv_to_state state list_simplified in 
      (*là on a la liste des etats atteignable depuis state*)

      match state_list with 
        | [] -> []
        | st :: tl -> 
            (match result st with 
              | None -> (*si t'as pas de resultat tu fais ton fold f etc*)
                  (*let (a,b)=(cache (best_move) (state_mv)) in (Some mv,b)::aux tl *)
              | Some res -> (Some mv,res)(* tu fais ton fold f avec ce resulat (je vois pas trop la chose*)



  in 

  let l_possible_mv=List.filter (is_valid state) (all_moves state) in 
    (find_max (turn state) (aux l_possible_mv));;







(*
avec profondeur (marche pas + pb de cache)

let rec best_move_with_depth state depth=
let rec aux list_possible_mv depth= 
let list_simplified=supr_occur list_possible_mv in 
match list_simplified with 
(*là on a la liste de mouvement possible valide à partir de state*)
| [] -> []
| mv :: tl -> 
let state_mv =(play (state) (mv)) in 
(match result state_mv with 
| None -> let (a,b)= if depth=0 then (Some mv,result_socre state_mv) else (best_move_with_depth (state_mv) (depth-1)) in (Some mv,b)::aux tl depth
| Some res -> (Some mv,res)::aux tl depth)
in 
let l_possible_mv=List.filter (is_valid state) (all_moves state) in 
let  (a,b)=(find_max (turn state) (aux l_possible_mv depth)) in (a,b);;





*)
