open Game

(* Returns the move the current player can do to reach the best result. 
 * Returns None if the game is finished in the current state. *)
val best_move: state -> move option * result

(*same algorithm that the former one but the recursion depth is now 
limited by the value depth*)
val best_move_with_depth: state*int -> move option * result
