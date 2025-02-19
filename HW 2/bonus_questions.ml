let compress (equal : 'a -> 'a -> bool) (input_list : 'a list) : 'a list = 
  List.fold_right (fun x1 x2 ->
                  match x2 with 
                  |[] -> [x1]
                  |h::t -> if (equal x1 h) then (x2) else (x1::x2)
                  ) input_list [];;

let max (input_list : int list) : int option = 
  match input_list with 
  | [] -> None
  | h::t -> (Some (List.fold_right (fun x1 x2 -> if(x1 > x2) then (x1) else (x2)) input_list Int.min_int));;

let join (input_list : 'a option list) : 'a list option = 
  List.fold_right (fun x acc -> match x with
                                Some x -> )