


let todo () = failwith "TODO"

let singletons (xs : 'a list) : 'a list list = 
  List.map (fun x -> [x]) xs;;

let map2d (f : 'a -> 'b) (xss : 'a list list) : 'b list list = 
  List.map (fun x -> List.map f x) xss;;

let product (xs : 'a list) (ys : 'b list) : ('a * 'b) list list = 
  List.map (fun x -> List.map (fun y -> (x, y)) ys) xs;;


let power (xs : 'a list) : 'a list list = todo ()

let both : 'a option -> 'b option -> ('a * 'b) option =
 fun x -> match x with Some x -> todo () | None -> todo ()
