open Grid
open Base

let rec member i j l = 
  match l with
  | [] -> false
  | (a,b)::q -> (a,b)=(i,j) || member i j q

let rec searchElInSnakeRec i j s b nls = 
  match s with
  | [] -> (b, nls)
  | ((ls,_) as t)::q -> 
    if member i j ls
    then  searchElInSnakeRec i j q true (t::nls)
    else searchElInSnakeRec i j q b nls

let searchElInSnake i j s = searchElInSnakeRec i j s false []

let rec outSnakes i j s cs neighboor = 
  match cs with
  | [] -> []
  | t::q ->
    if member (getNthElement s neighboor) t
    then 
      

  
let rec updateSnakesList i j s snakes neighboor= 
  let (exist, cs) = searchElInSnake i j snakes in
  if exist 
  then outSnakes i j s cs neighboor
  else addInSnake i j s neighboor
