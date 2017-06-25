open Grid
open Base

(*
  @member
  Check if element is in lits
  @param e 'a element to search
  @param l ('a list) list to search in
  @return true if e in l, false else
*)
let rec member e l = 
  match l with
  | [] -> false
  | t::q -> t=e || member e q

(*
  @split
  Split liste in two art
  @param l 'a list list to split
  @return list cut in 2 as couple
*)
let rec split l = 
  match l with
  | [] | _::[] -> (l, [])
  | t::t1::q -> let (l1,l2) = split q in (t::l1,t1::l2)

(*
  @merge
  Merge sorted list into a sorted list
  @param l1/l2 ('a list) list to merge
  @param c ('a -> 'a -> bool) comp function
  @return sorted merged of l1 and l2
*)
let rec merge l1 l2 c =
  match (l1, l2) with
  | ([], l) | (l, []) -> l
  | (t1::q1, t2::q2) -> if c t1 t2 then t1::merge q1 l2 c else t2::merge l1 q2 c

let snakeSort a b = 
  let (_,v1) = a and (_,v2) = b in v1 > v2

(*
  @quickSort
  quickly sort list of any kind
  @param l ('a list) list to sort
  @param c ('a -> 'a -> bool) comparaison function
  @return sorted l
*)
let rec quickSort l c= 
  match l with 
  | [] -> []
  | t::[] -> t
  | _ -> let (l1, l2) = split l in merge (quickSort l1 c) (quickSort l2 c) c

(*
  @searchElInSnakeRec
  Recursive call for searchElInSnake. 
  @param i int  indice of the play
  @param j char ord of the play
  @param s int side of the play
  @param b boolean to see if we found or not a snake for (i,j)
  @param nls list of the snakes containing (i,j)
  @return a list of all snakes containing played case
*)
let rec searchElInSnakeRec i j s b ols nls = 
  match s with
  | [] -> (b, ols, nls)
  | ((ls,_) as t)::q -> 
    if member (i, j) ls
    then  searchElInSnakeRec i j q true ols (t::nls)
    else searchElInSnakeRec i j q b (t::ols) nls

(*
  @searchElInSnakeRec
  Search for e in snakes. 
  @param i int  indice of the play
  @param j char ord of the play
  @param s int side of the play
  @return a list of all snakes containing played case
*)
let searchElInSnake i j s = searchElInSnakeRec i j s false [] []

(*
  @calculateSnake
  Calculate snake value
  @param l (snake list) list to calculate
*)
let rec calculateSnake l = 
  match l with
  | [] -> ([], 0)
  | t::q -> let (nl,nv) = calculateSnake q in (t::nl, nv +1)

(*
  @diviseSnake
  Cut snakes if played move cut it. 
  @param ls snake snake to cut
  @param i int played ind
  @param j char played ord
  @param s int side played
  @param neighboor (int * char) list list of the neighboor from played move
  @return (snake, snake) the 2 new snakes or (oldSnake, [])
*)
let rec diviseSnake ls i j s neighboor = 
  match ls with
  | [] -> ([],[])
  | t::[] -> ([t],[])
  | t::t1::q -> try
      let (l1,l2) = diviseSnake q i j s neighboor and nb = getNthElement neighboor s in 
      if ((i,j) = t && t1 = nb) || ((i,j) = t1 && t = nb)
      then (t::l1, t1::q)
      else (t::t1::l1,l2)
    with
      | _ -> (ls, [])

(*
  @outSnakes
  Search for snake to cut
  @param i int ind
  @param j char ord 
  @param s int side
  @param cs snake list snake list to manage
  @param neighboor (int * char) list list of the neighboor from played move
  @return new list of snakes after cutting.
*)
let rec outSnakes i j s cs neighboor = 
  match cs with
  | [] -> []
  | ((snake, _) as t)::q ->
    try
      if member (getNthElement neighboor s) snake
      then
        let (l1,l2)= diviseSnake snake i j s neighboor in
          calculateSnake l1::calculateSnake l2::outSnakes i j s q neighboor
      else t::outSnakes i j s q neighboor
    with
      | _ -> t::outSnakes i j s q neighboor

(*
  @addInSnake
  Add element in Snake
  @param i int abs
  @parma j char ord
  @parame snakes list snake tlet print_bool b = print_string (string_of_bool b)

let rec printLboolDebug lbool =
  match lbool with
  | [] -> print_string " X\n"
  | t::q -> print_bool t; print_string " -> "; printLboolDebug q
o add element in
  @param neighboor list list of neighboor from (i,j)
  @param ok bool
  @return new snake and his value
*)
let rec addInSnake i j snakes neighboor ok = 
  match snakes with 
  | [] -> (calculateSnake [(i,j)], false)
  | t::[] ->
    if (member t neighboor && not ok) 
    then (calculateSnake (t::(i,j)::[]), true)
    else (calculateSnake [t], false)
  | t::q ->
    if (member t neighboor && ok) 
    then (calculateSnake ((i,j)::snakes), true)
    else if (member t neighboor && not ok) 
      then (calculateSnake(snakes), false)
      else let ((l, _), b) = addInSnake i j q neighboor false in
        (calculateSnake (t::l), b)


(*
  @addInSnakesList
  Add element in Snakes list
  @param i int abs
  @parma j char ord
  @parame snakes list snake to add element in
  @param neighboor list list of neighboor from (i,j)
  @return new snake and his value
*)
let rec addInSnakesList i j snakes nb = 
  match snakes with
  | [] -> calculateSnake[(i,j)]::snakes
  | ((snake,_) as t)::q -> let (l,b) = addInSnake i j snake nb false in
    if b
    then l::addInSnakesList i j q nb
    else t::addInSnakesList i j q nb

(*
  @updateSnakesList
  Update list of snakes after an hit
  @param i int abs
  @param j chat ord
  @param s int side
  @param snakes list list of current snakes
  @param neighboor neighboor of (i,j)
  @return updated snake list
*)
let rec updateSnakesList i j s snakes neighboor= 
  let l = addInSnakesList i j snakes neighboor in
  let (exist, os, cs) = searchElInSnake i j l in
  if exist 
  then os@outSnakes i j s cs neighboor
  else l

(*
  @isClosableSnake
  Check is snake can be closed
  @param snake list snake to close
  @param grid grid
  @retrun true if snake closable. False else.
*)
let rec isClosableSnake snake grid b =
  match snake with
  | [] -> b 
  | (i,j)::q ->let (_,lbool,_) = searchNode grid i j in
    let side = isClosable lbool in
    if side >  -1
    then 
      let (ng, _, _) = addPlay grid i j side in
      isClosableSnake q ng true
    else
      false

let rec reverseSnakeRec l nl = match l with 
  | [] -> nl
  | t::q -> reverseSnakeRec q (t::nl)

let reverseSnake l = reverseSnakeRec l []

let rec searchClosableSnake snakeList grid = 
  match snakeList with 
  | [] -> ([], 0, false)
  | (l,v)::q -> 
    if isClosableSnake l grid false
    then (l, v, true)
    else 
      if isClosableSnake (reverseSnake l) grid false
      then (reverseSnake l, v, true)
      else searchClosableSnake q grid

let rec closeSnake snake grid = 
  match snake with 
  | [] -> grid
  | (i,j)::q -> let (_, lbool, _) = searchNode grid i j in
      try 
      let (_,side) = getNthElement (getUnclosedSides lbool (i,j) 0) 0 in
      let (ng, _, _) = addPlay grid i j side in
      closeSnake q ng
    with 
      | _ -> grid

let rec searchClosedSide lb s = 
  match lb with 
  | [] -> s
  | t::q -> if t then s else searchClosedSide q (s+1)

let randomMoveSnakes l = 
  let o = Random.int((List.length(l)+30)*Random.int(10)+Random.int(50)+10) in
  (randomMoveRec l o l, o mod 4)


let rec randomMoveInSnake grid snakes rmList = 
  match grid with
  | EmptyGrid -> let ((i,j),s) = randomMoveSnakes rmList in 
    let (_,_,nb) = searchNode grid i j and (ng,_,_) = addPlay grid i j s in
      (ng, updateSnakesList i j s snakes nb, rmList)
  | GridNode(lg, ((i,j), lbool,_), rg) ->
    let (b, _, _) = searchElInSnake i j snakes in 
    if not b then 
      let (ng, nl, moves) = randomMoveInSnake lg snakes ((i,j)::rmList) in
      randomMoveInSnake rg snakes moves
    else 
      let (ng, nl, moves) = randomMoveInSnake lg snakes rmList in
      randomMoveInSnake rg snakes moves

let rec testSidePlay i j lbool ln grid snakes side testSide =
  try 
    if (getNthElement lbool testSide)
    then
      (false, false, grid, snakes)
    else
      let (ni,nj) = getNthElement ln testSide in
      if (ni = -99)
      then
        let (_,_,nb) = searchNode grid ni nj and (grid, _, _) = addPlay grid ni nj side in 
        (false, true, grid , updateSnakesList ni nj side snakes nb)
      else 
        let (_,_,nb) = searchNode grid ni nj and (grid, _, _)=addPlay grid ni nj side in
        (true, true, grid, updateSnakesList ni nj side snakes nb)
  with 
    | _ -> (false, false, grid, snakes)

let rec playIn snake grid = 
  try 
    let (l, v) = getNthElement snake 0 in
    let (i,j) = getNthElement l 0 in
    let (_,lbool,ln) = searchNode grid i j in
    let playSide = searchClosedSide lbool 0 in
    match playSide with
    | 0 -> let (pl,cs1,g1,sn1) = testSidePlay i j lbool ln grid snake 0 1 in
      if pl 
      then (g1,sn1)
      else
        let (pl,cs2,g2,sn2) = testSidePlay i j lbool ln grid snake 0 2 in
        if pl 
        then (g2,sn2)
        else 
          let (pl,cs3,g3,sn3) = testSidePlay i j lbool ln grid snake 0 3 in
          if pl 
          then (g3,sn3)
          else
            if cs1
            then (g1,sn1)
            else 
              if cs2
              then (g2,sn2)
              else (g3,sn3)
    | 2 ->  let (pl,cs1,g1,sn1) = testSidePlay i j lbool ln grid snake 2 1 in
      if pl 
      then (g1,sn1)
      else
        let (pl,cs2,g2,sn2) = testSidePlay i j lbool ln grid snake 2 0 in
        if pl 
        then (g2,sn2)
        else 
          let (pl,cs3,g3,sn3) = testSidePlay i j lbool ln grid snake 2 3 in
          if pl 
          then (g3,sn3)
          else
            if cs1
            then (g1,sn1)
            else 
              if cs2
              then (g2,sn2)
              else (g3,sn3)
    | 1 ->  let (pl,cs1,g1,sn1) = testSidePlay i j lbool ln grid snake 1 2 in
      if pl 
      then (g1,sn1)
      else
        let (pl,cs2,g2,sn2) = testSidePlay i j lbool ln grid snake 1 0 in
        if pl 
        then (g2,sn2)
        else 
          let (pl,cs3,g3,sn3) = testSidePlay i j lbool ln grid snake 1 3 in
          if pl 
          then (g3,sn3)
          else
            if cs1
            then (g1,sn1)
            else 
              if cs2
              then (g2,sn2)
              else (g3,sn3)
    | 3 ->  let (pl,cs1,g1,sn1) = testSidePlay i j lbool ln grid snake 3 1 in
      if pl 
      then (g1,sn1)
      else
        let (pl,cs2,g2,sn2) = testSidePlay i j lbool ln grid snake 3 0 in
        if pl 
        then (g2,sn2)
        else 
          let (pl,cs3,g3,sn3) = testSidePlay i j lbool ln grid snake 3 2 in
          if pl 
          then (g3,sn3)
          else
            if cs1
            then (g1,sn1)
            else 
              if cs2
              then (g2,sn2)
              else (g3,sn3)
    | _ -> (grid,snake)
  with
    | _ -> (grid,snake)
  
let rec play1 grid snakes score =
  let (snake, v, ok) = searchClosableSnake snakes grid in
  if ok 
  then
    play1 (closeSnake snake grid) snakes (score + v)
  else 
    match snakes with
    | [] -> let (g, s, _) = randomMoveInSnake grid snakes [] in (g,s, score)
    | t::_ -> let (g,s) = playIn snakes grid in (g,s,score)
  