open Grid;;

let rec randomMoveRec l n il = 
  match (l,n) with
  | ([], n) -> randomMoveRec il (n-1) il
  | (t::_, 0) | (t::[], 1) -> t
  | (_::q, n) -> randomMoveRec q (n-1) il

let randomMove l =
  let o = Random.int(List.length(l)) in
  randomMoveRec l o l

let rec isClosableRec lbool c side = 
  match lbool with
  | [] -> side
  | t::[] -> 
    if t && c 
    then -1 
    else 
      if (t||c) 
      then side
      else -1
  | t::q ->
    if t && c
    then
      isClosableRec q t (side+1)
    else
      if (c || t) 
      then isClosableRec q (t&&c) side
      else -1

let isClosable l = isClosableRec l true 0

let rec getUnclosedSides l pos s = 
  match l with
  | [] -> []
  | t::q -> if t then getUnclosedSides q pos (s+1) else (pos,s)::getUnclosedSides q pos (s+1)

let print_bool b = print_string (string_of_bool b)

let rec printLboolDebug lbool =
  match lbool with
  | [] -> print_string " X\n"
  | t::q -> print_bool t; print_string " -> "; printLboolDebug q

let rec play0 grid randomMoveList = 
  match grid with
  | EmptyGrid -> randomMoveList
  | GridNode(lg, (pos, lbool,_), rg) ->
    let cside = isClosable lbool in 
    if (cside > -1)
    then [(pos, cside)]
    else
      let lsides = getUnclosedSides lbool pos 0 in
      let recMove = play0 lg (randomMoveList@lsides) in
      if List.length(recMove) = 1
      then recMove
      else play0 rg recMove