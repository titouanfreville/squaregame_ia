open Grid;;
open Base;;
open Ia1;;
open Ia2;;
(* Usefull functions (conversion, reading, printing ...) *)
let readChar () = 
  let r = read_line () in
  r.[0]

let getVal valName valType getter= 
  print_string ("Enter "^valName^": "^valType^" <- ");
  getter ()

let printCurrentGrid grid =
  magicDrawing grid printNode 200 1000

let printCurrentScore p1 p2 s1 s2 = 
  print_string "<><><><><><><><><><><><><><><><><><><><><><><><><><>\n";
  print_string "<><><><><><><><><><><> SCORE! <><><><><><><><><><><>\n";
  print_string ("<><> Player "^p1^": "); print_int s1; print_string ".\n";
  print_string ("<><> Player "^p2^": "); print_int s2; print_string ".\n";
  print_string "<><><><><><><><><><><><><><><><><><><><><><><><><><>\n"

let printWinner p s = 
  print_string ("\nWELL PLAY "^p^" !!!!!\nYou won this game with ");
  print_int s; print_string (" points.\n")

let printResult p1 p2 s1 s2 = 
  if s1 > s2
  then
    printWinner p1 s1
  else
    if s1 < s2
    then
      printWinner p2 s2
    else
      print_string ("\nIT'S A TIE !!!!!\nWell play both: "^p1^" and "^p2^".\n")

let rec processPlay grid i j side score sl tree upTree player f =
  if allClosed grid 
  then (grid, score, sl, tree)
  else
    if upTree
    then
      let (_,_,nb) = searchNode grid i j in
      let (g, ok, replay) = addPlay grid i j side and nsl = updateSnakesList i j side sl nb 
      and nt = processMove tree (i,j,side) in
      if ok 
      then
        begin
          if replay
          then
            if allClosed g
            then (g, score+1, nsl, nt)
            else f g (score+1) (nsl) nt player
          else (g, score, nsl, nt)
        end
      else 
        begin
          print_string ("The move you enter can't be proced. Ever it was already play, ever it is outside the  Please try again !\n");
          (*f grid score sl tree player*) exit 0
        end
    else 
      let (_,_,nb) = searchNode grid i j in
      let (g, ok, replay) = addPlay grid i j side and nsl = updateSnakesList i j side sl nb in
      if ok 
      then
        begin
          if replay
          then
            if allClosed g
            then (g, score+1, nsl, tree)
            else f g (score+1) (nsl) tree player
          else (g, score, nsl, tree)
        end
      else 
        begin
          print_string ("The move you enter can't be proced. Ever it was already play, ever it is outside the  Please try again !\n");
          (*f grid score sl tree player*) exit 0
        end

(* Ia calls *)
let rec playIa0 grid score snakeList tree player = 
  let ((i,j), s) = Base.randomMove (play0 grid []) in
  processPlay grid i j s score snakeList tree true 0 playIa0

let playIa1 grid score snake = 
  let (g,s,sc) = Ia1.play1 grid snake score in
  (g,sc, s, E)

let rec playIa2 grid score snakeList tree player = 
  let ((i,j,s),t) = play2 tree player in 
  print_int i; print_string " "; print_string (Char.escaped j); print_string " "; print_int s; print_string "\n";
  processPlay grid i j s score snakeList t false player playIa2
(* Player plays *)

let rec move grid score snakeList tree player =
  let i = getVal "x" "int" read_int and j = getVal "y" "char" readChar and side = getVal "side" "int in [0,1,2,3]" read_int in
  processPlay grid i j side score snakeList tree true 0 move

let reset () =
  print_string ("Enter the size of the grid you wish (x:int, y:int):\n");
  let x = getVal "x" "int" read_int and y = getVal "y" "int" read_int in
  let grid = initGrid x y in 
  resetGraph();
  printCurrentGrid grid;
  (grid, -2, [], E)

let endGame name =
  print_string ("player "^name^" leaved the game.\n");
  (EmptyGrid, -1, [], E)

(* 
  @waitPlayer 
  Function to let human player make their moves
  @param name string player nickname
  @param grid (gridNode grid) game grid
  @return (gridNode grid) new grid according to user play or choice
*)
let rec waitPlayer name grid score snake tree npl = 
  print_string ("Playing: "^name^"\n");
  print_string ("What do you want to do ?\nPlay a Move (play)\nReset the game (reset)\nLeave the game (leave):\n");
  let userinput = read_line() in
  match userinput with 
  | "play" -> move grid score snake tree npl
  | "reset" -> reset ()
  | "leave" -> endGame name
  | _ -> waitPlayer name grid score snake tree npl

(* Player creation*)
(*
  @player
  Define a new player
  @param name string user nickname
  @param kind string type of player (ia type (0,1,2), human (h) -- Default is ia2)
  @param grid (gridNode grid) 
  @return grid 
*)
let player name kind score grid snakeList tree npl =
  match kind with  
  | "human" | "h" -> waitPlayer name grid score snakeList tree npl
  | "ia0" | "0" -> playIa0 grid score snakeList tree npl
  | "ia1" | "1" -> playIa1 grid score snakeList
  | _ -> playIa2 grid score snakeList tree npl

let rec game grid p1 p2 t1 t2 s1 s2 snakeList tree =
    if allClosed grid
    then (printResult p1 p2 s1 s2; exit 0;);
    let (ngrid, ns1, nsl1, nt1) = player p1 t1 s1 grid snakeList tree 1 in
    let nsl1 = quickSort nsl1 snakeSort in 
    if ns1 = -2
    then game ngrid p1 p2 t1 t2 0 0 [] E 
    else
      match ngrid  with
      | EmptyGrid -> exit 0
      | _ -> printCurrentGrid ngrid; printCurrentScore p1 p2 ns1 s2;
      if allClosed ngrid
      then (printResult p1 p2 ns1 s2; exit 0;);
      let (ngrid, ns2, nsl2, nt2) = player p2 t2 s2 ngrid nsl1 nt1 (-1) in
      let nsl2 = quickSort nsl2 snakeSort in 
      if ns2 = -2
      then game ngrid p1 p2 t1 t2 0 0 [] E
      else
        match ngrid  with
        | EmptyGrid -> exit 0
        | _ -> printCurrentGrid ngrid; printCurrentScore p1 p2 ns1 ns2; game ngrid p1 p2 t1 t2 ns1 ns2 nsl2 nt2;;

(* Main block *)
let main () =
  openGraph "1900" "800";
  Random.self_init ();
  let (grid, s, sl, tree) = reset ()
  and n1 = getVal "player 1" "string" read_line
  and t1 = getVal "type of Player 1" "string in [h,0,1,2]" read_line
  and n2 = getVal "player 2" "string" read_line 
  and t2 = getVal "type of Player 2" "string in [h,0,1,2]" read_line  in
  print_string (t1^" "^t2^"\n");
  if (t1=="2" || t2 =="2")
  then let tree = initIA2 grid in game grid n1 n2 t1 t2 0 0 [] tree
  else game grid n1 n2 t1 t2 0 0 [] E;;

(* Running main *)
main ()