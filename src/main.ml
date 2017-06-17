open Grid;;
(*open Base;;*)
(* Usefull functions (conversion, reading, printing ...) *)
let readChar () = 
  let r = read_line () in
  r.[0]

let getVal valName valType getter= 
  print_string ("Enter "^valName^": "^valType^" <- ");
  getter ()

let printCurrentGrid grid =
  Grid.magicDrawing grid Grid.printNode 200 1000

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
  if s1 = s2
  then print_string ("\nIT'S A TIE !!!!!\nWell play both: "^p1^" and "^p2^".\n")
  else
    if s1 > s2
    then
      printWinner p1 s1
    else
      printWinner p2 s2

(* Ia calls *)
let playNoob grid = grid;;

let playIa0 = playNoob;;

let playIa1 = playNoob;;

let playIa2 = playNoob;;
(* Player plays *)

let rec move grid score =
  print_string ("Enter your move (x: int, y: char, side: int): \n");
  let i = getVal "x" "int" read_int and j = getVal "y" "char" readChar and side = getVal "side" "int in [0,1,2,3]" read_int in
  let (g, ok, replay) = Grid.addPlay grid i j side in
  if ok 
  then
    begin
      if replay
      then
        if allClosed g
        then (g, score+1)
        else move g (score+1)
      else (g, score)
    end
  else 
    begin
      print_string ("The move you enter can't be proced. Ever it was already play, ever it is outside the grid. Please try again !\n");
      move grid score
    end

let reset () =
  print_string ("Enter the size of the grid you wish (x:int, y:int):\n");
  let x = getVal "x" "int" read_int and y = getVal "y" "int" read_int in
  let grid = initGrid x y in 
  Grid.resetGraph();
  printCurrentGrid grid;
  (grid, -2)

let endGame name =
  print_string ("player "^name^" leaved the game.\n");
  (Grid.EmptyGrid, -1)

(* 
  @waitPlayer 
  Function to let human player make their moves
  @param name string player nickname
  @param grid (gridNode grid) game grid
  @return (gridNode grid) new grid according to user play or choice
*)
let rec waitPlayer name grid score = 
  print_string ("Playing: "^name^"\n");
  print_string ("What do you want to do ?\nPlay a Move (play)\nReset the game (reset)\nLeave the game (leave):\n");
  let userinput = read_line() in
  match userinput with 
  | "play" -> move grid score
  | "reset" -> reset ()
  | "leave" -> endGame name
  | _ -> waitPlayer name grid score

(* Player creation*)
(*
  @player
  Define a new player
  @param name string user nickname
  @param kind string type of player (ia type (0,1,2), human (h) -- Default is ia2)
  @param grid (gridNode grid) 
  @return grid 
*)
let player name kind score grid =
  match kind with  
  | "human" | "h" -> waitPlayer name grid score
  | "ia0" | "0" -> (playIa0 grid, score)
  | "ia1" | "1" -> (playIa1 grid, score)
  | _ -> (playIa2 grid, score)

let rec game grid p1 p2 t1 t2 s1 s2 =
    if allClosed grid
    then (printResult p1 p2 s1 s2; exit 0;);
    let (ngrid, ns1) = player p1 t1 s1 grid in
    if ns1 = -2
    then game ngrid p1 p2 t1 t2 0 0
    else
      match ngrid  with
      | Grid.EmptyGrid -> exit 0
      | _ -> printCurrentGrid ngrid; printCurrentScore p1 p2 ns1 s2;
      if allClosed ngrid
      then (printResult p1 p2 s1 s2; exit 0;);
      let (ngrid, ns2) = player p2 t2 s2 ngrid in
      if ns2 = -2
      then game ngrid p1 p2 t1 t2 0 0
      else
        match ngrid  with
        | Grid.EmptyGrid -> exit 0
        | _ -> printCurrentGrid ngrid; printCurrentScore p1 p2 ns1 ns2; game ngrid p1 p2 t1 t2 ns1 ns2;;


(* Main block *)
let main () =
  Grid.openGraph "1900" "800";

  let (grid, s) = reset ()
  and n1 = getVal "player 1" "string" read_line
  and t1 = getVal "type of Player 1" "string in [h,0,1,2]" read_line
  and n2 = getVal "player 2" "string" read_line 
  and t2 = getVal "type of Player 2" "string in [h,0,1,2]" read_line  in

  game grid n1 n2 t1 t2 0 0;;

(* Running main *)
main ()