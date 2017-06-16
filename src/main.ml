open Grid;;
(*open Base;;*)
(* Usefull functions (conversion, reading, printing ...) *)
let readChar () = 
  let r = read_line () in
  r.[0]

let printCurrentGrid grid =
  Grid.magicDrawing grid Grid.printNode 200 1000

(* Ia calls *)
let playNoob grid = grid;;

let playIa0 = playNoob;;

let playIa1 = playNoob;;

let playIa2 = playNoob;;
(* Player plays *)

let rec move grid =
  print_string ("Enter your move (x: int, y: char, side: int): \n");
  let i = read_int () and j = readChar () and side = read_int () in
  Grid.addPlay grid i j side

let reset () =
  print_string ("Enter the size of the grid you wish (x:int, y:int):\n");
  let x = read_int () and y = read_int () in
  let grid = initGrid x y in 
  Grid.resetGraph();
  printCurrentGrid grid;
  grid

let endGame () = 
  Grid.EmptyGrid

(* 
  @waitPlayer 
  Function to let human player make their moves
  @param name string player nickname
  @param grid (gridNode grid) game grid
  @return (gridNode grid) new grid according to user play or choice
*)
let rec waitPlayer name grid = 
  print_string ("Playing: "^name);
  print_string ("What do you want to do ?\nPlay a Move (play)\nReset the game (reset)\nLeave the game (leave):\n");
  let userinput = read_line() in
  match userinput with 
  | "play" -> move grid
  | "reset" -> reset ()
  | "leave" -> endGame ()
  | _ -> waitPlayer name grid

(* Player creation*)
(*
  @player
  Define a new player
  @param name string user nickname
  @param kind string type of player (ia type (0,1,2), human (h) -- Default is ia2)
  @param grid (gridNode grid) 
  @return grid 
*)
let player name kind grid =
  match kind with  
  | "human" | "h" -> waitPlayer name grid
  | "ia0" | "0" -> playIa0 grid
  | "ia1" | "1" -> playIa1 grid
  | _ -> playIa2 grid

let rec game grid p1 p2 t1 t2 =
    let ngrid = player p1 t1 grid in
    match ngrid  with
    | Grid.EmptyGrid -> exit 0
    | _ -> printCurrentGrid ngrid;
    let ngrid = player p2 t2 ngrid in
    match ngrid  with
    | Grid.EmptyGrid -> exit 0
    | _ -> printCurrentGrid ngrid; game ngrid p1 p2 t1 t2;;


(* Main block *)
let main () =
  Grid.openGraph "1900" "800";
  let n1 = "me" and n2 = "maya" in
  let grid = reset () in
  game grid n1 n2 "h" "0";;

(* Running main *)
main ()