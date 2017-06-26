type 'a moveTree = E | L of 'a | N of 'a * ('a moveTree) list

val addMove: ('a*int) moveTree -> 'a -> int -> int -> int -> ('a*int) moveTree 
val addMoveList: ('a*int) moveTree list -> 'a -> int -> int -> ('a*int) moveTree list
val manageMove: ('a*int) moveTree list -> 'a -> int -> int -> int -> ('a*int) moveTree list
val getMoveToPlay: 'a moveTree -> bool -> 'a moveTree
val processMove: ('a*int) moveTree -> 'a -> ('a*int) moveTree 
val initIA2: Grid.gridNode Grid.grid -> ((int * char * int) * int) moveTree
val playIA2: (int * char * int) moveTree ->
           int -> (int * char * int) * (int * char * int) moveTree
