val randomMoveRec: 'a list -> int -> 'a list -> 'a
val randomMove: 'a list -> 'a
val isClosableRec: bool list -> bool -> int -> int
val isClosable: bool list -> int
val getUnclosedSides: bool list -> (int*char) -> int -> ((int*char)*int) list
val play0: ((int*char) * bool list * 'b) Grid.grid -> ((int*char)*int) list -> ((int*char)*int) list