val member: 'a -> 'a list -> bool
val searchElInSnakeRec: 'a -> 'b -> (('a * 'b) list * 'c) list ->
                        bool -> (('a * 'b) list * 'c) list -> (('a * 'b) list * 'c) list
                        -> bool * (('a * 'b) list * 'c) list * (('a * 'b) list * 'c) list

val searchElInSnake: 'a -> 'b -> (('a * 'b) list * 'c) list
                      -> bool * (('a * 'b) list * 'c) list  * (('a * 'b) list * 'c) list 

val calculateSnake: 'a list -> 'a list * int

val diviseSnake: ('a * 'b) list -> 'a -> 'b ->
                 int -> ('a * 'b) list -> (('a * 'b) list * ('a * 'b) list)

val outSnakes: int -> char -> int -> ((int * char) list * int) list ->
               (int * char) list -> ((int * char) list * int) list

val addInSnake: int -> char -> (int * char) list -> 
                (int * char) list -> bool -> ((int * char) list * int) * bool

val addInSnakesList: int -> char ->  ((int * char) list * int) list -> 
                     (int * char) list -> ((int * char) list * int) list

val updateSnakesList: int -> char -> int -> ((int * char) list * int) list
                      -> (int * char ) list -> ((int * char) list * int) list

val isClosableSnake: (int * char) list -> Grid.gridNode Grid.grid -> bool -> bool

val play1: Grid.gridNode Grid.grid -> ((int * char) list * int) list -> int ->
           Grid.gridNode Grid.grid * ((int * char) list * int) list * int

val quickSort: 'a list -> ('a -> 'a -> bool) -> 'a list

val snakeSort: 'a * 'b -> 'a * 'b -> bool