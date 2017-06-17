(* Type declarations *)
type gridNode = (int * char) * bool list * (int * char) list
type 'a grid = EmptyGrid | GridNode of 'a grid * 'a * 'a grid
(* Untyped grid function. Usable for any balanced binary tree *)
val newEmptyGrid : unit -> 'a grid
val getHeight : 'a grid -> int
val rightRotate : 'a grid -> 'a grid
val leftRotate : 'a grid -> 'a grid
val balance : 'a grid -> 'a grid
val add : 'a -> 'a grid -> 'a grid
val addTreeInTree : 'a grid -> 'a grid -> 'a grid
(* Grid specific functions *)
val initGridRec : int -> int -> int -> int -> gridNode grid
val initGrid : int -> int -> gridNode grid
val searchNode : gridNode grid -> int -> char -> gridNode
val updateNode : gridNode grid -> int -> char -> gridNode -> gridNode grid
val addPlay : gridNode grid -> int -> char -> int -> gridNode grid * bool
(* List general functions *)
val getNthElement : 'a list -> int -> 'a
val updateNthElement : 'a list -> int -> 'a -> 'a list
(* Graphics functions *)
val openGraph : string -> string -> unit
val resetGraph : unit -> unit
val magicDrawing : 'a grid -> ('a -> 'b) -> int -> int -> unit
val soi : int -> string
val drawInt : int -> unit
val drawBoolList : bool list -> unit
val drawNodeList : (int * char) list -> unit
val printNode : gridNode -> unit