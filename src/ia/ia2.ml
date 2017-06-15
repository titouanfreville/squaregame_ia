#use "../graphic/grid.ml";;

type 'a moveTree = E | L of 'a | N of 'a * ('a moveTree) list;;

let newEmptyTree () = E;;

let rec addMove tree move movePosition =
	match tree with