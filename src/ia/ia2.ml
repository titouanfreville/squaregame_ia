#use "../graphic/grid.ml";;

type 'a moveTree = E | L of 'a | N of 'a * ('a moveTree) list;;
