open Grid;;
open Base;;
open Ia1;;

type 'a moveTree = E | L of 'a | N of 'a * ('a moveTree) list;;

let newEmptyTree () = E;;

let rec addMoveList lMove move mult score = 
	match lMove with
	| [] -> [(L (move,score))]
	| t::q -> t::addMoveList q move mult score

let treeSort t1 t2 = 
	match (t1,t2) with 
	| (E, _) | (_, E) -> false 
	| (L (_,v1), L (_, v2)) | (L (_,v1), N((_,v2), _)) | (N((_,v1), _), L (_,v2)) -> v1 > v2
	| (N((_,v1), _), N((_,v2), _)) -> v1 > v2
(*
	@addMove
	Add a move in the tree
	@param tree ('a moveTree) tree to add move in
	@param move (int*char*int) to represent the move (ind, ord, side)
	@param movePosition int move number in the game flow
	@param mult (int) value equal to -1 or 1 depend on the player turn
	@return new tree with move inside it
*)
let rec addMove tree move movePosition mult score =
	match (tree, movePosition) with
	| (E, 0) -> L (move,score)
	| (L (mv, v), 0) -> 
		if mv == move 
		then tree
		else N ((mv,v), [L (move,score)])
	| (L (mv, v), _) -> tree
	| (N ((m,v), lMove), 0) ->
		if m == move
		then tree
		else let nLMove = addMoveList lMove move mult score in let nLMove = quickSort nLMove treeSort in N((m,v), nLMove)
	| (N ((m,v), lMove), n) -> 
		if m == move 
		then tree
		else let nLMove = manageMove lMove move (n-1) (mult*1) score in let nLMove = quickSort nLMove treeSort in N((m,v), nLMove)
and manageMove lMove move n mult score =
	match lMove with
	| [] -> []
	| t::q -> (addMove t move n mult score)::(manageMove q move n mult score)

let rec getLast l = 
	match l with 
	| t::[] -> t 
	| t::q -> getLast q

let getFirst l = 
	match l with
	| t::_ -> t

let getMoveToPlay tree inv = 
	match tree with
	| E | L (_) ->  E
	| N (_, lm) -> if inv then getLast lm else getFirst lm

let rec findMoveInList move lMove = 
	match lMove with 
	| [] -> E
	| t::q -> match t with
		| E -> findMoveInList move q 
		| L (m,_) | N ((m,_), _) -> if m == move then t else findMoveInList move q

let rec processMove tree move = 
	match tree with 
	| E | L (_) -> E
	| N (_, lm) -> findMoveInList move lm

let rec createMove lbool i j s grid tree player moveRank branchScore =
	match lbool with 
	| [] -> (grid, tree, player * -1, branchScore)
	| t::q ->
		if t
		then 
			let (ng,_,replay) = addPlay grid i j s and nt = addMove tree (i,j,s) moveRank player branchScore in 
			if replay
			then (ng, nt, player, branchScore + player)
			else createMove q i j (s+1) ng nt player moveRank branchScore
		else createMove q i j (s+1) grid tree player moveRank branchScore

let rec initTreeMove grid tree moveRank player branchScore = 
	match grid with
	| EmptyGrid -> tree
	| GridNode(rg, ((i,j),lbool,_), lg) -> 
		let (ng, nt, np, ns) = createMove lbool i j 0 grid tree player moveRank branchScore in
		let nt = initTreeMove ng nt (moveRank+1) (player*(-1)) ns in 
		let nt = initTreeMove rg nt moveRank player branchScore in 
		initTreeMove lg nt moveRank player branchScore

let initIA2 grid = initTreeMove grid E 0 1 0

let playIA2 tree player = let nt = getMoveToPlay tree (player == 1) in 
	match nt with 
	| E | L (_) -> ((-99,' ', -1), E)
	| N((m, _)) -> (m, nt)   