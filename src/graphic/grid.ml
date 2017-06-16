type gridNode = (int * char) * bool list * (int * char) list;;
type 'a grid = EmptyGrid | GridNode of 'a grid * 'a * 'a grid;;
(*
	@newEmptyGrid
	Function to init new grid
	@renturn empty grid
*)
let newEmptyGrid () = EmptyGrid;;

let coi i m = Char.chr (i+97+m)

(*
    @getHeight
    Get tree heigh
    @param tree myTree tree we want to get hight from.
    @return tree height
 *)
 let rec getHeight tree =
    match tree with
    | EmptyGrid -> 0
    | GridNode(fg,_,fd) -> max (getHeight fg) (getHeight fd) + 1;;

(*
    @rightRotate
    Swap tree element to right
    @param tree myTree tree to rotate
    @return tree rotated
 *)
let rec rightRotate tree =
    match tree with
    | GridNode(GridNode(EmptyGrid, a, EmptyGrid),r,EmptyGrid) -> GridNode(EmptyGrid, a,GridNode(EmptyGrid, r, EmptyGrid))
    | GridNode(
        GridNode(fgg, rg, fgd),
        r, fd
      ) -> GridNode(fgg, rg, GridNode(fgd, r, fd))
    | _ -> tree;;

(*
    @leftRotate
    Swap tree element to left
    @param tree myTree tree to rotate
    @return tree rotated
 *)
let rec leftRotate tree =
    match tree with
    | GridNode(EmptyGrid,r,GridNode(EmptyGrid, a, EmptyGrid)) -> GridNode(GridNode(EmptyGrid, r, EmptyGrid), a, EmptyGrid)
    | GridNode(
        fg, r,
        GridNode(fdg, rd, fdd)
      ) -> GridNode(GridNode(fg, r, fdg), rd, fdd)
    | _ -> tree;;

(*
    @balance
    Balance a tree to avoid having a big useless tree
    @param tree myTree tree to balance
    @return tree balanced
 *)
let rec balance tree =
    match tree with
    | GridNode (fg, _, fd) ->
        begin
            let h1 = getHeight (balance fg) and h2 = getHeight (balance fd) in
            let diff = h1 - h2 in
            if (diff > 1)
            then rightRotate tree
            else
                if (diff < -1)
                then leftRotate tree
                else tree
        end
    | _ -> tree;;

(*
    @add
    Simple function to add an element into a myTree tree.
    @param i int element to add
    @param tree myTree tree in whom we which to add the element
    @return new tree with i added into old tree
*)
let rec add i tree =
    match tree with
    | EmptyGrid -> GridNode(EmptyGrid, i, EmptyGrid)
    | GridNode(EmptyGrid, a, EmptyGrid) -> if a < i then GridNode (EmptyGrid, a, GridNode(EmptyGrid, i, EmptyGrid)) else GridNode (GridNode(EmptyGrid, i, EmptyGrid), a, EmptyGrid)
    | GridNode(fg, r, fd) -> if i < r then balance (GridNode (add i fg, r, fd)) else balance (GridNode(fg, r, add i fd));;

(*
    @addTreeInTree
    Function to add a correct tree into a correct tree
    @param tree1 myTree tree to add
    @param tree2 myTree tree where to add
    @return new tree
 *)
let rec addTreeInTree tree1 tree2 =
    match tree1 with
    | EmptyGrid-> tree2
    | GridNode(EmptyGrid, e, EmptyGrid) -> add e tree2
    | (GridNode (fg, r, fd)) ->
        begin
            let newTree = add r tree2 in
            let newTree = addTreeInTree fg newTree in
            addTreeInTree fd newTree
        end;;

(*
	@searchNode
	Search for a Node in the grid
	@param grid (gridNode grid) grid where to find the element
	@param i int position in column
	@param j char position in row
	@return found node or empty list
*)
let rec searchNode grid i j =
	match grid with
	| EmptyGrid -> ((-99,'a'),[],[])
	| GridNode(lg, (((gi, gj), _, _) as r), rg) ->
		if (gi,gj) = (i,j)
		then r
		else
			if (gi, gj) < (i,j)
			then searchNode rg i j
			else searchNode lg i j;;

(*
	@updateNode
	Update value in a Node
	@param grid (gridNode grid) grid to update
	@param i int position of the node
	@param j char position of the node
	@param newNode gridNode new value for node
*)
let rec updateNode grid i j newNode =
	match grid with
	| EmptyGrid -> EmptyGrid
	| GridNode(lg, (((gi, gj), _, _) as r), rg) ->
		if (gi,gj) = (i,j)
		then addTreeInTree (add newNode lg) rg
		else
			if (gi, gj) < (i,j)
			then addTreeInTree (GridNode(lg,r,EmptyGrid)) (updateNode rg i j newNode)
			else addTreeInTree (GridNode(EmptyGrid,r, rg)) (updateNode lg i j newNode);;

exception NotEnougthElementInList;;

let rec getNthElement l n =
	match (l, n) with
	| (t::q, 0) -> t
	| (t::q, n) -> getNthElement q (n-1)
	| _ -> (raise (NotEnougthElementInList));;

let rec updateNthElement l n e =
	match (l,n) with
	| ([], _) -> []
	| ((t::q),0) -> e::q
	| (t::q, _) -> t::updateNthElement q (n-1) e;;


let rec addPlay grid i j side =
	let ((_,_), lbool, lnode) = searchNode grid i j in
	let (ni,nj) = getNthElement lnode side and played = getNthElement lbool side in
	if played
	then
		grid
	else
		let newLBoll = updateNthElement lbool side true in
		let newGrid = updateNode grid i j ((i,j), newLBoll, lnode) in
		if ni = -99
		then
			newGrid
		else
			addPlay newGrid ni nj ((side+2) mod 4)
			;;


(*
   @initGridRec
   Create a representation for the game grid.
   @param i int evolutive paramater to count the number of column to initialise
   @param j int evolutive paramater to count the number of row to initialise
   @param mi int max value of i
   @param mj int max value of j

*)
let rec initGridRec i j mi mj =
	match (i,j) with
	| (0, 0) -> GridNode(EmptyGrid, ((0, 'a'), [false; false; false; false], [(-99,' ');(0, 'b');(1, 'a');(-99,' ')]), EmptyGrid)
	| (0, n) -> if n = mj
		then add ((0, coi n 0), [false; false; false; false], [(-99,' ');(-99,' ');(1, coi n 0);(0,coi n (-1))]) (initGridRec 0 (n-1) mi mj)
		else add ((0, coi n 0), [false; false; false; false], [(-99,' ');(0, coi n 1); (1, coi n 0); (0, coi n (-1))]) (initGridRec 0 (n-1) mi mj)
	| (n, 0) -> if n = mi
		then add ((n, 'a'), [false; false; false; false], [(n-1, 'a'); (n, 'b'); (-99,' '); (-99,' ')]) (initGridRec (n-1) mj mi mj)
		else add ((n, 'a'), [false; false; false; false], [(n-1, 'a'); (n, 'b'); (n+1, 'a');(-99,' ')]) (initGridRec (n-1) mj mi mj)
	| (i,j) ->
	if j = mj
		then if i = mi
			then add ((i, coi j 0), [false; false; false; false], [(i-1, coi j 0);(-99,' ');(-99,' ');(i, coi j (-1))]) (initGridRec i (j-1) mi mj)
		else  add ((i, coi j 0), [false; false; false; false], [(i-1, coi j 0);(-99,' ');(i+1, coi j 0);(i, coi j (-1))]) (initGridRec i (j-1) mi mj)
	else
		if i = mi
			then add ((i, coi j 0), [false; false; false; false], [(i-1, coi j 0); (i, coi j 1); (-99,' '); (i, coi j (-1))]) (initGridRec i (j-1) mi mj)
		else add ((i, coi j 0), [false; false; false; false], [(i-1, coi j 0);(i, coi j 1);(i+1, coi j 0); (i, coi j (-1))]) (initGridRec i (j-1) mi mj);;

(*
 	@initGrid
 	Cleaner way to init the grid.
 	@param i int number of row to init
 	@param j int number of column to init
 *)
let rec initGrid i j = let i = i - 1 and j = j - 1 in initGridRec j i j i;;

(* Loading graphics library to used in REPL *)
(*#load "graphics.cma";;*)
(* Opening graphics module to avoid having to call Graphics.method*)
open Graphics;;

(*
    @openGraph
    Create a new graphic window using provided size.
    @param w string width of the window in pixels
    @param h string heigth of the window in pixels
    @ensures a new graphic window is open
*)
let openGraph w h = let st = " " ^ w ^ "x" ^ h in open_graph st; set_window_title "Square Game";;

let resetGraph () = clear_graph ();;

(*
    @treeDrawing
    Draw a tree in order
    @param t alphaTree tree to draw
    @param x string initial width position in px
    @param y string initial height position in px
    @param h string height of step in px
    @param w string width of step in px
    @param printer 'a -> unit function to print element
    @param textColor color text color
    @param lineColor color line color
    @effect Draw correctly the tree.
*)
let rec treeDrawing t x y h w printer textColor lineColor=
  let drawingZoneWidth = w / 2 and drawingZoneHeight = h / 2 in
  match t with
  | GridNode(EmptyGrid, l, EmptyGrid) -> moveto (x + drawingZoneWidth) y; set_color textColor; printer l; set_color black;
  | GridNode(fg,r,fd) -> begin
                    moveto (x + drawingZoneWidth) (y + 12);
                    set_color lineColor; lineto (x + drawingZoneWidth / 2) (y + h - 5);
                    moveto (x + drawingZoneWidth) (y + 12); lineto (x+ drawingZoneWidth + drawingZoneWidth / 2) (y + h - 5);
                    set_color textColor; moveto (x + drawingZoneWidth) y; printer r; set_color black;
                    treeDrawing fg x (y+h) drawingZoneHeight drawingZoneWidth printer textColor lineColor;
                    treeDrawing fd (x+drawingZoneWidth) (y+h) drawingZoneHeight drawingZoneWidth printer textColor lineColor;
                  end
  | _ -> moveto (x + drawingZoneWidth) y; set_color textColor; draw_string "X"; set_color black;;

let magicDrawing t printer stepHight drawingZoneWidth =
resetGraph();
treeDrawing t 0 0 stepHight drawingZoneWidth printer red blue;;

let soi i = string_of_int i;;

let drawInt i = draw_string (soi i);;

let rec drawBoolList lbool =
	match lbool with
	| t::q -> draw_string (string_of_bool t); draw_string ", "; drawBoolList q;
	| _ -> ();;

let rec drawNodeList lnode =
	match lnode with
	| t::q -> let (i,j) = t in draw_string "( "; drawInt i; draw_string ", "; draw_char j; draw_string " )"; drawNodeList q;
	| _ -> ();;

let printNode node =
	let ((i,j), lbool, lnode) = node in
	draw_string "( ";
	drawInt i; draw_string ", "; draw_char j; draw_string " ), (";
	drawBoolList lbool; draw_string " ), "; drawNodeList lnode;;