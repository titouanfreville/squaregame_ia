type gridNode = (int * char) * bool list * (int * char) list;;
type 'a grid = EmptyGrid | GridNode of 'a grid * 'a * 'a grid;;

let newEmptyGrid () = EmptyGrid;;

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
   @initGridRec
   Create a representation for the game grid.
   @param i int evolutive paramater to count the number of column to initialise
   @param j int evolutive paramater to count the number of row to initialise
   @param mi int max value of i
   @param mj int max value of j

*)
let rec initGridRec i j mi mj =
	match (i,j) with
	| (0, 0) -> GridNode(EmptyGrid, ((0, 'a'), false :: false ::[], (1, 'a')::(0, 'b')::[]), EmptyGrid)
	| (0, n) -> if n == mj
		then add ((0, Char.chr (n+97)), false :: false :: [], (0,Char.chr (n+96))::(1, Char.chr (n+97))::[]) (initGridRec 0 (n-1) mi mj)
		else add ((0, Char.chr (n+97)), false :: false :: false :: [], (0, Char.chr (n+96))::(0, Char.chr (n+98))::(1, Char.chr (n+97))::[]) (initGridRec 0 (n-1) mi mj)
	| (n, 0) -> if n = mi
		then add ((n, 'a'), false :: false :: [], (n, 'b') :: (n-1, 'a') :: []) (initGridRec (n-1) mj mi mj)
		else add ((n, 'a'), false :: false :: false :: [], (n, 'b') :: (n-1, 'a') :: (n+1, 'a') :: []) (initGridRec (n-1) mj mi mj)
	| (i,j) -> add ((i, Char.chr (j+97)), false :: false :: false :: [], (i, Char.chr (j+96)) :: (i, Char.chr (j+98)) :: (i+1, Char.chr (j+97)) :: (i-1, Char.chr (j+97)) :: []) (initGridRec i (j-1) mi mj);;

(*
 	@initGrid
 	Cleaner way to init the grid.
 	@param i int number of row to init
 	@param j int number of column to init
 *)
let rec initGrid i j = initGridRec j i j i;;

(* Loading graphics library to used in REPL *)
#load "graphics.cma";;
(* Opening graphics module to avoid having to call Graphics.method*)
open Graphics;;

(*
    @openGraph
    Create a new graphic window using provided size.
    @param w string width of the window in pixels
    @param h string heigth of the window in pixels
    @ensures a new graphic window is open
*)
let openGraph w h = let st = " " ^ w ^ "x" ^ h in open_graph st;;

let resetGraph = clear_graph;;

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

let magicDrawing t printer =
resetGraph();
treeDrawing t 0 0 450 1800 printer red blue;;

let soi i = string_of_int i;;

let drawInt i = draw_string (soi i);;

let rec drawBoolList lbool =
	match lbool with
	| t::q -> draw_string (string_of_bool t); draw_string ", "; drawBoolList q;
	| _ -> ();;

let rec drawNodeList lnode =
	match lnode with
	| t::q -> let (i,j) = t in draw_string "( "; drawInt i; draw_string ", "; draw_char j; draw_string " )"
	| _ -> ();;

let printNode node =
	let ((i,j), lbool, lnode) = node in
	draw_string "( ";
	drawInt i; draw_string ", "; draw_char j; draw_string " ), (";
	drawBoolList lbool; draw_string " ), "; drawNodeList lnode;;

openGraph "1900" "1000";;

let grid = initGrid 4 4;;

magicDrawing grid printNode;;
