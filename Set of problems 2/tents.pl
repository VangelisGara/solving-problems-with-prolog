/* Import Libraries */
:-lib(ic).
:-lib(ic_global). % Only for sumlist
:- lib(branch_and_bound).

/* Main Predicate */
tents(RowTents, ColumnTents, Trees, Tents):-
	/* Get dimensions */
	length(RowTents, N),
	length(ColumnTents, M),
	/* Create variables and domains */
	createMap(N, M, Map),
	/* Plant trees on the map , Tree Constraint*/
	treeCon(Trees, Map),
	/* Row Constraint , Number of Tents in row */
	rowCon(RowTents, Map),
	/* Do the same for columns constraints */
	columnCon(ColumnTents,M, Map),
	/* Every tree should have a tent near it , constraint */
	neighborsCon(Trees, M, N, Map),
	/* Tents should be near each other , constraint */
	coupleCon(Map, M, N, 1, Map),
	/* Merge lists to one */
	flatten(Map, FlatMap),
	/* Get number of tents */
	sumlist(FlatMap, Cost),
	/* Find the first solution with less tents */ 
	bb_min(search(FlatMap, 0, input_order, indomain, complete, []), Cost, bb_options{strategy:continue}), !,
	
	/* Find rest of solutions as shown above */
	createMap(N, M, Map1),
	treeCon(Trees, Map1),
	rowCon(RowTents, Map1),
	neighborsCon(Trees, M, N, Map1),
	columnCon(ColumnTents,M, Map1),
	coupleCon(Map1, M, N, 1, Map1),
	flatten(Map1, FlatMap1), !,
	/* Get all solutions , replacing bb_min */
	search(FlatMap1, 0, input_order, indomain_middle, complete, []),
	sumlist(FlatMap1, Cost),
	getTents(FlatMap1, Tents, N, M, 1, 1).

/* Returns Map(List of lists) , the variables with the domains applied */
createMap(0, _, _).
createMap(N, M, Map):-
	N1 is N -1,
	createMap(N1,M,Map1),
	/* Create a variable for row*/
	length(Row, M),
	/* Apply domains*/
	Row :: [0,1],
	addFirst(Row, Map1, Map).

/* Sets variables given to 0 , as constraint */
treeCon([], _).
treeCon([H|T], Map):-
	treeCon(T, Map),
	H = X - Y,
	/* Finds row */
	getElement(X, Map, Row),
	/* Finds column */
	getElement(Y, Row, Tree),
	/* Apply Constraint , Plant Tree */
	Tree #= 0.

/* Number of tents in row must be less or eq to the constraints given*/
rowCon([], []).
rowCon([H|T], [X|Y]):-
	rowCon(T, Y),
	sumlist(X, M), % number of tents in row 
	/* apply constraint */
	(H >= 0->
		H #>= M
	;
		write('')).

/* Columns constraint like the row constraint*/
columnCon(ColumnTents, N, Map):-
	/* Conversion , get the collumns to row format */
	getColumns(N, Map, Columns),
	rowCon(ColumnTents, Columns).

/* Number of tents near tree must be at least 1 */
neighborsCon([], _, _, _).
neighborsCon([H|T], M, N ,Map):-
	neighborsCon(T, M, N, Map),
	H = X - Y,
	/* gets positions near tree so we can apply constraint */
	getListOfNeighbors(X, Y, Neighbors, M, N, Map),
	sumlist(Neighbors, N1),
	N1 #>= 1.

/* Sets up the couple constraints , for each col. apply con */
coupleCon([], _, _, _, _).
coupleCon([H|T], M, N, Z, Map):-
	Z1 is Z + 1,
	coupleCon(T, M, N, Z1, Map),
	/* apply constraint to rows */
	checkEveryElement(H, M, N, Z, 1, Map).

/* Get each var of row */
checkEveryElement([], _, _, _, _, _).
checkEveryElement([H|T], M, N, Z, W, Map):-
	W1 is W + 1,
	checkEveryElement(T, M, N, Z, W1, Map),
	getListOfNeighbors(Z, W, Neighbors, M, N, Map),
	/* Find the neighbors of each var and apply constraint */
	compareWIthNeighbor(H, Neighbors).

/* Apply Constraint */
compareWIthNeighbor(_, []).
compareWIthNeighbor(X, [H|T]):-
	compareWIthNeighbor(X, T),
	/* positions next to X or X must have maximum 1 tent */
	1 #>= X + H.

/*=================== Helping Predicates =====================*/
getTents([], [], _, _, _, _):-!.
getTents([H|T], Tents, N, M, X, M):-
	X1 is X + 1,
	getTents(T, Tents1, N, M, X1, 1),
	(H == 1 ->
		addFirst((X - M), Tents1, Tents)
	;
		Tents = Tents1
	),!.
getTents([H|T], Tents, N, M, X, Y):-
	Y1 is Y + 1,
	getTents(T, Tents1, N, M, X, Y1),
	(H == 1 ->
		addFirst((X - Y), Tents1, Tents)
	;
		Tents = Tents1
	),!.

/* Returns a list of neighboring positions to (X,Y) */
getListOfNeighbors(X, Y, Neighbors, M, N, Map):-
			Xplus is X+1,
			Xminus is X-1,
			Yplus is Y+1,
			Yminus is Y-1,
			/* Gets valid neighbors, for each position case , cases : */
			/* ~ Corner Limits Cases */
			/* Corner Up Left */
			(((X == 1) , (Y == 1)) ->
				getElement(X, Map, Row1),
				getElement(Yplus, Row1, Neighbor1),
				addFirst(Neighbor1, [], N1),
				
				getElement(Xplus, Map, Row2),
				getElement(Y, Row2, Neighbor2),
				addFirst(Neighbor2, N1, N2),
				
				getElement(Xplus, Map, Row3),
				getElement(Yplus, Row3, Neighbor3),
				addFirst(Neighbor3, N2, Neighbors)
			/* Corner Up Right */
			;((X == 1) , (Y == M)) ->
				getElement(X, Map, Row1),
				getElement(Yminus, Row1, Neighbor1),
				addFirst(Neighbor1, [], N1),
				
				getElement(Xplus, Map, Row2),
				getElement(Yminus, Row2, Neighbor2),
				addFirst(Neighbor2, N1, N2),
				
				getElement(Xplus, Map, Row3),
				getElement(Y, Row3, Neighbor3),
				addFirst(Neighbor3, N2, Neighbors)
			/* Corner Down Right */
			;((X == N) , (Y == M)) ->
				getElement(Xminus, Map, Row1),
				getElement(Y, Row1, Neighbor1),
				addFirst(Neighbor1, [], N1),
				
				getElement(Xminus, Map, Row2),
				getElement(Yminus, Row2, Neighbor2),
				addFirst(Neighbor2, N1, N2),
				
				getElement(X, Map, Row3),
				getElement(Yminus, Row3, Neighbor3),
				addFirst(Neighbor3, N2, Neighbors)
			/* Corner Down Left */
			;((X == N) , (Y == 1)) ->
				getElement(Xminus, Map, Row1),
				getElement(Y, Row1, Neighbor1),
				addFirst(Neighbor1, [], N1),
				
				getElement(Xminus, Map, Row2),
				getElement(Yplus, Row2, Neighbor2),
				addFirst(Neighbor2, N1, N2),
				
				getElement(X, Map, Row3),
				getElement(Yplus, Row3, Neighbor3),
				addFirst(Neighbor3, N2, Neighbors)
			/* ~ Garden's Limits Cases*/
			/* Top Side */
			;(X == 1) ->
				getElement(X, Map, Row1),
				getElement(Yminus, Row1, Neighbor1),
				addFirst(Neighbor1, [], N1),
				
				getElement(X, Map, Row2),
				getElement(Yplus, Row2, Neighbor2),
				addFirst(Neighbor2, N1, N2),
				
				getElement(Xplus, Map, Row3),
				getElement(Yminus, Row3, Neighbor3),
				addFirst(Neighbor3, N2, N3),

				getElement(Xplus, Map, Row4),
				getElement(Y, Row4, Neighbor4),
				addFirst(Neighbor4, N3, N4),
				
				getElement(Xplus, Map, Row5),
				getElement(Yplus, Row5, Neighbor5),
				addFirst(Neighbor5, N4, Neighbors)
			/* Right Side */
			;(Y == M) ->
				getElement(Xminus, Map, Row1),
				getElement(Y, Row1, Neighbor1),
				addFirst(Neighbor1, [], N1),
				
				getElement(Xminus, Map, Row2),
				getElement(Yminus, Row2, Neighbor2),
				addFirst(Neighbor2, N1, N2),
				
				getElement(X, Map, Row3),
				getElement(Yminus, Row3, Neighbor3),
				addFirst(Neighbor3, N2, N3),

				getElement(Xplus, Map, Row4),
				getElement(Yminus, Row4, Neighbor4),
				addFirst(Neighbor4, N3, N4),
				
				getElement(Xplus, Map, Row5),
				getElement(Y, Row5, Neighbor5),
				addFirst(Neighbor5, N4, Neighbors)
			/* Bottom Side */
			;(X == N) ->
				getElement(X, Map, Row1),
				getElement(Yminus, Row1, Neighbor1),
				addFirst(Neighbor1, [], N1),
				
				getElement(X, Map, Row2),
				getElement(Yplus, Row2, Neighbor2),
				addFirst(Neighbor2, N1, N2),
				
				getElement(Xminus, Map, Row3),
				getElement(Yminus, Row3, Neighbor3),
				addFirst(Neighbor3, N2, N3),

				getElement(Xminus, Map, Row4),
				getElement(Y, Row4, Neighbor4),
				addFirst(Neighbor4, N3, N4),
				
				getElement(Xminus, Map, Row5),
				getElement(Yplus, Row5, Neighbor5),
				addFirst(Neighbor5, N4, Neighbors)
			/* Left Side*/
			;(Y == 1) ->
				getElement(Xminus, Map, Row1),
				getElement(Y, Row1, Neighbor1),
				addFirst(Neighbor1, [], N1),
				
				getElement(Xminus, Map, Row2),
				getElement(Yplus, Row2, Neighbor2),
				addFirst(Neighbor2, N1, N2),
				
				getElement(X, Map, Row3),
				getElement(Yplus, Row3, Neighbor3),
				addFirst(Neighbor3, N2, N3),

				getElement(Xplus, Map, Row4),
				getElement(Y, Row4, Neighbor4),
				addFirst(Neighbor4, N3, N4),
				
				getElement(Xplus, Map, Row5),
				getElement(Yplus, Row5, Neighbor5),
				addFirst(Neighbor5, N4, Neighbors)

			; 
			/* Has all 8 neighbors */
				getElement(Xminus, Map, Row1),
				getElement(Y, Row1, Neighbor1),
				addFirst(Neighbor1, [], N1),
				
				getElement(Xminus, Map, Row2),
				getElement(Yplus, Row2, Neighbor2),
				addFirst(Neighbor2, N1, N2),
				
				getElement(X, Map, Row3),
				getElement(Yplus, Row3, Neighbor3),
				addFirst(Neighbor3, N2, N3),

				getElement(Xplus, Map, Row4),
				getElement(Y, Row4, Neighbor4),
				addFirst(Neighbor4, N3, N4),
				
				getElement(Xplus, Map, Row5),
				getElement(Yplus, Row5, Neighbor5),
				addFirst(Neighbor5, N4, N5),

				getElement(Xminus, Map, Row6),
				getElement(Yminus, Row6, Neighbor6),
				addFirst(Neighbor6, N5, N6),

				getElement(X, Map, Row7),
				getElement(Yminus, Row7, Neighbor7),
				addFirst(Neighbor7, N6, N7),
				
				getElement(Xplus, Map, Row8),
				getElement(Yminus, Row8, Neighbor8),
				addFirst(Neighbor8, N7, Neighbors)).

getColumns(0, _, []).
getColumns(N, Map, Columns):-
	N1 is N -1,
	getColumns(N1, Map, Columns1),
	getCol(N, Map, C),
	addLast(C, Columns1, Columns).

getCol(_, [], []).
getCol(Pos, [H|T], Column):-
	getCol(Pos, T, Column1),
	getElement(Pos, H, E),
	addFirst(E, Column1, Column).

getElement(_, [], _).
getElement(X, [H|T], E):-
	X1 is X - 1,
	getElement(X1, T, E),
	(X == 1 ->
		E = H
	;
		write('')).
 
addFirst(X, L, [X|L]).

addLast(X, [], [X]).
addLast(X, [Y|L1], [Y|L2]):- addLast(X,L1,L2).
