/* Max Clique Problem Is Solved Here */
maxclq(N,D,Clique,Size) :-
  create_graph(N,D,G),
  graph_formating(N,G,Graph),
  findmax(N,Graph,Clique),
  length(Clique,Size).

/* Recreate graph in a format viable for find a find_a_clique */
graph_formating(N,G,L) :-
    range(N,Vs),
    findall((X,Y),
        setof(Y,(member(X,Vs),(member(X-Y,G);member(Y-X,G))),Y),L).

/* Finds the max clique */
findmax(N,G,C) :-
  ( (find_a_clique(N,G,Clique) ) -> (C = Clique ,! ); ( N1 is N - 1,findmax(N1,G,C) ) ).

/* Finds a clique with size N */
find_a_clique(0, _, []):-!.
find_a_clique(N, Graph, [Vertex|Clique]):-
  other_vertexes((Vertex,Neighbours), Graph, Graph1),
  closest_neighbors(Graph1, Neighbours, Graph2),
  N1 is N - 1,
  find_a_clique(N1, Graph2, Clique).

/* Get the vertexes which have more common neighbours*/
closest_neighbors([], _, []).
closest_neighbors([(V,_)|X], Y, Z):-
  \+ member(V, Y), !, closest_neighbors(X, Y, Z).
closest_neighbors([X|X1], Y, [X|Z]):-
  closest_neighbors(X1, Y, Z).

/* Get the other_vertexes of the vertexes */
other_vertexes(X, [X|Y], Y).
other_vertexes(X, [_|Y], Z):-other_vertexes(X, Y, Z).

/* ------------------------------------------------------------- */
create_graph(NNodes, Density, Graph) :-
   cr_gr(1, 2, NNodes, Density, [], Graph).

cr_gr(NNodes, _ , NNodes, _ , Graph, Graph).
cr_gr(N1, N2, NNodes, Density, SoFarGraph, Graph) :-
   N1 < NNodes,
   N2 > NNodes,
   NN1 is N1 + 1,
   NN2 is NN1 + 1,
   cr_gr(NN1, NN2, NNodes, Density, SoFarGraph, Graph).
cr_gr(N1, N2, NNodes, Density, SoFarGraph, Graph) :-
   N1 < NNodes,
   N2 =< NNodes,
   rand(1, 100, Rand),
   (Rand =< Density ->
      append(SoFarGraph, [N1 - N2], NewSoFarGraph) ;
      NewSoFarGraph = SoFarGraph),
   NN2 is N2 + 1,
   cr_gr(N1, NN2, NNodes, Density, NewSoFarGraph, Graph).

rand(N1, N2, R) :-
   random(R1),
   R is R1 mod (N2 - N1 + 1) + N1.

range(N,L) :-
    range(1,N,L).
range(I,N,[]) :-
    I > N.
range(I,N,[I|L]) :-
    I =< N,
    I1 is I+1,
    range(I1,N,L).
    