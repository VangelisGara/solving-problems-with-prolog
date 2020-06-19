/* My code defines successfully the constraints , the domains and the variables of the CSP problem domino , */
/* I manage to get all the successors for each domino and I know how to check if a list of successors can solve the frame */
/* I had a problem trying to generate of possible combinations of successors and check each one , due to overflows etc */
/* Not so fancy eh ? Nevermind , but just bellow starts a tremendous and creative way of prolog advanced manipulation */

/* Default Dominos Input */
dominos([(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),
(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),
(2,2),(2,3),(2,4),(2,5),(2,6),
(3,3),(3,4),(3,5),(3,6),
(4,4),(4,5),(4,6),
(5,5),(5,6),
(6,6)]).

/* Default Frame */
frame([
% 0 1 2 3 4 5 6 7
 [3,1,2,6,6,1,2,2],  % 0
 [3,4,1,5,3,0,3,6],  % 1
 [5,6,6,1,2,4,5,0],  % 2
 [5,6,4,1,3,3,0,0],  % 3
 [6,1,0,6,3,2,4,0],  % 4
 [4,1,5,2,4,3,5,5],  % 5
 [4,1,0,2,4,5,2,0]   % 6
]).


/* Print Dominos */
printsolution(_,[]).
printsolution([D|ND],[S|NS]) :-
	write('We put domino '),
	write(D),
	write(' in '),
	write(S),nl,
	printsolution(ND,NS).

/* Creates the variables list , a list with all possible dominos positions */
variables1([],_).
variables1( [(X1,Y1)-(X2,Y2)|T],L) :-
	variables1(T,L1),
	% nl,write(X1),write(Y1),write(X2),write(Y2),nl,
	Y3 is Y1 - Y2,
	X3 is X1 - X2,
	( ( ((X1 == X2) , (Y3 == 1; Y3 == -1)) ; ((Y1 == Y2) , (X3 == 1 ; X3 == -1))) -> addfirst([(X1,Y1)-(X2,Y2)],L1,L) ; copy(L1,L)).

/* Check if the  dominos positions given , can fill the frame successfully */
checksolution([],_,_).
checksolution([(X1,Y1,X2,Y2)|T],Frame,F) :-
	del((X1,Y1),Frame,Frame1),
	del((X2,Y2),Frame1,Frame2),
	checksolution(T,Frame2,F).

/* Gets the first possible solution */ 
get_first_solution([],_).
get_first_solution([H|T],P) :-
	get_first_solution(T,P1),
	first(X,H),
	addfirst(X,P1,P).
/* Sets up the creation for variables */
variables:-
	positions(1,2,M,2),
	pairs(M,L),
	variables1(L,L1),printlist(L1).

/* Tries to find all different ways to put dominos in the frame */
get_all_solutions([],_) :- write(','),nl.
get_all_solutions([P|NP],ND) :-
	write(P),nl,
	domino_successors(ND),
	get_all_solutions(NP,ND).

domino_successors([]) :- write('.'),nl.
domino_successors([D|ND]) :-
	get_all_solutions(D,ND).
		
put_dominos :- 
	frame(F) , 
	dominos(D) ,
	positions(6,7,M,7),
	write('CSP DOMINO\' DOMAINS :'),nl,
	search_dominos(F,D,Possible_Co),
	write('CSP DOMINO\' VARIABLES:'),nl,
	positions(6,7,M,7),printlist(M),nl,
	/* We should give a solution */
	get_first_solution(Possible_Co,Path),
	write('A Possible Solution :'),nl,
	printlist(Path),
	nl,write('Each line represents a domino from the dominos list given'),nl,
	(checksolution(Path,M,Flag) -> printsolution(D,Path) ; write('Solution not found')).

/* Gets the successor for each domino */
search_dominos(_,[],_).
search_dominos(Frame,[(F,L)|T],Possible_Co) :- 
	rsearch_domino_in_frame(Frame,(F,L),0,RMatches),
	
	first(H,Frame),
	list_length(H,Len),
	get_collumn(Frame,Len,Collumns),
	csearch_domino_in_frame(Collumns,(F,L),0,CMatches),
	
	search_dominos(Frame,T,Possible_co1),

	merge_list(RMatches,CMatches,Q),
	write('domino '),write('('),write(F),write(' '),write(L),write(')'),
	write(' successors :'),nl,printlist(Q),nl,
	addfirst(Q,Possible_co1,Possible_Co).

/* search domino's successors in the rows of the frame */
rsearch_domino_in_frame([],_,_,_).
rsearch_domino_in_frame([H|T],(F,L),R,Matches) :-
	R1 is R + 1,
	rsearch_list(H,(F,L),R,0,Match),
	rsearch_domino_in_frame(T,(F,L),R1,TempMatches),
	merge_list(Match,TempMatches,Matches).

/* search domino's successors in the Collumns of the frame */
csearch_domino_in_frame([],_,_,_).
csearch_domino_in_frame([H|T],(F,L),R,Matches) :-
	R1 is R + 1,
	csearch_list(H,(F,L),R,0,Match),
	csearch_domino_in_frame(T,(F,L),R1,TempMatches),
	merge_list(Match,TempMatches,Matches).

/* search domino's successors in the row R */
rsearch_list([_|[]],_,_,_,_).
rsearch_list([X,Y|T],(F,L),R,C,Match) :-
	C1 is C + 1 ,
	rsearch_list([Y|T],(F,L),R,C1,Match1),
	(((F = X , L = Y ) ; ( F = Y , L = X )) -> addfirst((R,C,R,C1) , Match1 , Match) ; copy(Match1,Match)).

/* search domino's successors in the collumn C */
csearch_list([_|[]],_,_,_,_).
csearch_list([X,Y|T],(F,L),R,C,Match) :-
	C1 is C + 1 ,
	csearch_list([Y|T],(F,L),R,C1,Match1),
	(((F = X , L = Y ) ; ( F = Y , L = X )) -> addfirst((C,R,C1,R) , Match1 , Match) ; copy(Match1,Match)).

/* Gets the collumns of the frame , as a list of lists */
get_collumn(_,0,_). 
get_collumn(Frame,R,Collumns) :-
	pop_col(Frame,[],L, [], CroppedFrame),
	R1 is R - 1,
	get_collumn(CroppedFrame,R1,TempCol),
	addfirst(L,TempCol,Collumns).

pop_col([],TempCol,Col,TempCFrame, CroppedFrame):- Col = TempCol, CroppedFrame = TempCFrame. 
pop_col([H|T],TempCol,Col, TempCFrame, CroppedFrame) :-
	first(X,H),
	addlast(X,TempCol,L2),
	removehead(H,Y),
	addlast(Y, TempCFrame, NW2),
	pop_col(T, L2,Col, NW2, CroppedFrame).

/*----------------------------------------------------------------------------------------------------------------------*/
/* Combines two lists into one */
merge_list([],L,L ).
merge_list([H|T],L,[H|M]):-
    merge_list(T,L,M).

/* Gets all coordinates for an 2d array with R rows amd C collumns */
positions(-1,_,L,_) :- printlist(L).
positions(R,-1,L,F) :-
	R1 is R - 1 ,
	positions(R1,F,L,F).
positions(R,C,L,F) :-
	C1 is C - 1 ,
	positions(R,C1,L1,F),
	addlast((R,C),L1,L).

/* checks if lists is empty */
list_empty([], 1).
list_empty([_|_], 0).

/* removes a sublist from list */
remove_list([], _, []).
remove_list([X|Tail], L2, Result):- member(X, L2), !, remove_list(Tail, L2, Result). 
remove_list([X|Tail], L2, [X|Result]):- remove_list(Tail, L2, Result).

printlist([]).
printlist([X|List]) :-
   write(X),nl,
   printlist(List).

/* leaves a list intact */
copy(L,R) :- accCp(L,R).
accCp([],[]).
accCp([H|T1],[H|T2]) :- accCp(T1,T2).

addlast(X, [], [X]).
addlast(X, [Y|L1], [Y|L2]) :-
	addlast(X, L1,L2).

addfirst(X,L,[X|L]).

/* add an element into a list after some constraints */
addfirst1(_, L, L , 1).
addfirst1(X, L, [X|L],0).

first(_,[] ).
first(X, [X|_]).

list_length(Xs,L) :- list_length(Xs,0,L) .
list_length( []     , L , L ) .
list_length( [_|Xs] , T , L ) :-
  T1 is T+1 ,
  list_length(Xs,T1,L).

removehead([_|Tail], Tail).

pop_col1([],_,_). 
pop_col1([H|T],Col,CroppedFrame) :-
	pop_col(T,TempCol,TempCroppedFrame),
	first(X,H),
	addfirst(X,TempCol,Col),
	removehead(H,Y),
	addfirst(Y,TempCroppedFrame,CroppedFrame).

/*  deletes an element from a list */
del(ElementX,[Element|Tail],Tail).   
del(Element,[Head|Tail1],[Head|Tail2]) :-
    del(Element,Tail1,Tail2).

/* creates all possible pairs given list elements */)
pairs([X|T],PS):- T=[_|_], pairs(X,T,T,PS,[]) ; T=[], PS=[].
pairs([],[]).

pairs(_,[],[],Z,Z).
pairs(_,[],[X|T],PS,Z):- pairs(X,T,T,PS,Z).
pairs(X,[Y|T],R,[X-Y|PS],Z):- pairs(X,T,R,PS,Z).