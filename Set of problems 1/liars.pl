/* liars.pl works by suggesting all possible compinations , of who can lie and who not*/

/* Investigates who is actually lying and who is telling the truth. */
/*  Example of test_liar functionality */
/* [3,2,1,4,2] : Friends Claims and [0,1,0,0,1] a suggestion their trustworthiness*/
/* if the 1rst friend is saying the truth , then there should be at least 3 liars in the party, which is false */
/* if the 2nd friend is a liar , then there should be no more than 1 liar, which is false too */
/* So this combination is not a valid one */

	/* if friend claims to say the truth , then it checks if there are actually the liars he claims */
test_liar(N,0,HF,1) :-
    N >= HF,
    !.

/* if friend is proposed guilt for lying ,  then it checks if he is an actual liar. */
test_liar(N,1,HF,1) :-
    N < HF,
    !.
test_liar(_,_,_,0).

custom_and(1,1,1) :-
    !.
custom_and(_,_,0).

test_liars(_,[],_,1).
test_liars(N,[HF|TF],[HT|TT],Flag) :-
          test_liar(N,HT,HF,F),
          test_liars(N,TF,TT,SubFlag),
          custom_and(SubFlag,F,Flag).

	/* Creates all possible combinations and tests them , to find the actual liars */
liar_detector(_,_,0,_).
liar_detector(Friends,Try,Max,Solution) :-
	    /* if we reach the final combination , give up the search for the liars */
		Try == Max -> liar_detector(Friends,Try,0,Solution) ;
		(
			/* Gets the next combination of possible liers */
			add_binary(Try,[1,0],NewTry),
			liar_detector(Friends,NewTry,Max,Solution),
			/* counts the liars of the combination */
			count(1,Try,N),
			/* checks if the liars suggested , are actually them */
			test_liars(N,Friends,Try,Flag),
			solution(Flag,Try,Solution)
		).

/* Solution sets the Solution List */	
/* Case of not satisfying combination */
solution(0,_,_).
/* Case of satisfying combination */
solution(1,Try,Result) :-
	Result = Try.

/* The actual predicate. */
/* Sets up the critical data for the detector to work */
liars([],[]).
liars(Friends,L) :-
	/* Gives the number of friends , so it can create all possible combinations */
	list_length(Friends,Number),
	/* Starting combination , in the format [0,0,...,Last Friend] */
	first_compination(Number,[],C),
	/* Finds the maximum number of combinations given those friends */
	pow(2,Number,Stop),
	/* Gets the terminating combination */
	decimal_to_binary(Stop,Max),
	liar_detector(Friends,C,Max,L).


/* Helping predicates*/
/* ------------------------------------------------------- */
/* Z = X^Y */
pow(_,0,1).
pow(X,Y,Z) :- Y1 is Y - 1,
              pow(X,Y1,Z1), Z is Z1*X.

/* L = Binary_of(N) */
decimal_to_binary(0,[0]).
decimal_to_binary(1,[1]).
decimal_to_binary(N,L):- 
    N > 1,
    X is N mod 2,
    Y is N // 2,  
    decimal_to_binary(Y,L1),
    L = [X|L1].

/*For N , L = [0,0,...,N]*/
first_compination(0,LT,L) :- L = LT.
first_compination(N,LT,L) :-
	addlast(0,LT,LN),
	N1 is N -1  ,
	first_compination(N1,LN,L).

/* New_List = [X1,X2,...,XN] + [Y1] = [X1,...,Y1] */
addlast(X, [], [X]).
addlast(X, [Y|L1], [Y|L2]) :-
	addlast(X, L1,L2).

/* Count the times an element exists in a list */
count(_, [], 0) :- !.
count(X, [X|T], N) :- 
    count(X, T, N2), 
    N is N2 + 1.     
count(X, [Y|T], N) :- 
    X \= Y,        
    count(X, T, N). 

/* Length of the list */
list_length([], Number):-
    Number is 0.
list_length([_|T], Number):-
    list_length(T, L),
    Number is L + 1.

/* [0,0,0,0,0] + [1] = [1,0,0,0,0] */
add_binary(AL, BL, CL) :-
   add_binary(AL, BL, 0, CL).
add_binary([A | AL], [B | BL], Carry, [C | CL]) :-
   X is (A + B + Carry),
   C is X rem 2,
   NewCarry is X // 2,
   add_binary(AL, BL, NewCarry, CL).
add_binary([], BL, 0, BL) :- !.
add_binary(AL, [], 0, AL) :- !.
add_binary([], [B | BL], Carry, [C | CL]) :-
   X is B + Carry,
   NewCarry is X // 2,
   C is X rem 2,
   add_binary([], BL, NewCarry, CL).
add_binary([A | AL], [], Carry, [C | CL]) :-
   X is A + Carry,
   NewCarry is X // 2,
   C is X rem 2,
   add_binary([], AL, NewCarry, CL).
add_binary([], [], Carry, [Carry]).

