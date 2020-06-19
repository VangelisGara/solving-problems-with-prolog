/* H Askhsh akolouthei akribws thn idia logikh ths liars.pl , ths akshshs 2 */
/* Auto pou kanw einai na ekmetaleutw thn ic , gia na orisw to problhma san problhma periorismwn */

/* Insert IC library */
:-lib(ic).

liars_csp(L, Liars):-
	length(L, N),  % create variables
	length(Liars,N),  % number of friends
	Liars :: [0,1],   % create domains and assign them to variables
	Count_liars #= sum(Liars), % count number of liars 
	constraints(L, Liars, Count_liars),
	search(Liars,0, input_order, indomain, complete, []).

/* Apply all 3 constraints */
constraints(L, Liars, Count_liars):-
	false_claims(L, Liars),
	check_for_liars(L, Liars, Count_liars),
	check_for_sincere(L, Liars, Count_liars).

/* Decline occasions , when friends claim the same number of liars */
/* and one of' em is a liar and the other true */
false_claims([], []).
false_claims([H|T], [Liar|Rest_Liars]):-
	false_claims(T, Rest_Liars),
	check_claim(H, Liar,T, Rest_Liars).

check_claim(_, _, [], []).
check_claim(H, Liar, [H1|T1], [Liar1|Rest_Liars1]):-
	(H == H1 ->	
		Liar #= Liar1,
		check_claim(H, Liar, T1, Rest_Liars1)
	; 
		check_claim(H, Liar, T1, Rest_Liars1)).

/* Check_for liars and check_for_sincere , just like liars.pl  from exc2 ,*/
/* if someone is a liar then we demand that his prediction is false */
/* else we demand him to actually say the truth */
check_for_liars([], [], _).
check_for_liars([_|T], [0|Rest_Liars], Count_liars):-
	check_for_liars(T, Rest_Liars, Count_liars).
check_for_liars([H|T], [1|Rest_Liars], Count_liars):-	
	H #> Count_liars,
	check_for_liars(T, Rest_Liars, Count_liars).

check_for_sincere([], [], _).
check_for_sincere([_|T], [1|Rest_Liars], Count_liars):-
	check_for_sincere(T, Rest_Liars, Count_liars).
check_for_sincere([H|T], [0|Rest_Liars], Count_liars):-
	H #=< Count_liars,
	check_for_sincere(T, Rest_Liars, Count_liars).

/* Make L Predicates*/
genrand(N, L) :-
	length(L, N),
	make_list(N, L).

make_list(_, []).
make_list(N, [X|L]) :-
	random(R),
	X is R mod (N+1),
	make_list(N, L).
