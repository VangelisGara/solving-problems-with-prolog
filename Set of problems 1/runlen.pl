/*  Apanthsh sthn erwthsh poy upovalete :       	  		 */	
/*  --------------------------------------------  	  		 */
/*  H prolog tha dwsei sthn erwthsh :             	  		 */
/*       ?- encode_rl([p(3),p(X),q(X),q(Y),q(4)], L). 		 */
/*  thn apanthsh :					    			  		 */
/*       L = [(p(X), 2), (q(4), 3)]					  		 */
/*  Auto sumvainei giati sthn prolog h isothta    	  		 */
/*	antimetwpizetai san enas elegxos gia to an oi 	  		 */
/*  2 oroi mporoun na enopoihthoun.							 */
/* (The = "operator" in Prolog is actually a predicate 		 */
/* (with infix notation) 2 ) that succeeds when 			 */
/*  the two  terms are unified. (Prolog Documention)         */
/* Katalabainoume loipon to oti h sugkrish twn orwn metaksu  */
/* tous , metafrezetai se enan elegxos unification			 */
/* Parathroume oti oi oroi : p(3) , p(X) ennopoiountai		 */
/* opws epishs : q(X),q(Y),q(4) . Auto apodeiknuetai me mia  */
/* aplh erthsh sthn prolog: p(3) = p(X) , h opoia epistrefei */
/* Yes , X=3 . Sunepws se kathe kodika , h aparaithth xrhsh  */
/* sugkrishs prokeimenou na elegxthoun 2 stoixeia , tha 	 */
/* katalhksei se epituxia ,an o oroi mporoun na enopoihthoun */
/* Afou p(X) = p(3) kai q(X)=q(Y)=q(4), tote einai 			 */
/* S = p(X) , kai N = 2 kai antistoixa gia to q  			 */
/* to S isoute me to teleutaio stoixeio panta 				 */

/* Encode Implemantation in Prolog */
/* ---------------------------------------------------------------------------------------------------------------- */

/* Terminator of encode_rl */
encode_rl([], _).
/* encode_rl call encode , function that differs in arguments */
encode_rl([H|T], L) :-
	encode([H|T], L, 1, []).		

/* encode is basically the function that does the encoding */

/* encode Terminator */
encode([], _, _, _).
/* In case we are on our final element of the list that we want to encode */
encode([X|[]], L, N , LT) :- 
	(
		N =\= 1 -> addlast((X,N), LT , L)
	)
	;
	addlast(X, LT, L).

/* Encode all elements except the last one */
encode([H|T], L, N, LT) :-
	/* Get the element next to the head*/
	first(F, T),
	/* Now compare'em */
	(
		/* if they are not equal :*/
		H\=F -> (
					/* If more than 1 same elements , add the element with format (Element,Counter) to the list */
					N =\= 1 -> addlast((H,N), LT, LN),
					   		   encode(T, L, 1, LN)
					;
					/* Else just add the element to list */
					addlast(H, LT, LN),
					encode(T, L, 1, LN)
				)
		;	
		/* in case of equality , increase the counter of same elements */
		/* and continue to the next pair of list elements */
		++(N, N1),
		encode(T, L , N1, LT)
	).

/* ----------------------------------------------------------------------------------------------------------------- */

/* Decode Implemantation in Prolog */
decode_rl([], _).

/* decode_rl function that starts the decoding */
decode_rl([H|T], L) :-
	/* Simirarly to decode */
	decode([H|T], L, []).

/* Terminator of decode */
decode([], _, _).
/* analyzes each element , so fillist decodes them right */
decode([H|T], L, LT) :-
	/* Get element and the times that we should add it to list */
	getsandn(H,S,N),
	/* fillist starts the decoding */
	fillist(S,N,T,LT,L).
	
/*fillist list gets the times that an element appears,each time appending it in a temp list  */

/* fillist Terminator*/
fillist(_, 0, _, [], _).
/* in case of last element of the list */
fillist(S, 1, [], LT , L) :-
	addlast(S, LT , L).

/* fillist works as explained above , i am using a temp list that elements are appented to */
/* and in the end the temporaty list is assigned to L */
fillist(S,N,T,LT,L) :-
	/* add element to temp list */
	addlast(S, LT, LN),
	--(N,N1),
	(
		/* if the are still same elements , we continue appending'em till counter hits 0 */
		N1 =\= 0 -> fillist(S, N1, T, LN, L)
		;
		/* else , case were not any more same elements are left , reset decode with the next pending pair , to decode */
		decode(T, L, LN)
	).
	
/* ----------------------------------------------------------------------------------------------------------------------*/
/* memo | H: Head , T: Tail , LT : Temp List , LN : New List , S : Element , N : Number*/

addlast(X, [], [X]).
addlast(X, [Y|L1], [Y|L2]) :-
	addlast(X, L1,L2).

first(X, [X|_]).

++(N, N1) :-
	N1 is N + 1. 
	
--(N, N1) :-
	N1 is N - 1.

getsandn((S1,N1),S,N) :- 
	S = S1, 
	N = N1.
 
getsandn(S1,S,N) :- 
	S = S1,
 	N = 1.
