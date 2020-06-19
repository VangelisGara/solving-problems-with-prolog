

 p(1). p(2). p(5). p(8).
r(2). r(4). r(5). r(7). r(9).
s(X) :- p(X), not r(X) , write('1').
s(X) :- r(X), not p(X), write('2').

% shuffle(+As,-Bs)
%  returns the shuffle of list As in list Bs
shuffle([],[]).
shuffle([A|As],Bs) :- shuffle(As,Xs), append(Ps,Qs,Xs), append(Ps,[A|Qs],Bs).