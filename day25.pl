input(2978-3083).
% To continue, please consult the code grid in the manual.  Enter the code at row 2978, column 3083.

next(In, Out) :-
    Out is (In * 252533) rem 33554393.

fill(Wanted, Out) :-
    fill(1-1, Wanted, 20151125, Out).

nextpos(1-C, R1-1) :- R1 is C + 1.
nextpos(R-C, R1-C1) :- R > 1, R1 is R - 1, C1 is C + 1.

fill(Pos, Pos, Val,Val). % at wanted position, value is the answer
fill(P1, Wanted, V1, Ans) :-
    \+ P1 = Wanted,
    next(V1, V2),
    nextpos(P1, P2),
    fill(P2, Wanted, V2, Ans).

part1(Ans) :- input(P), fill(P, Ans).
%part1=2650453
