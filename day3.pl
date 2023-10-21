:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).

move( 94, X0-Y0, X0-Y1) :- Y1 is Y0 - 1. % ^
move( 60, X0-Y0, X1-Y0) :- X1 is X0 - 1. % <
move( 62, X0-Y0, X1-Y0) :- X1 is X0 + 1. % >
move(118, X0-Y0, X0-Y1) :- Y1 is Y0 + 1. % v

state(S0, S1), [S1] --> [S0].

moves([10]) --> [].
moves([M|Moves]) -->
    state(s([P0|Ps0],Gifts), s(Ps1,[P1|Gifts])),
    { move(M, P0, P1), append(Ps0, [P1], Ps1) },
    moves(Moves).

unique_houses(Movers, Moves, Count) :-
    phrase(moves(Moves), [s(Movers, [0-0])], [s(_, Gifts)]),
    sort(Gifts, Houses),
    length(Houses, Count).

input(Input) :- read_file_to_codes('day3.txt', Input, []).

part1(Ans) :- input(Moves), unique_houses([0-0],Moves, Ans).
part2(Ans) :- input(Moves), unique_houses([0-0,0-0],Moves, Ans).
