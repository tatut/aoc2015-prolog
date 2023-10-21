:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).

go(F,F) --> blanks.
go(N,F) --> "(", { N1 is N + 1 }, go(N1,F).
go(N,F) --> ")", { N1 is N - 1 }, go(N1,F).

part1(Answer) :- phrase_from_file(go(0,Answer), 'day1.txt').

state(OldState, NewState), [NewState] --> [OldState].

basement([40|Cs]) --> state(F0-P0, F1-P1), { F1 is F0 + 1, P1 is P0 + 1}, basement(Cs).
basement([41|Cs]) --> state(F0-P0, F1-P1), { F0 > 0, F1 is F0 - 1, P1 is P0 + 1}, basement(Cs).
basement([41|_]) --> state(0-P0, position(P0)).

part2(Answer) :-
    read_file_to_codes('day1.txt', Cs, []),
    phrase(basement(Cs), [0-1], [position(Answer)]).
