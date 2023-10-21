:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).

sue(sue(N, Props)) -->
    "Sue ", integer(N), ": ", sue_props(PropList), { list_to_ord_set(PropList, Props) }.

sue_props([Prop-Num|Props]) --> string_without(":", PropCs), { atom_codes(Prop, PropCs) },
                                ": ", integer(Num), more_sue_props(Props).
more_sue_props([]) --> [].
more_sue_props(Props) --> ", ", sue_props(Props).

sues([S|Ss]) --> sue(S), eol, sues(Ss).
sues([]) --> eos.

input(I) :- phrase_from_file(sues(I), 'day16.txt').

matches1([], _).
matches1([Prop-Val|Ps], Props) :- (ord_memberchk(Prop-Val, Props); \+ member(Prop-_, Props)),
                                 matches1(Ps, Props).

find_sue(Match, Output) :-
    input(Sues),
    list_to_ord_set([children-3, cats-7, samoyeds-2, pomeranians-3, akitas-0,
                     vizslas-0, goldfish-5, trees-3, cars-2, perfumes-1], WantedSet),
    member(sue(Output,Props), Sues),
    call(Match, Props, WantedSet).


part1(Ans) :- find_sue(matches1, Ans).

matches2([], _).
matches2([Prop-Val|Ps], Props) :-
    ( (member(Prop-V, Props),
       (matchop(Prop, Op); Op = '='),
       call(Op, Val, V))
    ; \+ member(Prop-_, Props)),
    matches2(Ps, Props).

matchop(trees, >).
matchop(cats, >).
matchop(pomeranians, <).
matchop(goldfish, <).

part2(Ans) :- find_sue(matches2, Ans).
% 405 oikein, mutta ei ainoa tulos (part1 103 antaa myös)
