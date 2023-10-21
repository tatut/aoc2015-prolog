:- use_module(library(dcg/basics)).
:- use_module(library(ugraphs)).
:- use_module(library(yall)).
:- set_prolog_flag(double_quotes, codes).

line(Name1-(Name2-Happiness)) -->
    string_without(" ", Name1Cs), " would ", lose_or_gain(N), " ", integer(Hap),
    { Happiness is N * Hap },
    " happiness units by sitting next to ",
    string_without(".", Name2Cs), ".",
    { atom_codes(Name1, Name1Cs), atom_codes(Name2, Name2Cs) }.

lose_or_gain(1) --> "gain".
lose_or_gain(-1) --> "lose".

lines([]) --> eos.
lines([L|Ls]) --> line(L), eol, lines(Ls).

input(Rules) :- phrase_from_file(lines(Ls), 'day13.txt'), group_pairs_by_key(Ls, Rules).


sitting_next_to([_,_], []).
sitting_next_to([P1,P2,P3|Rest], [P1-P2, P2-P1, P2-P3, P3-P2|NextTo]) :-
    sitting_next_to(Rest, NextTo).

seating_graph(Diners, G) :-
    [First|RestDiners1] = Diners,
    append(RestDiners1, [First], RestDiners),
    maplist([D,N,D-N]>>true, Diners, RestDiners,  NextLinks),
    maplist([D,N,D-N]>>true, RestDiners, Diners, PrevLinks),
    append(NextLinks,PrevLinks, Links),
    add_edges([], Links, G).

diner_happiness(AllRules, SeatingGraph, Diner, Happiness) :-
    member(Diner-Rules, AllRules),
    neighbors(Diner, SeatingGraph, Neighbors),
    maplist([N,H]>>member(N-H,Rules), Neighbors, Happiness0),
    sum_list(Happiness0, Happiness).

% Generate happiness of all different seating arrangements
seating_happiness(AllRules, TotalHappiness) :-
    pairs_keys(AllRules, Diners),
    permutation(Diners, DinersP),
    seating_graph(DinersP, Seating),
    maplist(diner_happiness(AllRules, Seating), Diners, Happies),
    sum_list(Happies, TotalHappiness).

part1(Ans) :-
    input(AllRules),
    findall(H, seating_happiness(AllRules,H), Hs),
    max_list(Hs, Ans).


add_myself(RulesIn, [me-[_-0]|RulesOut]) :-
    %findall(Diner, member(Diner-_, RulesIn), Diners),
    maplist([N-R, N-[me-0|R]]>>true, RulesIn, RulesOut).

part2(Ans) :-
    input(InputRules),
    add_myself(InputRules, AllRules),
    findall(H, seating_happiness(AllRules,H), Hs),
    max_list(Hs, Ans).
