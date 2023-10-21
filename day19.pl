:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).

replacement(From-To) -->
    string_without(" ", From), " => ", string_without("\n", To).

replacements([R|Rs]) --> replacement(R), eol, replacements(Rs).
replacements([]) --> eol.

parse(input(Replacements, Molecule)) -->
    replacements(Replacements),
    string(Molecule), eol, eos.

input(I) :- phrase_from_file(parse(I), 'day19.txt').

% Try to create new molecule with given replacement
molecule(Input, From-To, New) :-
    append(From, Rest, Input),
    append(To,Rest,New).
molecule([F|Input], Replacement, [F|New]) :- molecule(Input,Replacement,New).

new_molecules(Input,Replacements, New) :-
    member(R,Replacements),
    molecule(Input,R,New).

part1(Ans) :-
    input(input(Replacements,Molecule)),
    aggregate(count, New, New^new_molecules(Molecule,Replacements,New), Ans).

% part1=509
