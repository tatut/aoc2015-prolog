:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).

replacement(From-To) -->
    string_without(" ", From), " => ", string_without("\n", To).

replacements([R|Rs]) --> replacement(R), eol, replacements(Rs).
replacements([]) --> eol.

parse(input(Replacements, Molecule)) -->
    replacements(Replacements),
    string(Molecule), eol, eos.

input(Replacements,Molecule) :-
    phrase_from_file(parse(input(Replacements,Molecule)), 'day19.txt').

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

%% part2 idea, run replacements backwards with a random permutation


generate(Ans) :-
    input(Rs, Mol),
    random_permutation(Rs, Rand),
    substitute(Rand, Mol, "e", 0, Ans).

substitute(_, Target, Target, Steps, Steps).
substitute(Rs, Current, Target, AtStep, Steps) :-
    member(From-To, Rs),
    AtStep1 is AtStep + 1,
    molecule(Current1, From-To, Current),
    substitute(Rs, Current1, Target, AtStep1, Steps).

% part2=195
% after multiple retries, a random permutation produced that
