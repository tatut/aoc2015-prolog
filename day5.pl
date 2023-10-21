:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).

words([]) --> eos.
words([W|Words]) --> string_without("\n", W), eol, words(Words).

% aeiou
vowel(97).
vowel(101).
vowel(105).
vowel(111).
vowel(117).

three_vowels(W) :-
    include(vowel, W, Vowels),
    length(Vowels, VC),
    VC >= 3.

repeated([C1,C1|_]).
repeated([_,C|Cs]) :- repeated([C|Cs]).

contains([C1,C2], [C1,C2|_]).
contains(Wanted, [_,C|Cs]) :- contains(Wanted, [C|Cs]).

no_forbidden(W) :-
    \+ contains("ab", W),
    \+ contains("cd", W),
    \+ contains("pq", W),
    \+ contains("xy", W).

nice(W) :-
    three_vowels(W),
    repeated(W),
    no_forbidden(W).

part(Goal, Ans) :-
    phrase_from_file(words(Ws), 'day5.txt'),
    include(Goal, Ws, Included),
    length(Included, Ans).

part1(Ans) :- part(nice, Ans).

repeated2([C1,C2|Cs]) :- contains([C1,C2], Cs).
repeated2([_|Cs]) :- repeated2(Cs).

repeat_letter_between([C1,_,C1|_]).
repeat_letter_between([_|Cs]) :- repeat_letter_between(Cs).

nice2(W) :- repeated2(W), repeat_letter_between(W).

part2(Ans) :- part(nice2, Ans).
