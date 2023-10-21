:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).

gift(dim(L,W,H)) --> integer(L), "x", integer(W), "x", integer(H).
gifts([G|Gs]) --> gift(G), eol, gifts(Gs).
gifts([]) --> [].

wrapping(dim(L,W,H), Wrapping) :-
    Small is min(L*W,min(W*H,H*L)),
    Wrapping is 2*L*W + 2*W*H + 2*H*L + Small.

ribbon(dim(L,W,H), Ribbon) :-
    Ribbon is min(L+W+L+W, min(W+H+W+H, H+L+H+L)) + L*W*H.

part(Goal, Answer) :-
    phrase_from_file(gifts(Gs), 'day2.txt'),
    maplist(Goal, Gs, Results),
    sum_list(Results, Answer).

part1(Answer) :- part(wrapping, Answer).
part2(Answer) :- part(ribbon, Answer).
