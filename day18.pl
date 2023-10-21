:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).
:- dynamic stuck/1.

on(35, 1).  % # means on
on(46, 0).  % . means off

line(L) --> string_without("\n", L), eol.
lines([L|Ls]) --> line(LCs), { maplist(on, LCs, L) }, more_lines(Ls).
more_lines([]) --> eos.
more_lines(Ls) --> lines(Ls).


idx(X-Y, Idx) :- Idx is Y*100 + X + 1.

input(Grid) :-
    phrase_from_file(lines(Ls), 'day18.txt'),
    foldl([Line,Acc,Out]>>append(Acc,Line,Out), Ls, [], GridArgs),
    compound_name_arguments(Grid, grid, GridArgs).

at(_, X-Y, 0) :- X < 0; Y < 0; X > 99; Y > 99.
at(Grid, X-Y, V) :-
    \+ stuck(X-Y),
    between(0, 99, X),
    between(0, 99, Y),
    idx(X-Y, Idx), arg(Idx, Grid, V), !.

at(_, P, 1) :- stuck(P), !.

on_neighbors(G, X-Y, On) :-
    Xp is X-1, Xn is X+1, Yp is Y-1, Yn is Y+1,
    at(G, Xp-Yp, N1),
    at(G, X-Yp, N2),
    at(G, Xn-Yp, N3),
    at(G, Xp-Y, N4),
    at(G, Xn-Y, N5),
    at(G, Xp-Yn, N6),
    at(G, X-Yn, N7),
    at(G, Xn-Yn, N8),
    On is N1+N2+N3+N4+N5+N6+N7+N8.

animate(_, _, _, 100) --> [].
animate(GIn, GOut, 100, Y) --> { Y1 is Y + 1 }, animate(GIn, GOut, 0, Y1).
animate(GIn, GOut, X, Y) -->
    { X < 100, Y < 100, on_neighbors(GIn, X-Y, On),
      X1 is X + 1, idx(X-Y, Idx),
      at(GIn, X-Y, Me),
      switch(Me, On, NextMe)
    },
    setlight(GOut, Idx, NextMe),
    animate(GIn, GOut, X1, Y).

setlight(G, Idx, V) --> { nb_setarg(Idx, G, V) }.

switch(1, 0, 0).
switch(1, 1, 0).
switch(1, 2, 1).
switch(1, 3, 1).
switch(1, 4, 0).
switch(1, 5, 0).
switch(1, 6, 0).
switch(1, 7, 0).
switch(1, 8, 0).

switch(0, 0, 0).
switch(0, 1, 0).
switch(0, 2, 0).
switch(0, 3, 1).
switch(0, 4, 0).
switch(0, 5, 0).
switch(0, 6, 0).
switch(0, 7, 0).
switch(0, 8, 0).

display(G) :-
    writeln('-----------'),
    compound_name_arguments(G, grid, Args),
    display_grid(Args).
display_grid([]).
display_grid(G) :-
    \+ G = [],
    length(Row, 100),
    append(Row, Rest, G),
    maplist(light_ch, Row, Codes),
    string_codes(S, Codes),
    writeln(S),
    display_grid(Rest).

light_ch(1, 35).
light_ch(0, 46).

all_lights(G,L) :- between(0,99,X),
                   between(0,99,Y),
                   at(G, X-Y, L).

animate_times(G1, _, 0) -->
    [Count],
    {
        % For part1 we could just sum the compound args
        % but part2 needs the special handling in at/3 to be present
        % so we aggregate a sum of matching all_lights predicate
        aggregate(sum(L), all_lights(G1,L), Count)
    }.

animate_times(G1, G2, N) -->
    { N > 0, N1 is N - 1 },
    animate(G1, G2, 0, 0),
    animate_times(G2, G1, N1).

part(Count) :-
    input(G), duplicate_term(G, GNext),
    phrase(animate_times(G,GNext,100), [Count]).

part1(Count) :-
    retractall(stuck(_)),
    part(Count).

part2(Count) :-
    assert(stuck(0-0)),
    assert(stuck(99-99)),
    assert(stuck(0-99)),
    assert(stuck(99-0)),
    part(Count).
