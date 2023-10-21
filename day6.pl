:- use_module(library(dcg/basics)).
:- use_module(library(rbtrees)).
:- set_prolog_flag(double_quotes, codes).

onoff(on) --> " on ".
onoff(off) --> " off ".

area(Sx-Sy, Ex-Ey) --> integer(Sx), ",", integer(Sy), " through ", integer(Ex), ",", integer(Ey).
cmd(turn(OnOff,S,E)) --> "turn", onoff(OnOff), area(S,E).
cmd(toggle(S,E)) --> "toggle ", area(S,E).

cmds([]) --> eos.
cmds([Cmd|Cmds]) --> cmd(Cmd), eol, cmds(Cmds).

input(C) :- phrase_from_file(cmds(C), 'day6.txt').

state(S0,S1), [S1] --> [S0].

each(_, _-Ey, _, Y, _) --> { Y > Ey }. % done
each(Sx-Sy, Ex-Ey, X, Y, Op) -->
    { Y =< Ey, X > Ex, Y1 is Y + 1 },
    each(Sx-Sy, Ex-Ey, Sx, Y1, Op). % end of line
each(Sx-Sy, Ex-Ey, X, Y, Op) -->
    { Y =< Ey, X =< Ex, X1 is X + 1 },
    operate(X,Y,Op),
    each(Sx-Sy, Ex-Ey, X1, Y, Op).

operate(X, Y, turn(off)) -->
    %{ writeln(turnoff(X,Y)) },
    state(Before, After),
    { (rb_delete(Before, X-Y, After); Before=After), ! }.
operate(X, Y, turn(on)) -->
    %{ writeln(turnon(X,Y)) },
    state(Before, After),
    { rb_insert(Before, X-Y, 1, After), ! }.
operate(X, Y, toggle) -->
    %{ writeln(toggle(X,Y)) },
    state(Before, After),
    { ((rb_lookup(X-Y, 1, Before), rb_delete(Before, X-Y, After));
       rb_insert(Before, X-Y, 1, After)), ! }.
operate(X, Y, bright(N)) -->
    state(Before, After),
    { (rb_lookup(X-Y, B, Before); B=0), B1 is max(0, B + N), rb_insert(Before, X-Y, B1, After), ! }.

instr(turn(OnOff, Sx-Sy, Ex-Ey)) --> {writeln(turn(OnOff))}, each(Sx-Sy, Ex-Ey, Sx, Sy, turn(OnOff)), { ! }.
instr(toggle(Sx-Sy, Ex-Ey)) --> {writeln(toggle)},each(Sx-Sy, Ex-Ey, Sx, Sy, toggle), { ! }.
instr(bright(B, Sx-Sy,Ex-Ey)) --> {writeln(bright(B))},each(Sx-Sy, Ex-Ey, Sx, Sy, bright(B)), { ! }.

instrs([]) --> [].
instrs([I|Is]) --> % {writeln(instr(I))},
                   instr(I), instrs(Is).

process_instructions(Is, Set1) :-
    rb_empty(Set),
    phrase(instrs(Is), [Set], [Set1]).

part1(Answer) :-
    input(Is),
    writeln(instructions(Is)),
    process_instructions(Is, Set),
    rb_size(Set, Answer).

fix_instr(turn(on, S, E), bright(1, S, E)).
fix_instr(turn(off, S, E), bright(-1, S, E)).
fix_instr(toggle(S,E), bright(2, S, E)).

total_bright(_-B, Acc, Total) :- Total is B + Acc.

part2(Answer) :-
    input(Is),
    maplist(fix_instr, Is, Fixed),
    process_instructions(Fixed, Set),
    rb_fold(total_bright, Set, 0, Answer).
