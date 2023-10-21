:- use_module(library(yall)).

input([1,1,1,3,1,2,2,1,1,3]).

look_and_say(s([Item|Items], Item-Count)) -->
    { Count1 is Count + 1 },
    look_and_say(s(Items, Item-Count1)).
look_and_say(s([Other|Items], Item-Count)) -->
    { \+ Other = Item },
    state(Acc, [Item,Count|Acc]),
    look_and_say(s(Items, Other-1)).
look_and_say(s([], Item-Count)) -->
    state(Acc, [Item,Count|Acc]).

look_and_say(In,Out) :-
    phrase(look_and_say(s(In,0-0)), [[]], [Lst]),
    reverse(Lst, [_,_|Out]).

expand_rle([], []).
expand_rle([Item-Count|Items], Out) :-
    times(Item, Count, Repeated),
    expand_rle(Items, Expand),
    append(Repeated, Expand, Out).

times(_, 0, []).
times(It, N, [It|Items]) :- N > 0, N1 is N - 1, times(It, N1, Items).

state(S0,S1), [S1] --> [S0].
repeatedly(0) --> [].
repeatedly(N) --> state(V0, V1),
                  { N > 0, length(V0, Len), writeln(len(Len)), time(look_and_say(V0, V1)), N1 is N - 1 },
                  repeatedly(N1).

part1(Ans) :- phrase(repeatedly(40), [[1,1,1,3,1,2,2,1,1,3]], [Out]), length(Out,Ans).

part2(Ans) :- phrase(repeatedly(50), [[1,1,1,3,1,2,2,1,1,3]], [Out]), length(Out,Ans).
