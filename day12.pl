:- use_module(library(http/json)).

input(Json) :-
    setup_call_cleanup(
        open('day12.txt', read, In),
        json_read(In, Json),
        close(In)).

state(S0,S1), [S1] --> [S0].
state(S), [S] --> [S].

should_ignore(true,Attrs) :- member(_=red, Attrs).

sum_numbers(Num) --> { integer(Num) }, state(s(Ign,Old), s(Ign,New)), { New is Old + Num }.
sum_numbers(Atom) --> { atom(Atom) }.
sum_numbers([]) --> [].
sum_numbers([I|Items]) --> sum_numbers(I), sum_numbers(Items).
sum_numbers(json(Attrs)) --> state(s(Ign,_)), { (should_ignore(Ign, Attrs), Check=[]); Check=Attrs },
                             sum_numbers(Check).
sum_numbers(_=Val) --> sum_numbers(Val).

part1(Ans) :- input(I), phrase(sum_numbers(I), [s(false,0)], [s(false,Ans)]).
part2(Ans) :- input(I), phrase(sum_numbers(I), [s(true,0)], [s(true,Ans)]).
