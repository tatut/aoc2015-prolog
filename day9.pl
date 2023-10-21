:- use_module(library(dcg/basics)).
:- use_module(library(yall)).
:- set_prolog_flag(double_quotes, codes).

distance(d(From, To, Km)) --> string_without(" ", FromCs), { atom_codes(From, FromCs) },
                              " to ",
                              string_without(" ", ToCs), { atom_codes(To, ToCs) },
                              " = ", integer(Km).

distances([]) --> eos.
distances([D|Ds]) --> distance(D), eol, distances(Ds).

route(Ds, From, To, Len) :- member(d(From,To,Len), Ds).
route(Ds, From, To, Len) :-
    route(Ds, From, Intermediate, Len0),
    route(Ds, Intermediate, To, Len1),
    Len is Len0 + Len1.

connected(Ds, From, To, Len) :- member(d(From,To,Len), Ds).
connected(Ds, From, To, Len) :- member(d(To,From,Len), Ds).

build_route(_, _, []).
build_route(Ds, From, [d(From,To,Len)|Ts]) :-
    connected(Ds, From, To, Len),
    build_route(Ds, To, Ts).

trip_len(Ds, Len) :-
    maplist([d(_,_,L),L]>>true, Ds, Lens),
    sum_list(Lens, Len).

input(I) :- phrase_from_file(distances(I), 'day9.txt').

all_places(I, Places) :-
    maplist([d(From,_,_), From]>>true, I, AllFroms),
    maplist([d(_,To,_), To]>>true, I, AllTos),
    append(AllFroms,AllTos, AllPlaces),
    sort(AllPlaces, Places).

route(Route) :-
    input(I),
    all_places(I, Places),
    member(P, Places),
    length(Places, NumPlaces),
    RouteLen is NumPlaces - 1,
    length(Route,RouteLen),
    build_route(I, P, Route),
    all_places(Route, Places).

% 562 too high

part1(Ans) :-
    findall(Len, (route(Route), trip_len(Route,Len)), Lengths),
    sort(Lengths, [Ans|_]).

part2(Ans) :-
    findall(Len, (route(Route), trip_len(Route,Len)), Lengths),
    sort(Lengths, Sorted),
    last(Sorted, Ans).
    
    
