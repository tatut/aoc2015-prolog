:- use_module(library(clpfd)).

input(items(1,2,3,7,11,13,17,19,23,31,37,41,43,47,53,59,61,67,71,
            73,79,83,89,97,101,103,107,109,113)).

total_sum(S) :-
    input(I),
    I =.. [_ | Args],
    sum_list(Args, S).

compartment_sum(NumCompartments, S) :-
    total_sum(T),
    S #= T div NumCompartments.

% use the "bits control which items are taken" method

item_at(X, Bit, Val) :-
    1 is getbit(X, Bit),
    input(I),
    Bit1 is Bit + 1,
    arg(Bit1, I, Val), !.

item_at(X, Bit, 0) :- 0 is getbit(X,Bit).

items_total(B, _, 0) :- B > 28.
items_total(Bit, X, Total) :-
    item_at(X, Bit, At),
    Bit1 is Bit + 1,
    items_total(Bit1, X, Total1),
    Total is At + Total1, !.

items_product(B, _, 1) :- B > 28.
items_product(Bit, X, Product) :-
    item_at(X, Bit, At),
    Bit1 is Bit + 1,
    items_product(Bit1, X, Product1),
    Product is max(1,At) * Product1, !.


items_total(X, Total) :- items_total(0, X, Total).
items_product(X, Product) :- items_product(0, X, Product).

try1(Ans) :-
    compartment_sum(3, Sum),
    between(1,536870912,X),
    B is popcount(X),
    between(4,9,B),
    items_total(X, Sum),
    items_product(X, Ans),
    writeln(items_product(Ans)).

part1(Ans) :- aggregate(min(A), try1(A), Ans).
%part1 = 11846773891.

% part2, divide into 4 compartments,
% the bigger ones take 8 items and small takes 5
% 8*3+5 = 29

try2(Ans) :-
    compartment_sum(4, Sum),
    between(1,536870912,X),
    B is popcount(X),
    between(4,5,B),
    items_total(X, Sum),
    items_product(X, Ans).

part2(Ans) :- aggregate(min(A), try2(A), Ans).
% part2= 80393059.
