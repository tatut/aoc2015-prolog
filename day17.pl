:- use_module(library(dcg/basics)).
:- dynamic j/2.

% define jugs as facts because they are indexed for
% fast access.
% we could also use a 20 arg compound term as an "array"

jugs(N) --> integer(J), { assertz(j(N, J)), N1 is N + 1 },
            eol, jugs(N1).
jugs(20) --> eos.

input :-
    retractall(j(_,_)),
    phrase_from_file(jugs(0), 'day17.txt').

sum_on(In, B, Acc, Out) :-
    1 is getbit(In,B),
    j(B, N),
    Out is Acc + N.

sum_on(In, B, Acc, Acc) :- 0 is getbit(In,B).

%% select by numeric bits
take_jugs(In, Sum) :-
    numlist(0,19,Bits),
    foldl(sum_on(In), Bits, 0, Sum).

count(1048577, 0).
count(N, Acc) :-
    N < 1048577,
    N1 is N + 1,
    count(N1, Acc1),
    ((take_jugs(N, 150), Inc = 1; Inc = 0)),
    Acc is Acc1 + Inc.

part1(Ans) :- input, count(1, Ans).
% part1=1304

count_bits(N, Bits) :- Bits is popcount(N).

count_min_bits(1048577, 20).
count_min_bits(N, Min) :-
    N < 1048577,
    N1 is N + 1,
    count_min_bits(N1, Min1),
    ((take_jugs(N,150), count_bits(N, Bits), Min is min(Bits, Min1));
     Min = Min1).

count_with_bits(1048577, _, 0).
count_with_bits(N, Bits, Acc) :-
    N < 1048577,
    N1 is N + 1,
    count_with_bits(N1, Bits, Acc1),
    ((count_bits(N, Bits), take_jugs(N, 150), Inc=1); Inc=0),
    Acc is Acc1 + Inc.

part2(Ans) :-
    input,
    count_min_bits(1, Bits),
    count_with_bits(1, Bits, Ans).

% part2=18
