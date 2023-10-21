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

count_bits(N, Bits) :-
    Bits is getbit(N, 0)  + getbit(N, 1)  + getbit(N, 2)  + getbit(N, 3)  + getbit(N, 4) +
            getbit(N, 5)  + getbit(N, 6)  + getbit(N, 7)  + getbit(N, 8)  + getbit(N, 9) +
            getbit(N, 10) + getbit(N, 11) + getbit(N, 12) + getbit(N, 13) + getbit(N, 14) +
            getbit(N, 15) + getbit(N, 16) + getbit(N, 17) + getbit(N, 18) + getbit(N, 19).

% A more concise list based count_bits variant that is much slower
%count_bits(N, Bits) :-
%    numlist(0, 19, Lst),
%    foldl([B,Acc,Out]>>(Out is Acc + getbit(N,B)), Lst, 0, Bits).

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
