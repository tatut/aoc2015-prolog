:- use_module(library(clpfd)).
:- use_module(library(yall)).

%% inc(Key, AssocIn, AssocOut) :-
%%     (get_assoc(Key, AssocIn, V); V=0),
%%     V1 is V + 1,
%%     put_assoc(Key, AssocIn, V1, AssocOut).

%% % remove key when decrementing a 1
%% dec(Key, AssocIn, AssocOut) :- get_assoc(Key, AssocIn, 1), del_assoc(Key, AssocIn, 1, AssocOut).
%% dec(Key, AssocIn, AssocOut :- get_assoc(Key, AssocIn, V),
%%                               V > 1,
%%                               V1 = V - 1,
%%                               put_assoc(Key, AssocIn, V1, AssocOut).

%% input(I) :-
%%     empty_assoc(Empty),
%%     foldl(inc, [33, 14, 18, 20, 45, 35, 16, 35, 1, 13, 18, 13, 50, 44, 48, 6, 24, 41, 30, 42],
%%           Empty, I).


%% % Exactly this jugs worth needed, succeed
%% fits(Jugs, J) :- get_assoc(J, Jugs, _).

%% fits(Jugs, N) :-
%%     gen_assoc(J, Jugs, C),
%%     N #> J,
%%     N1 #= N - J,
%%     dec(J, Jugs, Jugs1),
%%     fits(Jugs1, N1).


j( 1, 1).
j( 2, 6).
j( 3, 13).
j( 4, 13).
j( 5, 14).
j( 6, 16).
j( 7, 18).
j( 8, 18).
j( 9, 20).
j(10, 24).
j(11, 30).
j(12, 33).
j(13, 35).
j(14, 35).
j(15, 41).
j(16, 42).
j(17, 44).
j(18, 45).
j(19, 48).
j(20, 50).

on(N, B) :- 1 is (N>>(B-1)) /\ 1.

sum_on(In, B, Acc, Out) :-
    1 is getbit(In,B-1),
    j(B, N),
    Out is Acc + N.

sum_on(In, B, Acc, Acc) :- 0 is getbit(In,(B-1)).

%% select by numeric bits
take_jugs(In, Sum) :-
    numlist(1,20,Bits),
    foldl(sum_on(In), Bits, 0, Sum).

count(1048577, 0).
count(N, Acc) :-
    N < 1048577,
    N1 is N + 1,
    count(N1, Acc1),
    ((take_jugs(N, 150), Inc = 1; Inc = 0)),
    Acc is Acc1 + Inc.

% part1=1304

count_bits(N, Bits) :-
    Bits is getbit(N, 0)  + getbit(N, 1)  + getbit(N, 2)  + getbit(N, 3)  + getbit(N, 4) +
            getbit(N, 5)  + getbit(N, 6)  + getbit(N, 7)  + getbit(N, 8)  + getbit(N, 9) +
            getbit(N, 10) + getbit(N, 11) + getbit(N, 12) + getbit(N, 13) + getbit(N, 14) +
            getbit(N, 15) + getbit(N, 16) + getbit(N, 17) + getbit(N, 18) + getbit(N, 19).

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

% part2=18
