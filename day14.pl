:- use_module(library(dcg/basics)).
:- use_module(library(yall)).
:- set_prolog_flag(double_quotes, codes).

line(n(Speed, Duration, RestDuration)) -->
    string_without(" ", _),
    " can fly ", integer(Speed), " km/s for ", integer(Duration),
    " seconds, but then must rest for ", integer(RestDuration), " seconds.".

lines([L|Ls]) --> line(L), eol, lines(Ls).
lines([]) --> eos.

input(I) :- phrase_from_file(lines(I), 'day14.txt').

state(S0, S1), [S1] --> [S0].
state(S), [S] --> [S].

compete(n(Speed, FlyDur, RestDur), WantedTime) -->
    compete(n(Speed, FlyDur,RestDur), flying(FlyDur), WantedTime).

compete(n(Speed, Fd, Rd), flying(N), TimeLeft) -->
    { N > 0 },
    state(Flown0, Flown1),
    { TimeLeft > 0,
      Time1 is TimeLeft - 1,
      Flown1 is Flown0 + Speed,
      N1 is N - 1 },
    compete(n(Speed,Fd,Rd), flying(N1), Time1).

% Swap between flying and resting
compete(n(Speed,Fd,Rd), flying(0), T) --> compete(n(Speed,Fd,Rd), resting(Rd), T).
compete(n(Speed,Fd,Rd), resting(0), T) --> compete(n(Speed,Fd,Rd), flying(Fd), T).

compete(Reindeer, resting(N), T) -->
    { T > 0, N > 0, N1 is N - 1, T1 is T - 1 },
    compete(Reindeer, resting(N1), T1).

% End when no time is left
compete(_, _, 0) --> [].

compete_rounds(Rounds, Reindeer, Distance) :- phrase(compete(Reindeer, Rounds), [0], Distance).

compete_all(Farthest) :-
    input(Reindeer),
    maplist(compete_rounds(2503), Reindeer, Distances),
    sort(Distances, Sorted),
    last(Sorted, Farthest).

part1(A) :- compete_all(A).

% Part2, instead of single winner, award points at end of each round
% Hold all the state in semicontext, so we can step it one round at a time
% state s(flying(Rounds)|resting(Rounds), Distance, Speed, FlyDur, RestDur)

compete2(s(flying(N), Dist, Speed, Fd, Rd, Score),
         s(flying(N1), Dist1, Speed, Fd, Rd, Score)) :-
    N > 1,
    N1 is N - 1,
    Dist1 is Dist + Speed.

compete2(s(flying(1), Dist, Speed, Fd, Rd, Score),
         s(resting(Rd), Dist1, Speed, Fd, Rd, Score)) :- Dist1 is Dist + Speed.

compete2(s(resting(N), D, Sp, Fd, Rd, S),
         s(resting(N1), D, Sp, Fd, Rd, S)) :-
    N > 1,
    N1 is N - 1.

compete2(s(resting(1), D, Sp, Fd, Rd, S),
         s(flying(Fd), D, Sp, Fd, Rd, S)).

initial(n(Speed, Fd, Rd), s(flying(Fd), 0, Speed, Fd, Rd, 0)).

award_point(D, s(State,D,Sp,Fd,Rd,S), s(State,D,Sp,Fd,Rd,S1)) :- S1 is S + 1.
award_point(D, s(State,D1,Sp,Fd,Rd,S), s(State,D1,Sp,Fd,Rd,S)) :- \+ D = D1.

compete2_round(StatesIn, StatesOut) :-
    maplist(compete2, StatesIn, AfterRound),
    maplist([S,D]>>arg(2,S,D), AfterRound, Distances),
    sort(Distances, DistancesSorted),
    last(DistancesSorted, ScoreDistance),
    maplist(award_point(ScoreDistance), AfterRound, StatesOut).

compete2_rounds(0, S, S).
compete2_rounds(N, StateIn, StateOut) :-
    N > 0, N1 is N - 1,
    compete2_rounds(N1, StateIn, State),
    compete2_round(State, StateOut).

best_score(States, Best) :-
    maplist([S,Score]>>arg(6, S, Score), States, Scores),
    sort(Scores, Sorted),
    last(Sorted, Best).

part2(A) :-
    input(I),
    maplist(initial,I,Rs),
    compete2_rounds(2503, Rs, Out),
    best_score(Out, A).
