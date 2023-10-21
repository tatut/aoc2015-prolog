:- set_prolog_flag(double_quotes, codes).

% must have increasing straight (like a,b,c or x,y,z)
increasing_straight([C1,C2,C3|_]) :-
    C2 is C1 + 1,
    C3 is C1 + 2.
increasing_straight([_|Rest]) :- increasing_straight(Rest).

bad_letter(105).
bad_letter(111).
bad_letter(108).
% i,o,l are bad letters
bad_letters([C|_]) :- bad_letter(C).
bad_letters([_|Rest]) :- bad_letters(Rest).

% two separate double letters
doubled_letters([C,C|Rest], Rest).
doubled_letters([_|Rest], Out) :- doubled_letters(Rest, Out).

doubled_letters_twice(Str) :-
    doubled_letters(Str, Rest),
    doubled_letters(Rest, _).

valid_password(Pass) :-
    reverse(Pass, P),
    \+ bad_letters(P), doubled_letters_twice(P), increasing_straight(P).

% z (122) is highest char, a (97) is the lowest
% we want strings reversed, so it is easier
increment([122|In], [97|Out]) :- increment(In, Out).
increment([C|In], [C1|In]) :- \+ C = 122, C1 is C + 1.

input(I) := reverse("hepxcrrq", I).

state(S0,S1), [S1] --> [S0].
state(S), [S] --> [S].

next_valid_password --> state(pw(Pw)), { valid_password(Pw) }.
next_valid_password --> state(pw(Old), pw(New)), { increment(Old, New) }, next_valid_password.

get_next_valid(Input, Output) :-
    reverse(Input, R),
    increment(R, R1),
    phrase(next_valid_password, [pw(R1)], [pw(Next)]),
    reverse(Next, Pass),
    string_codes(Output, Pass).
