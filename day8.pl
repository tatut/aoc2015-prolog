:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).

ch(92) --> "\\\\".
ch(C) --> "\\", [C], { \+ C = 120 }. % not \xCC
ch(Digit) --> "\\x", [C1, C2], { atom_codes(A, [C1,C2]), hex_bytes(A, [Digit]) }.
ch(Ch) --> [Ch], { \+ Ch = 92 }.

chs([C|Chs]) --> ch(C), chs(Chs).
chs([]) --> [].

str(S) --> "\"", chs(S), "\"", eos.

raw_line(L) --> string_without("\n", L), eol.

lines([]) --> eos.
lines([line(Raw,Parsed)|Ls]) --> raw_line(Raw), { phrase(str(Parsed), Raw) }, lines(Ls).

code_len(line(Raw,_), Len) :- length(Raw, Len).
memory_len(line(_, Parsed), Len) :- length(Parsed, Len).

total_len(G, Lines, Total) :-
    maplist(G, Lines, Lens),
    sum_list(Lens, Total).


encode_mem([],[]).
encode_mem([C|Cs], [C|Encoded]) :- \+ C = 92, \+ C = 34, encode_mem(Cs, Encoded).
encode_mem([92|Cs], [92,92|Encoded]) :- encode_mem(Cs, Encoded).
encode_mem([34|Cs], [92,34|Encoded]) :- encode_mem(Cs, Encoded).

enc(Code, [34|Enc]) :-
    encode_mem(Code, Encoded),
    append(Encoded, [34], Enc).

enc_len(line(Raw,_),L) :- enc(Raw,Enc), length(Enc, L).

input(I) :- phrase_from_file(lines(I), 'day8.txt').

part1(Ans) :- input(Ls), total_len(code_len, Ls, Code), total_len(memory_len, Ls, Mem), Ans is Code - Mem.
part2(Ans) :- input(Ls), total_len(enc_len, Ls, Enc), total_len(code_len, Ls, Code), Ans is Enc - Code.
