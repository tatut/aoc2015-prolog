:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).
:- table(evaluate/3).

wire(W) --> [C], { code_type(C, alpha) }, nonblanks(Cs), { atom_codes(W, [C|Cs]) }.
operand(num(N)) --> integer(N).
operand(wire(W)) --> wire(W).
statement(Wire-Num) --> operand(Num), " -> ", wire(Wire).
statement(Wire-binop(Operand1, Op, Operand2)) -->
    operand(Operand1), operation(Op), operand(Operand2), " -> ", wire(Wire).
statement(Wire-not(Operand)) --> "NOT ", operand(Operand), " -> ", wire(Wire).
operation(and) --> " AND ".
operation(or) --> " OR ".
operation(shift_left) --> " LSHIFT ".
operation(shift_right) --> " RSHIFT ".

program([P|Ps]) --> statement(P), eol, program(Ps).
program([]) --> eos.

input(I) :- phrase_from_file(program(I), 'day7.txt').

evaluate(_, I, I) :- integer(I).
evaluate(P, wire(W), V) :-
    member(W-Statement, P),
    evaluate(P, Statement, V).
evaluate(_, num(N), N).
evaluate(P,not(W), Val) :- evaluate(P, W, S), Val is \ S.
evaluate(P,binop(O1, Op, O2), Val) :-
    evaluate(P, O1, O1V),
    evaluate(P, O2, O2V),
    oper(O1V, Op, O2V, Val).


oper(A, and, B, Out) :- Out is A /\ B.
oper(A, or, B, Out) :- Out is A \/ B.
oper(A, shift_left, B, Out) :- Out is A << B.
oper(A, shift_right, B, Out) :- Out is A >> B.

unsigned(Signed, Unsigned) :- Unsigned is Signed /\ 0xffff.

part1(Ans) :-
    input(P),
    evaluate(P, wire(a), Signed),
    unsigned(Signed, Ans).

override_b(With, b-_, b-With).
override_b(_, Stmt, Stmt) :- \+ Stmt = b-_.

part2(Ans) :-
    input(P1),
    evaluate(P1, wire(a), A),
    maplist(override_b(A), P1, P2),
    evaluate(P2, wire(a), A1),
    unsigned(A1, Ans).
