:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).

reg(Reg) --> [R], {atom_codes(Reg,[R])}.

instr(inc(R)) --> "inc ", reg(R).
instr(hlf(R)) --> "hlf ", reg(R).
instr(tpl(R)) --> "tpl ", reg(R).
instr(jmp(Off)) --> "jmp ", integer(Off).
instr(jie(R,Off)) --> "jie ", reg(R), ", ", integer(Off).
instr(jio(R,Off)) --> "jio ", reg(R), ", ", integer(Off).

program([I|Is]) --> instr(I), eol, program(Is).
program([]) --> eos.

input(s(0,r(0,0),CS,Ins)) :- phrase_from_file(program(Prg), 'day23.txt'),
                             length(Prg, CS),
                             compound_name_arguments(Ins, ins, Prg).

% Register argument indexes

state(S0,S1), [S1] --> [S0].

pc(S,PC) :- arg(1, S, PC).
cs(S,CS) :- arg(3, S, CS).
ins(S,PC,Ins) :- arg(4, S, Instructions),
                 PC1 is PC + 1, % arg indexing in compound
                 arg(PC1, Instructions, Ins).

regv(R, a, V) :- arg(1, R, V).
regv(R, b, V) :- arg(2, R, V).
setregv(r(_,B), r(A1,B), a, A1).
setregv(r(A,_), r(A,B1), b, B1).

jumpmod(R,M,Off, s(PC,Reg,CS,Ins), s(PC1,Reg,CS,Ins)) :-
    regv(Reg, R, V),
    ( M is V mod 2
    -> PC1 is PC+Off
    ; PC1 is PC+1 ).

run(inc(R), s(PC,Reg,CS,Ins), s(PC1, Reg1,CS,Ins)) :-
    PC1 is PC + 1,
    regv(Reg, R, V0),
    V1 is V0 + 1,
    setregv(Reg, Reg1, R, V1).

run(jio(R,Off), s(PC,Reg,CS,Ins), s(PC1,Reg,CS,Ins)) :-
    % jio = jump if ONE (not ODD :D)
    regv(Reg, R, V),
    ( V is 1
    -> PC1 is PC + Off;
      PC1 is PC + 1 ).

run(jie(R,Off), S0, S1) :- jumpmod(R, 0, Off, S0, S1).

run(tpl(R), s(PC,Reg,CS,Ins), s(PC1,Reg1,CS,Ins)) :-
    regv(Reg, R, V),
    V1 is V * 3,
    setregv(Reg, Reg1, R, V1),
    PC1 is PC + 1.

run(jmp(Off), s(PC,Reg,CS,Ins), s(PC1,Reg,CS,Ins)) :- PC1 is PC + Off.
run(hlf(R), s(PC,Reg,CS,Ins), s(PC1,Reg1,CS,Ins)) :-
    regv(Reg, R, V),
    V1 is V div 2,
    setregv(Reg, Reg1, R, V1),
    PC1 is PC + 1.


% Reached the end, stop running
runm(_, S, S) :- pc(S, PC), cs(S, CS), PC >= CS.

runm(_, SIn, SOut) :-
    pc(SIn, PC),
    ins(SIn, PC, Ins),
    arg(2, SIn, Reg),
    %writeln(running(pc(PC),ins(Ins),reg(Reg))),
    run(Ins, SIn, S1),
    runm(_, S1, SOut).

runm(SIn, SOut) :-
    runm(SIn, SIn, SOut).

part1(Ans) :- input(R), runm(R, s(_,Reg,_,_)),
              regv(Reg,b,Ans).

% part1=255

part2(Ans) :- input(s(PC,_,CS,Ins)),
              runm(s(PC,r(1,0),CS,Ins), s(_,Reg,_,_)),
              regv(Reg, b, Ans).

% part2=334
