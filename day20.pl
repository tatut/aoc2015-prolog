input(29000000).

presents(Elf, Presents) :-
    Presents = Elf * 10.

all_presents(House, Presents) :-
    divs(House, Fs),
    maplist(presents, Fs, ElfPresents),
    sum_list(ElfPresents, Presents).

house(House) :-
    all_presents(House, P),
    input(Wanted),
    P >= Wanted.

div(N, By, Out) :- Out is N div By.

divs(N, Divs) :-
    Upto is floor(sqrt(N)),
    findall(D, (between(2,Upto, D), divmod(N, D, _, 0)), D1s),
    %writeln(d1s(upto(Upto),n(N),D1s)),
    maplist(div(N), D1s, D2s),
    append(D1s, D2s, Divs1),
    append(Divs1, [1,N], Divs2),
    sort(Divs2, Divs).

part1(H) :- between(50000, inf, H), house(H).
% part1=665280

mul(N, By, Out) :- Out is N * By.

over50x(N, D) :- LastH is D*50, LastH >= N.

house2(H) :-
    divs(H, Divs),
    include(over50x(H), Divs, Included),
    maplist(mul(11), Included, Presents),
    sum_list(Presents, AllPresents),
    input(Wanted),
    AllPresents >= Wanted.


part2(H) :-
    between(666000, inf, H), house2(H).
% part2=705600
