% represent a combatant as c(Name,Hp,Damage,Armor).
input(c(boss, 100, 8, 2)).

hit(Dmg, Hp, Armor, HpOut) :- HpOut is Hp - max(1, (Dmg - Armor)).

% battle until one of them dies, unifies Winner with the
% the name of the combatant who won

% If 1st hits and 2nd dies, winner is 1st
battle(c(N1,_,D1,_), c(_,Hp2,_,A2), N1) :-
    hit(D1, Hp2, A2, Hp2Out), Hp2Out =< 0.

% If 1st hits, 2nd stays alive and 1st dies, winner is 2nd
battle(c(_,Hp1,D1,A1), c(N2,Hp2,D2,A2), N2) :-
    hit(D1, Hp2, A2, Hp2Out), Hp2Out > 0,
    hit(D2, Hp1, A1, Hp1Out), Hp1Out =< 0.

% If both survive hits, go to next round
battle(c(N1,Hp1,D1,A1), c(N2,Hp2,D2,A2), W) :-
    hit(D1, Hp2, A2, Hp2Out), Hp2Out > 0,
    hit(D2, Hp1, A1, Hp1Out), Hp1Out > 0,
    battle(c(N1,Hp1Out,D1,A1), c(N2,Hp2Out,D2,A2), W).

% Facts about the equipment
% Weapons:       Cost Dmg
weapon(dagger,      8, 4).
weapon(shortsword, 10, 5).
weapon(warhammer,  25, 6).
weapon(longsword,  40, 7).
weapon(greataxe,   74, 8).

% Armor:        Cost  Armor
armor(leather,    13, 1).
armor(chainmail,  31, 2).
armor(splintmail, 53, 3).
armor(bandedmail, 75, 4).
armor(platemail, 102, 5).
armor(none,        0, 0). % added because armor is optional

% Rings:      Cost Dmg Armor
ring(damage_1,  25, 1, 0).
ring(damage_2,  50, 2, 0).
ring(damage_3, 100, 3, 0).
ring(defense_1, 20, 0, 1).
ring(defense_2, 40, 0, 2).
ring(defense_3, 80, 0, 3).
ring(none1,      0, 0, 0). % added because optional
ring(none2,      0, 0, 0). % added because optional

% Buy one configuration of items, returns total cost, dmg and armor
buy(Cost, DmgOut, ArmorOut) :-
    weapon(_, WCost, Dmg),
    armor(_, ACost, Armor),
    ring(R1, R1Cost, R1Dmg, R1Armor),
    ring(R2, R2Cost, R2Dmg, R2Armor),
    \+ R1 = R2,
    Cost is WCost + ACost + R1Cost + R2Cost,
    DmgOut is Dmg + R1Dmg + R2Dmg,
    ArmorOut is Armor + R1Armor + R2Armor.

% play for winner
play(Cost, W) :-
    buy(Cost, Dmg, Armor),
    input(Boss),
    battle(c(player, 100, Dmg, Armor), Boss, W).

part1(MinCost) :- aggregate(min(C), play(C,player), MinCost).
% part1=91

part2(MaxCost) :- aggregate(max(C), play(C,boss), MaxCost).
% part2 = 158
