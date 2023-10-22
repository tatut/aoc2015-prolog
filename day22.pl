% Spell mana cost and effects
spell(magic_missile, 53, magic_missile). % instant 4 damage
spell(drain, 73, drain). % instant 2 dmg and 2 heal
spell(shield, 113, fx(shield, 6, 7)). % increase armor by 7 for 6 rounds
spell(poison, 173, fx(damage, 6, 3)). % 3 dmg at start of round for 6 rounds
spell(recharge, 229, fx(mana, 5, 101)). % +101 mana at start of round for 5 rounds

input(55, 8). % boss hp and damage

% game state is a dict
% s{hp: PlayerHp, mana: PlayerMana, armor: PlayerArmor,
%   boss_hp: BossHp, boss_dmg: BossDmg,
%   mana_spent: CumulativeManaSpent,
%   effects: [Effect1...EffectN]}.

effect_active(fx(Name,_,_), Effects) :-
    member(fx(Name,_,_), Effects).

% Check if we can cast a spell, we must have enough mana for it
% and effect of that name must not be active
can_cast(S, Spell) :-
    _{mana: Mana, effects: Effects} :< S,
    spell(Spell, ManaCost, Effect),
    ManaCost =< Mana,
    \+ effect_active(Effect, Effects).

% Cast a spell, decreases mana and adds effect
cast(Spell, SIn, SIn.put(mana, Mana1).put(mana_spent, MSpent1).put(effects, [Effect|Effects])) :-
    _{effects: Effects, mana: Mana, mana_spent: MSpent} :< SIn,
    spell(Spell, ManaCost, Effect),
    fx(_, _,_) = Effect, % don't include instant spells
    MSpent1 is MSpent + ManaCost,
    Mana1 is Mana - ManaCost.

% Handle instant spells
cast(magic_missile, SIn, SIn.put(mana, Mana1).put(mana_spent, MSpent1).put(boss_hp, Hp1)) :-
    spell(magic_missile, ManaCost, _),
    _{boss_hp: Hp, mana: Mana, mana_spent: MSpent} :< SIn,
    MSpent1 is MSpent + ManaCost,
    Mana1 is Mana - ManaCost,
    Hp1 is Hp - 4.

cast(drain, SIn, SIn.put(mana, Mana1).put(mana_spent,MSpent1).put(boss_hp, BHp1).put(hp, PHp1)) :-
    spell(drain, ManaCost, _),
    _{boss_hp: BHp, mana: Mana, mana_spent: MSpent, hp: PHp} :< SIn,
    MSpent1 is MSpent + ManaCost,
    Mana1 is Mana - ManaCost,
    PHp1 is PHp + 2,
    BHp1 is BHp - 2.

try_cast(SIn, SOut) :-
    spell(Spell, _,_),
    can_cast(SIn, Spell),
    cast(Spell, SIn, SOut).

round_init(SIn, SIn.put(armor, 0)). % clear armor on start of round

% process effects (SIn, fx(...), SOut)
process_effect(fx(shield, _, A), SIn, SIn.put(armor,A)).
process_effect(fx(damage, _, D), SIn, SIn.put(boss_hp, Hp1)) :-
    _{boss_hp: Hp} :< SIn,
    Hp1 is Hp - D.
process_effect(fx(mana, _, M), SIn, SIn.put(mana, M1)) :-
    _{mana: Mana} :< SIn,
    M1 is Mana + M.

process_effects(SIn, SOut) :-
    _{effects: Effects} :< SIn,
    foldl(process_effect, Effects, SIn, SOut).

decrease_counters(SIn, SIn.put(effects, NewEffects)) :-
    _{effects: OldEffects} :< SIn,
    convlist(decrease_counter, OldEffects, NewEffects).

% Decrease counters that are above 1 (fails for 1 counters, which get removed)
decrease_counter(fx(T, R, A), fx(T, R1, A)) :- R > 1, R1 is R - 1.

initial_state(s{hp: 50, mana: 500, mana_spent: 0, armor: 0,
                boss_hp: BossHp, boss_dmg: BossDmg,
                effects: []}) :- input(BossHp, BossDmg).

% A Clojure like pipe for passing intermediate values
pipe(Value, [], Value).
pipe(ValueIn, [G|Goals], ValueOut) :-
    call(G, ValueIn, Value0),
    pipe(Value0, Goals, ValueOut).

difficulty(S, S.put(hp, Hp1)) :-
    _{hp: Hp} :< S,
    nb_getval(player_hp_dec, HpDec),
    Hp1 is Hp - HpDec.

player_round(SIn, SOut) :-
    pipe(SIn, [round_init, difficulty], S1),
    ( lose(S1) -> fail ;
      (pipe(S1,
            [round_init,        % reset player armor
             process_effects,   % process any lingering effects
             decrease_counters, % decrease effect counters and remove 0
             try_cast], SOut)) ).  % try to cast any spells

boss_round(SIn, SOut) :-
    pipe(SIn,
         [process_effects, decrease_counters, boss_hit],
         SOut).

boss_hit(SIn, SIn.put(hp, Hp1)) :-
    _{hp: Hp, armor: A, boss_dmg: D} :< SIn,
    Hp1 is Hp - max(1, D - A).

report_win(SIn, SIn) :-
    (_{boss_hp: Hp, mana_spent: Spent} :< SIn,
     Hp =< 0,
     writeln(win_with_mana_spent(Spent))); true.

win_mana_spent(SIn, ManaSpent) :-
    _{boss_hp: Hp, mana_spent: ManaSpent} :< SIn,
    Hp =< 0.

lose(SIn) :-
    _{hp: Hp, mana_spent: Ms} :< SIn,
    nb_getval(current_min_mana, CurrentMin),
    % We lost this one if we are dead or mana spent is above
    % the previously found minimum.
    (Hp =< 0; Ms >= CurrentMin).

round(SIn, SOut) :- pipe(SIn, [player_round, boss_round], SOut).

play_to_win(ManaSpent) :-
    initial_state(S),
    play_to_win(S, ManaSpent).

play_to_win(S0, ManaSpent) :-
    round(S0, S1),
    ( lose(S1) -> fail;
      (win_mana_spent(S1, ManaSpent) ; play_to_win(S1, ManaSpent)) ).

part(CurrentMin, MinManaSpent) :-
    % Get any 1 solution first and use that to short circuit
    % successive calls
    nb_setval(current_min_mana, CurrentMin),
    play_to_win(Ms),
    ( part(Ms, MinManaSpent) ; MinManaSpent = Ms).

part1(Min) :-
    nb_setval(player_hp_dec, 0),
    once(part(10000, Min)).
% part1=953

% Part2
part2(Min) :-
    nb_setval(player_hp_dec, 1),
    once(part(10000, Min)).
% part2=1289
