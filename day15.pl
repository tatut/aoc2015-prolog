:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(pairs)).
:- set_prolog_flag(double_quotes, codes).

ingredient(ingredient(Name, C, D, F, T, Cal)) -->
    string_without(":", NameCs), { atom_codes(Name, NameCs) },
    ": capacity ", integer(C),
    ", durability ", integer(D),
    ", flavor ", integer(F),
    ", texture ", integer(T),
    ", calories ", integer(Cal).

ingredients([I|Is]) --> ingredient(I), eol, ingredients(Is).
ingredients([]) --> eol.


input(I) :- phrase_from_file(ingredients(I), 'day15.txt').

attr_vals(ingredient(_, Attrs), Vals) :- pairs_values(Attrs, Vals).

score(Ingredients, Spoons, Score) :-
    score(2, Ingredients, Spoons, CapacityScore),
    score(3, Ingredients, Spoons, DurabilityScore),
    score(4, Ingredients, Spoons, FlavorScore),
    score(5, Ingredients, Spoons, TextureScore),
    Score #= CapacityScore * DurabilityScore * FlavorScore * TextureScore,
    Score #> 0.

ingredient_score(Val, Spoons, Out) :- Out #= Val * Spoons.
score(Idx, Ingredients, Spoons, Score) :-
    maplist(arg(Idx), Ingredients, Vals),
    maplist(ingredient_score, Vals, Spoons, IngredientScores),
    sum(IngredientScores, #=, Score),
    Score #> 0.

best(Is, Score) :-
    length(Is, IngredientCount),
    length(IngredientSpoons, IngredientCount),
    all_distinct(IngredientSpoons),
    IngredientSpoons ins 1 .. 100,
    sum(IngredientSpoons, #=, 100),
    score(Is, IngredientSpoons, Score),
    labeling([max,bisect],IngredientSpoons),
    labeling([max(Score)], [Score]).

best_real(Is, Score) :-
    findall(Score0, best(Is,Score0), Scores),
    sort(Scores, Sorted),
    last(Sorted, Score).

% best 500 calorie recipe
best500(Is, Score) :-
    length(Is, IngredientCount),
    length(IngredientSpoons, IngredientCount),
    all_distinct(IngredientSpoons),
    IngredientSpoons ins 1 .. 100,
    sum(IngredientSpoons, #=, 100),
    score(6, Is, IngredientSpoons, CalorieScore),
    CalorieScore #= 500,
    score(Is, IngredientSpoons, Score),
    labeling([max,bisect],IngredientSpoons),
    labeling([max(Score)], [Score]).

best500_real(Is, Score) :-
    findall(Score0, best500(Is,Score0), Scores),
    sort(Scores, Sorted),
    last(Sorted, Score).
