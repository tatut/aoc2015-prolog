:- set_prolog_flag(double_quotes, chars).

lowest_zero_hash5(Key, Num) :-
    between(0, 100000000, Num),
    format(atom(Data), '~s~w', [Key, Num]),
    md5_hash(Data, Hash, []),
    atom_chars(Hash, ['0','0','0','0','0'|_]).

lowest_zero_hash6(Key, Num) :-
    between(0, 100000000, Num),
    format(atom(Data), '~s~w', [Key, Num]),
    md5_hash(Data, Hash, []),
    atom_chars(Hash, ['0','0','0','0','0','0'|_]).
