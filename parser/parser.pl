:- module(parser, [
    apply/2,
    parse_clue/2
]).
:- use_module(library(clpfd)).
:- use_module('grammar').


apply(Text, Vs) :-
    parse_text(Text, Vs, Constraint),
    call(Constraint).

parse_text(Text, Vars, Constraint) :-
    split_string(Text, " ", "", TextList),
    maplist(atom_string, Atoms, TextList),
    maplist(downcase_atom, Atoms, AtomsLower),
    parse_clue(AtomsLower, Clue),
    clue_constraint(Clue, Vars, Constraint).

parse_clue(Sentence, Clue) :-
    phrase(clue(Clue), Sentence).

clue_constraint(clue(Position, less_than, Num), Vars, Constraint) :-
    safe_digit_val(Num),
    position_index(Position, Index),
    nth1(Index, Vars, Var),
    Constraint = (Var #< Num).
clue_constraint(clue(Position, greater_than, Num), Vars, Constraint) :-
    safe_digit_val(Num),
    position_index(Position, Index),
    nth1(Index, Vars, Var),
    Constraint = (Var #> Num).
clue_constraint(clue(Position1, less_than, Position2), Vars, Constraint) :-
    position_val(Position2),
    position_index(Position1, Index1),
    position_index(Position2, Index2),
    nth1(Index1, Vars, Var1),
    nth1(Index2, Vars, Var2),
    Constraint = (Var1 #< Var2).
clue_constraint(clue(Position1, greater_than, Position2), Vars, Constraint) :-
    position_val(Position2),
    position_index(Position1, Index1),
    position_index(Position2, Index2),
    nth1(Index1, Vars, Var1),
    nth1(Index2, Vars, Var2),
    Constraint = (Var1 #> Var2).

position_index(first, 1).
position_index(second, 2).
position_index(third, 3).
position_index(fourth, 4).

position_val(Token) :-
    phrase(ord(Token), [_]).

safe_digit_val(Num) :-
    phrase(safe_digit(Num), [_]).
