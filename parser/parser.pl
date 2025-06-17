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

clue_constraint(clue(Position, Relation, Num), Vars, Constraint) :-
    safe_digit_val(Num),
    position_index(Position, Index),
    nth1(Index, Vars, Var),
    relation_constraint(Relation, Var, Num, Constraint).
clue_constraint(clue(Position1, Relation, Position2), Vars, Constraint) :-
    position_val(Position2),
    position_index(Position1, Index1),
    position_index(Position2, Index2),
    nth1(Index1, Vars, Var1),
    nth1(Index2, Vars, Var2),
    relation_constraint(Relation, Var1, Var2, Constraint).

relation_constraint(less_than, A, B, A #< B).
relation_constraint(greater_than, A, B, A #> B).

position_index(first, 1).
position_index(second, 2).
position_index(third, 3).
position_index(fourth, 4).

position_val(Token) :-
    phrase(ord(Token), [_]).

safe_digit_val(Num) :-
    phrase(safe_digit(Num), [_]).
