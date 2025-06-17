:- use_module(library(clpfd)).
:- ensure_loaded('../parser/parser.pl').

:- begin_tests(parser).





test(clue_01) :-
    Sentence = [the, second, digit, is, less, than, '7'],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(second, less_than, 7)).

test(clue_02) :-
    Sentence = [the, first, digit, is, greater, than, '5'],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(first, greater_than, 5)).

% test(some_clue) :-
%     Vars = [A, B, C, D],
%     assertion(clue_constraint(clue(first, less_than, 7), Vars, (A #< 7))),
%     assertion(clue_constraint(clue(second, less_than, 7), Vars, (B #< 7))),
%     assertion(clue_constraint(clue(third, greater_than, 2), Vars, (C #> 2))),
%     assertion(clue_constraint(clue(fourth, equal_to, 4), Vars, (D #= 4))),
%     assertion(clue_constraint(clue(first, greater_than, 5), Vars, (A #> 5))).

:- end_tests(parser).
