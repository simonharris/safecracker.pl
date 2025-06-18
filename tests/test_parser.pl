:- use_module(library(clpfd)).
:- ensure_loaded('../parser/parser').

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


test(adjectives) :-
    Sentence = [the, fourth, digit, is, odd],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(fourth, odd)).

test(difference1) :-
    Sentence = [the, third, and, fourth, differ, by, '2'],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(third, fourth, differ_by, '2')).

test(difference2) :-
    Sentence = [the, first, and, third, differ, by, '3'],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(first, third, differ_by, '3')).

test(twice) :-
    Sentence = [the, second, is, twice, the, fourth],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(second, twice, fourth)).

test(qualified_difference) :-
    Sentence = [the, fourth, is, '3', more, than, the, first],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(fourth, first, greater_than, '3')).

test(normalise_numbers) :-
    normalise_numbers('hello', 'hello'),
    normalise_numbers('13', '13'),
    normalise_numbers('one', '1'),
    normalise_numbers('six', '6'),
    normalise_numbers('nine', '9'),
    !.

:- end_tests(parser).
