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

test(total_another_digit) :-
    Sentence = [the, first, and, second, total, the, third],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(first, second, add_up_to, third)).

test(total_less_than_another_digit) :-
    Sentence = [the, fourth, is, greater, than, the, sum, of, the, second, and, third],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(second, third, add_up_to_less_than, fourth)).

test(qualified_difference) :-
    Sentence = [the, fourth, is, '3', more, than, the, first],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(fourth, first, greater_than, '3')).

test(quantified_adjective1) :-
    Sentence = [exactly, '1', digit, is, square],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(square, '1')).

test(quantified_adjective2) :-
    Sentence = [only, '1', digit, is, odd],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(odd, '1')).

test(quantified_adjective3) :-
    Sentence = [exactly, '3', digits, are, even],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(even, '3')).

test(quantified_outcome1) :-
    Sentence = [exactly, '1', of, the, digits, is, '3'],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(equal, '1', 3)).

test(quantified_outcome2) :-
    Sentence = [exactly, '2', digits, are, divisible, by, '3'],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(divisible_by, '2', 3)).

test(twonary_outcome) :-
    Sentence = [the, sum, of, the, second, and, third, is, a, square],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(sum, second, third, square)).


test(normalise_numbers) :-
    normalise_numbers('hello', 'hello'),
    normalise_numbers('13', '13'),
    normalise_numbers('one', '1'),
    normalise_numbers('six', '6'),
    normalise_numbers('nine', '9'),
    !.

:- end_tests(parser).
