:- use_module(library(clpfd)).
:- ensure_loaded('../parser').

:- begin_tests(parser).


test(clue_01) :-
    Sentence = [the, second, digit, is, less, than, '7'],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(second, less_than, 7)).

test(clue_02) :-
    Sentence = [the, first, digit, is, greater, than, '5'],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(first, greater_than, 5)).

test(adjective) :-
    Sentence = [the, fourth, digit, is, odd],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(fourth, odd)).
test(adjective_greatest) :-
    Sentence = [the, third, digit, is, the, greatest],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(third, greatest)).

test(difference1) :-
    Sentence = [the, third, and, fourth, differ, by, '2'],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(third, fourth, differ_by, '2')).
test(difference3) :-
    Sentence = [the, first, and, last, digits, differ, by, '3'],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(first, fourth, differ_by, '3')).
test(difference_first_two) :-
    Sentence = [the, first, two, digits, differ, by, '4'],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(first, second, differ_by, '4')).

test(difference_qualified) :-
    Sentence = [the, first, and, second, differ, by, more, than, '4'],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(first, second, differ_by_more_than, '4')).

test(difference_qualified2) :-
    Sentence = [the, first, and, second, differ, by, no, more, than, '4'],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(first, second, differ_by_no_more_than, '4')).

test(add_up_to) :-
    Sentence = [the, second, and, third, digits, total, '11'],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(second, third, add_up_to, '11')).

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
    assertion(Clue = clue(sum, lt, second, third, fourth)).
test(total_less_than_another_digit2) :-
     Sentence = [the, sum, of, the, first, and, second, is, less, than, the, third],
     parse_clue(Sentence, Clue),
     assertion(Clue = clue(sum, lt, first, second, third)).
test(minus_less_than) :-
    Sentence = [the, second, minus, the, first, is, less, than, '3'],
    Sentence = [the, second, minus, the, first, is, less, than, '3'],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(minus, lt, second, first, '3')).


test(qualified_difference) :-
    Sentence = [the, fourth, is, '3', more, than, the, first],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(fourth, first, greater_than, '3')).
test(qualified_difference) :-
    Sentence = [the, second, is, '3', greater, than, the, fourth],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(second, fourth, greater_than, '3')).

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

test(quantified_adjective4) :-
    Sentence = [exactly, '2', digits, are, not, prime],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(not_prime, '2')).

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
test(twonary_outcome2) :-
    Sentence = [the, sum, of, the, first, and, fourth, is, square],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(sum, first, fourth, square)).

test(sum_of_exceeds) :-
    Sentence = [the, sum, of, the, first, and, third, exceeds, 10],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(sum, gt, first, third, 10)).
test(sum_of_gt) :-
    Sentence = [the, sum, of, the, first, and, third, is, greater, than, 13],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(sum, gt, first, third, 13)).
test(sum_of_divisible) :-
    Sentence = [the, sum, of, the, second, and, fourth, is, divisible, by, 5],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(sum, db, second, fourth, 5)).

test(either_odd) :-
    Sentence = [either, the, second, or, the, third, is, odd, but, not, both],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(either, second, third, odd)).
test(either_odd2) :-
    Sentence = [exactly, one, of, the, second, and, third, is, odd],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(either, second, third, odd)).

test(exceeds_more_than) :-
    Sentence = [the, second, exceeds, the, first, by, more, than, '2'],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(second, first, exceeds_by_more_than, '2')).

test(this_one_fails) :-
    Sentence = [the, first, digit, is, divisible, by, '3'],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(first, divisible_by, 3)).


test(normalise_numbers) :-
    normalise_numbers('hello', 'hello'),
    normalise_numbers('13', '13'),
    normalise_numbers('one', '1'),
    normalise_numbers('six', '6'),
    normalise_numbers('nine', '9'),
    !.

:- end_tests(parser).
