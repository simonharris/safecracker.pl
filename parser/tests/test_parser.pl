:- use_module(library(clpfd)).
:- use_module(library(plunit_assert)).
%:- use_module(plunit_assert).
:- ensure_loaded('../parser').


:- begin_tests(preprocessing).

test(text_preprocessing0) :-
    Sentence = 'Exactly one of the third and fourth is odd',
    Expected = [exactly, 1, of, the, third, and, fourth, is, odd],
    assert_output(text_preprocessed(Sentence, Parsed), [Parsed], [Expected]).

test(text_preprocessing1) :-
    Sentence = 'The sum of the second and fourth is divisible by five',
    Expected = [the, sum, of, the, second, and, fourth, is, divisible, by, 5],
    assert_output(text_preprocessed(Sentence, Parsed), [Parsed], [Expected]).

test(text_preprocessing2) :-
    Sentence = 'The first and third total 13',
    Expected = [the, first, and, third, total, 13],
    assert_output(text_preprocessed(Sentence, Parsed), [Parsed], [Expected]).

test(text_preprocessing3) :-
    Sentence = 'The first and second total the third',
    Expected = [the, first, and, second, total, the, third],
    assert_output(text_preprocessed(Sentence, Parsed), [Parsed], [Expected]).

test(text_preprocessing4) :-
    Sentence = 'The first two digits differ by four',
    Expected = [the, first, 2, digits, differ, by, 4],
    assert_output(text_preprocessed(Sentence, Parsed), [Parsed], [Expected]).

test(text_preprocessing4) :-
    Sentence = 'The fourth is greater than the sum of the second and third',
    Expected = [the, fourth, is, greater, than, the, sum, of, the, second, and, third],
    assert_output(text_preprocessed(Sentence, Parsed), [Parsed], [Expected]).

:- end_tests(preprocessing).


:- begin_tests(parser).

test(clue_01) :-
    Sentence = [the, second, digit, is, less, than, 7],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(second, less_than, 7)).

test(clue_02) :-
    Sentence = [the, first, digit, is, greater, than, 5],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(first, greater_than, 5)).

test(adjective) :-
    Sentence = [the, fourth, digit, is, odd],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(fourth, odd)).
test(adjective_greatest) :-
    Sentence = [the, third, digit, is, the, greatest],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(third, greatest)).
test(adjective_square) :-
    Sentence = [the, third, digit, is, square],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(third, square)).

test(difference1) :-
    Sentence = [the, third, and, fourth, differ, by, 2],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(third, fourth, differ_by, 2)).
test(difference3) :-
    Sentence = [the, first, and, last, digits, differ, by, 3],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(first, fourth, differ_by, 3)).
test(difference_first_two) :-
    Sentence = [the, first, 2, digits, differ, by, 4],
    atoms_clue(Sentence, Clue),
    assert_type(Clue, clue),
    assert_equals(Clue, clue(first, second, differ_by, 4)).

test(difference_qualified) :-
    Sentence = [the, first, and, second, differ, by, more, than, 4],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(first, second, differ_by_more_than, 4)).
test(difference_qualified2) :-
    Sentence = [the, first, and, second, differ, by, no, more, than, 4],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(first, second, differ_by_no_more_than, 4)).

test(add_up_to) :-
    Sentence = [the, second, and, third, digits, total, 11],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(second, third, add_up_to, 11)).
test(add_up_to_2) :-
    Sentence = [the, first, and, third, total, 13],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(first, third, add_up_to, 13)).

test(twice) :-
    Sentence = [the, second, is, twice, the, fourth],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(second, twice, fourth)).

test(total_another_digit) :-
    Sentence = [the, first, and, second, total, the, third],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(first, second, add_up_to, third)).

test(total_less_than_another_digit) :-
    Sentence = [the, fourth, is, greater, than, the, sum, of, the, second, and, third],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(sum, less_than, second, third, fourth)).
test(total_less_than_another_digit2) :-
     Sentence = [the, sum, of, the, first, and, second, is, less, than, the, third],
     atoms_clue(Sentence, Clue),
     assert_equals(Clue, clue(sum, less_than, first, second, third)).
test(minus_less_than) :-
    Sentence = [the, second, minus, the, first, is, less, than, 3],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(minus, less_than, second, first, 3)).

test(qualified_difference) :-
    Sentence = [the, fourth, is, 3, more, than, the, first],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(fourth, first, greater_than, 3)).
test(qualified_difference) :-
    Sentence = [the, second, is, 3, greater, than, the, first],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(second, first, greater_than, 3)).

test(quantified_adjective1) :-
    Sentence = [exactly, 1, digit, is, square],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(square, 1)).
test(quantified_adjective2) :-
    Sentence = [only, 1, digit, is, odd],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(odd, 1)).
test(quantified_adjective3) :-
    Sentence = [exactly, 3, digits, are, even],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(even, 3)).
test(quantified_adjective4) :-
    Sentence = [exactly, 2, digits, are, not, prime],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(not_prime, 2)).

test(quantified_outcome1) :-
    Sentence = [exactly, 1, of, the, digits, is, 3],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(equal, 1, 3)).

test(quantified_outcome2) :-
    Sentence = [exactly, 2, digits, are, divisible, by, 3],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(divisible_by, 2, 3)).

test(twonary_outcome_square) :-
    Sentence = [the, sum, of, the, second, and, third, is, a, square],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(sum, second, third, square)).
test(twonary_outcome_square2) :-
    Sentence = [the, sum, of, the, first, and, fourth, is, square],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(sum, first, fourth, square)).
test(twonary_outcome_prime) :-
    Sentence = [the, sum, of, the, first, and, fourth, is, prime],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(sum, first, fourth, prime)).
test(twonary_outcome_prime_twodigits) :-
    Sentence = [the, sum, of, the, second, and, third, is, a, 'two-digit', prime],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(sum, second, third, two_digit_prime)).

test(sum_of_exceeds) :-
    Sentence = [the, sum, of, the, first, and, third, exceeds, 10],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(sum, greater_than, first, third, 10)).
test(sum_of_gt) :-
    Sentence = [the, sum, of, the, first, and, third, is, greater, than, 13],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(sum, greater_than, first, third, 13)).
test(sum_of_lt) :-
    Sentence = [the, sum, of, the, first, and, second, is, less, than, 7],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(sum, less_than, first, second, 7)).
test(sum_of_divisible) :-
    Sentence = [the, sum, of, the, second, and, fourth, is, divisible, by, 5],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(sum, db, second, fourth, 5)).

test(either_odd) :-
    Sentence = [either, the, second, or, the, third, is, odd, but, not, both],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(either, second, third, odd)).
test(either_odd2) :-
    Sentence = [exactly, 1, of, the, second, and, third, is, odd],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(either, second, third, odd)).
test(either_odd2) :-
    Sentence = [exactly, 1, of, the, third, and, fourth, is, odd],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(either, third, fourth, odd)).

test(exceeds_more_than) :-
    Sentence = [the, second, exceeds, the, first, by, more, than, 2],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(second, first, exceeds_by_more_than, 2)).

test(this_one_fails) :-
    Sentence = [the, first, digit, is, divisible, by, 3],
    atoms_clue(Sentence, Clue),
    assert_equals(Clue, clue(first, divisible_by, 3)).

test(normalise_numbers) :-
    normalise_numbers('hello', 'hello'),
    normalise_numbers('one', 1),
    normalise_numbers('six', 6),
    normalise_numbers('nine', 9),
    !.

:- end_tests(parser).


:- begin_tests(constraint_factories).

% These are sparse: prior to plunit_assert is was very difficult to test these

% eg. 'The fourth is three more than the first' - function constraint
test(clue_constraint_gt) :-
    Clue = clue(fourth, first, greater_than, 3),
    once(clue_constraint(Clue, [A, _, _, D], Constraint)),
    assert_equals(Constraint, (D - A) #= 3).

test(clue_constraint_odd) :-
    Clue = clue(third, odd),
    once(clue_constraint(Clue, [_, _, C, _], Constraint)),
    assert_equals(Constraint, is_odd(C)).

test(clue_constraint_total_number) :-
    Clue = clue(first, third, add_up_to, 13),
    once(clue_constraint(Clue, [A, _, C, _], Constraint)),
    assert_equals(Constraint, (A + C) #= 13).

test(clue_constraint_total_other) :-
    Clue = clue(first, second, add_up_to, third),
    once(clue_constraint(Clue, [A, B, C, _], Constraint)),
    assert_equals(Constraint, (A + B) #= C).

test(clue_constraint_square) :-
    Clue = clue(third, square),
    Vs = [_, _, C, _],
    once(clue_constraint(Clue, Vs, Constraint)),
    assert_equals(Constraint, is_square(C)).

test(clue_constraint_one_odd) :-
    Clue = clue(either, second, third, odd),
    Vs = [_, B, C, _],
    once(clue_constraint(Clue, Vs, Constraint)),
    assert_equals(Constraint, xor((B mod 2) #=1, (C mod 2) #= 1)).

test(clue_constraint_differ_by) :-
    Clue = clue(first, second, differ_by, 4),
    Vs = [A, B, _, _],
    once(clue_constraint(Clue, Vs, Constraint)),
    assert_equals(Constraint, abs(A-B) #= 4).

test(clue_constraint_sum_lt_col) :-
    Clue = clue(sum, less_than, second, third, fourth),
    Vs = [_, B, C, D],
    once(clue_constraint(Clue, Vs, Constraint)),
    writeln(Constraint),
    assert_equals(Constraint, (B+C) #< D).

:- end_tests(constraint_factories).
