% :- use_module(library(plunit_assert)).
:- use_module('plunit_assert').
:- use_module('../safe_cracker').


:- begin_tests(safe_cracker).

test(xor) :-
    assert_true(xor(1, 0)),
    assert_true(xor(0, 1)),
    assert_false(xor(1, 1)),
    assert_false(xor(0, 0)).

test(is_prime) :-
    assert_true(is_prime(17)),
    assert_false(is_prime(6)),
    assert_true(is_prime(2)),
    assert_true(is_prime(2 + 1)),
    assert_false(is_prime(1)),
    !.

test(is_square) :-
    assert_true(is_square(1)),
    assert_false(is_square(7)),
    assert_true(is_square(4)),
    assert_false(is_square(13)),
    assert_true(is_square(16)).

test(is_odd) :-
    assert_false(is_odd(0)),
    assert_false(is_odd(4)),
    assert_true(is_odd(7)).

test(is_even) :-
    assert_true(is_even(0)),
    assert_false(is_even(77)),
    assert_true(is_even(44)).

test(divides_by) :-
    assert_false(divides_by(15, 2)),
    assert_true(divides_by(15, 5)),
    assert_true(divides_by(4, 2)).

test(occurrenceof0) :- assert_output(occurrenceof([], 1, HowMany1), [HowMany1], [0]).
test(occurrenceof1) :- assert_output(occurrenceof([2, 3, 4, 5], 1, HowMany2), [HowMany2], [0]).
test(occurrenceof2) :- assert_output(occurrenceof([1, 2, 3, 4], 1, HowMany3), [HowMany3], [1]).
test(occurrenceof3) :- assert_output(occurrenceof([1, 2, 3, 4, 1, 1], 1, HowMany4), [HowMany4], [3]).


:- end_tests(safe_cracker).

