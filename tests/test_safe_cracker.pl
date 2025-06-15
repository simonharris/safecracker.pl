:- begin_tests(safe_cracker).
:- use_module('../safe_cracker').

test(xor) :-
    assertion(xor(1, 0)),
    assertion(xor(0, 1)),
    assertion(\+ xor(1, 1)),
    assertion(\+ xor(0, 0)).

% test(is_prime) :-
%     assertion(is_prime(17)),
%     assertion(\+ is_prime(6)),
%     assertion(is_prime(2)),
%     assertion(\+ is_prime(1)).

test(is_square) :-
    assertion(is_square(1)),
    assertion(\+ is_square(7)),
    assertion(is_square(4)),
    assertion(\+is_square(13)),
    assertion(is_square(16)).

test(is_odd) :-
    assertion(\+ is_odd(0)),
    assertion(\+ is_odd(4)),
    assertion(is_odd(7)).

test(is_even) :-
    assertion(is_even(0)),
    assertion(\+ is_even(77)),
    assertion(is_even(44)).

test(divides_by) :-
    assertion(\+ divides_by(15, 2)),
    assertion(divides_by(15, 5)),
    assertion(divides_by(4, 2)).

test(occurrenceof) :-
    assertion(occurrenceof([], 1, 0)),
    assertion(occurrenceof([2, 3, 4, 5], 1, 0)),
    assertion(occurrenceof([1, 2, 3, 4], 1, 1)),
    assertion(occurrenceof([1, 2, 3, 4, 1, 1], 1, 3)).

:- end_tests(safe_cracker).

