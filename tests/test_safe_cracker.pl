:- begin_tests(safe_cracker).
:- use_module('../safe_cracker').


test(is_odd) :-
    assertion(\+ is_odd(4)),
    assertion(is_odd(7)).


test(is_square) :-
    assertion(is_square(1)),
    assertion(\+ is_square(7)),
    assertion(is_square(4)),
    assertion(\+is_square(13)),
    assertion(is_square(16)).

% test(is_prime) :-
%     assertion(is_prime(17)),
%     assertion(\+ is_prime(6)),
%     assertion(is_prime(2)),
%     assertion(\+ is_prime(1)).



:- end_tests(safe_cracker).

