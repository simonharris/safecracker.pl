:- ensure_loaded('../solutions/safe20250413').
:- ensure_loaded('../solutions/safe20250504').
:- ensure_loaded('../solutions/safe20250511').
:- ensure_loaded('../solutions/safe20250525').
:- ensure_loaded('../solutions/safe20250601').
:- ensure_loaded('../solutions/safe20250615').
:- ensure_loaded('../solutions/safe20250622').
:- ensure_loaded('../solutions/safe20250629').
:- ensure_loaded('../solutions/safe20250706').


:- begin_tests(safe_cracker_solutions).

test(solution_20250413) :-
    once(solution_20250413(A, B, C, D)),
    assertion((A = 3, B = 4, C = 1, D = 6)).

test(solution_20250504) :-
    once(solution_20250504(A, B, C, D)),
    assertion((A = 5, B = 8, C = 2, D = 4)).

test(solution_20250511) :-
    once(solution_20250511(A, B, C, D)),
    assertion((A = 9, B = 1, C = 4, D = 6)).

test(solution_20250525) :-
    once(solution_20250525(A, B, C, D)),
    assertion((A = 5, B = 1, C = 6, D = 7)).

test(solution_20250601) :-
    once(solution_20250601(A, B, C, D)),
    assertion((A = 6, B = 4, C = 5, D = 2)).

test(solution_20250615) :-
    once(solution_20250615(A, B, C, D)),
    assertion((A = 7, B = 8, C = 4, D = 6)).

test(solution_20250622) :-
    once(solution_20250622(A, B, C, D)),
    assertion((A = 5, B = 1, C = 7, D = 4)).

test(solution_20250629) :-
    once(solution_20250629(A, B, C, D)),
    assertion((A = 6, B = 2, C = 9, D = 3)).

test(solution_20250706) :-
    once(solution_20250706(A, B, C, D)),
    assertion((A = 3, B = 4, C = 8, D = 9)).

:- end_tests(safe_cracker_solutions).
