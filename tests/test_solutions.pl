:- begin_tests(safe_cracker_solutions).
:- consult('../solutions/safe20250413').
:- consult('../solutions/safe20250504').
:- consult('../solutions/safe20250511').
:- consult('../solutions/safe20250525').


test(solution_20250412) :-
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
        assertion((A = 3, B = 1, C = 4, D = 5)).


:- end_tests(safe_cracker_solutions).
