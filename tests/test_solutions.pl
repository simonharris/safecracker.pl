:- begin_tests(safe_cracker_solutions).
:- consult('../solutions/safe20250413').
:- consult('../solutions/safe20250504').
:- consult('../solutions/safe20250511').
:- consult('../solutions/safe20250525').


test(solution_20250504) :-
    once(solution_20250504(A, B, C, D)),
    assertion((A = 5, B = 8, C = 2, D = 4)).


:- end_tests(safe_cracker_solutions).
