:- use_module('../safe_cracker').
:- use_module('../parser').


solution_20250810(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    apply_clue('The third digit is odd', Vs),
    apply_clue('The first and fourth differ by two', Vs),
    apply_clue('Exactly one digit is square', Vs),
    apply_clue('The sum of the second and third is a two-digit prime', Vs),
    apply_clue('The sum of the first and second is less than seven', Vs),

    label(Vs).
