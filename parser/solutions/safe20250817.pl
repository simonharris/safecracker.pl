:- use_module('../safe_cracker').
:- use_module('../parser').


solution_20250817(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    apply_clue('The third digit is square', Vs),
    apply_clue('The first and fourth total 11', Vs),
    apply_clue('The sum of the first and second is greater than 13', Vs),
    apply_clue('The second is less than the third', Vs),
    apply_clue('Exactly two digits are prime', Vs),

    label(Vs).
