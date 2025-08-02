:- use_module('../safe_cracker').
:- use_module('../parser').


solution_20250713(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    apply_clue('The second digit is greater than five', Vs),
    apply_clue('Exactly one digit is odd', Vs),
    apply_clue('The first and third differ by no more than two', Vs),
    apply_clue('The fourth is greater than the second', Vs),
    apply_clue('The first and second differ by more than five', Vs),

    label(Vs).
