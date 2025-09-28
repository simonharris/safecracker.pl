:- use_module('../safe_cracker').
:- use_module('../parser').


solution_20250928(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    apply_clue('The first digit is greater than five', Vs),
    apply_clue('The third is less than the second', Vs),
    apply_clue('Only one digit is odd', Vs),
    apply_clue('The first and third differ by one', Vs),
    apply_clue('The third is two more than the fourth', Vs),

    label(Vs).
