:- use_module('../safe_cracker').
:- use_module('../parser').


solution_20250831(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    apply_clue('The sum of the first and third is less than nine', Vs),
    apply_clue('Only one digit is odd', Vs),
    apply_clue('The sum of the first and second is 12', Vs),
    apply_clue('The first and third differ by less than three', Vs),
    apply_clue('The second and fourth total 10', Vs),

    label(Vs).
