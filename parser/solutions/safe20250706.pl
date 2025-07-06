:- use_module('../safe_cracker').
:- use_module('../parser').


solution_20250706(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    apply_clue('The third digit is even', Vs),
    apply_clue('The sum of the second and fourth is greater than 12', Vs),
    apply_clue('Exactly two digits are odd', Vs),
    apply_clue('The sum of the first and second is less than the third', Vs),
    % apply_clue('The second minus the first is less than three', Vs),

    label(Vs).
