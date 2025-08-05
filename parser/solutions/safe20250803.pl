:- use_module('../safe_cracker').
:- use_module('../parser').


solution_20250803(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    apply_clue('The fourth digit is the greatest', Vs),
    apply_clue('The first and third differ by four', Vs),
    apply_clue('The sum of the second and fourth is divisible by five', Vs),
    apply_clue('Exactly one of the third and fourth is odd', Vs),
    % apply_clue('The second is three greater than the fourth', Vs),

    label(Vs).
