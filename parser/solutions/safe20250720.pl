:- use_module('../safe_cracker').
:- use_module('../parser').


solution_20250720(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    apply_clue('The sum of the first and fourth is square', Vs),
    apply_clue('The third digit is the greatest', Vs),
    % apply_clue('The first two digits differ by four', Vs),
    % apply_clue('Exactly one of the second and third is odd', Vs),
    apply_clue('The first is three less than the last', Vs),

    label(Vs).
