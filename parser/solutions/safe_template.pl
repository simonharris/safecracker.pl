:- use_module('../safe_cracker').
:- use_module('../parser').


solution_20250615(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    % apply_clue('', Vs),
    % apply_clue('', Vs),
    % apply_clue('', Vs),
    % apply_clue('', Vs),
    % apply_clue('', Vs),

    label(Vs).
