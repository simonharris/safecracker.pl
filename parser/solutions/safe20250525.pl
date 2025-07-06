:- use_module('../safe_cracker').
:- use_module('../parser').


solution_20250525(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    % 1. The fourth digit is odd
    % is_odd(D),
    apply_clue('The fourth digit is odd', Vs),

    % 2. The first and second total the third
    % (A + B) #= C,
    apply_clue('The first and second total the third', Vs),

    % 3. Exactly one digit is square
    % include(is_square, Vs, [_]),
    apply_clue('Exactly one digit is square', Vs),

    % 4. The fourth is two more than the first
    % D #= A + 2,
    apply_clue('The fourth is two more than the first', Vs),

    % 5. The first and third differ by one
    %abs(A - C) #= 1,
    apply_clue('The first and third differ by one', Vs),

    label(Vs).
