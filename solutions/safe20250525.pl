:- use_module('../safe_cracker').
:- use_module('../parser/parser').


solution_20250525(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    % 1. The fourth digit is odd
    % is_odd(D),
    apply('The fourth digit is odd', Vs),

    % 2. The first and second total the third
    (A + B) #= C,

    % 3. Exactly one digit is square
    include(is_square, Vs, [_]),

    % 4. The fourth is 2 more than the first
    D #= A + 2,

    % 5. The first and third differ by 1
    abs(A - C) #= 1,

    label(Vs).
