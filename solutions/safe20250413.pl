:- use_module('../safe_cracker').


solution_20250413(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    % 1. The fourth is 3 more than the first
    D #= A + 3,

    % 2. Either the second or the third is odd, but not both
    xor(B mod 2 #= 1, C mod 2 #= 1),

    % 3. The fourth digit is less than 7
    %D #< 7,
    parse_text('The fourth digit is less than 7', Vs, Constraint3),
    call(Constraint3),

    % 4. The second is greater than the first
    % B #> A,
    parse_text('The second is greater than the first', Vs, Constraint4),
    call(Constraint4),

    % 5. The fourth is greater than the sum of the second and fourth
    D #> (B + C),

    label(Vs).
