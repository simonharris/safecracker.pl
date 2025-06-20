:- use_module('../safe_cracker').


solution_20250413(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    % 1. The fourth is three more than the first
    % D #= A + 3,
    apply('The fourth is three more than the first', Vs),

    % 2. Either the second or the third is odd, but not both
    xor(B mod 2 #= 1, C mod 2 #= 1),

    % 3. The fourth digit is less than seven
    %D #< 7,
    apply('The fourth digit is less than seven', Vs),

    % 4. The second is greater than the first
    % B #> A,
    apply('The second is greater than the first', Vs),

    % 5. The fourth is greater than the sum of the second and third
    % D #> (B + C),
    apply('The fourth is greater than the sum of the second and third', Vs),

    label(Vs).
