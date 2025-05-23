:- use_module('../safe_cracker').


solution_20250413(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    D #= A + 3,
    xor(B mod 2 #= 1, C mod 2 #= 1),
    D #< 7,
    B #> A,
    D #> (B + C),
    label(Vs).
