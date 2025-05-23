:- use_module('../safe_cracker').


solution_20250504(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    % 1. The third digit is less than 5
    C #< 5,

    % 2. The second is twice the fourth
    B #= 2 * D,

    % 3. Exactly two digits are prime
    maplist(is_prime, Vs, PrimeDigits),
    sum(PrimeDigits, #=, 2),

    % 4. The second exceeds the first by more than 2
    B #> A + 2,

    % 5. The first and third differ by 3
    abs(A - C) #= 3,

    label(Vs).