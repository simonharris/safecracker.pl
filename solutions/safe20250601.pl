:- use_module('../safe_cracker').


solution_20250601(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    A #= 6,

    % 1. The sum of the second and third is a square
    is_square(B + C),

    % 2. Exactly three digits are even
    include(is_even, Vs, Evens),
    length(Evens, 3),

    % 3. The first and third differ by 1
    abs(A - C) #= 1,

    % 4. The first is greater than the second
    % A #> B,
    apply('The first is greater than the second', Vs)
,
    % 5. The fourth digit is prime
    % is_prime(D, 1),
    apply('The fourth digit is prime', Vs),

    label(Vs).
