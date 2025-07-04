:- use_module('../safe_cracker').
:- use_module('../parser').


solution_20250601(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    % 1. The sum of the second and third is a square
    % is_square(B + C),
    apply_clue('The sum of the second and third is a square', Vs),

    % 2. Exactly three digits are even
    % include(is_even, Vs, Evens),
    % length(Evens, 3),
    apply_clue('Exactly three digits are even', Vs),

    % 3. The first and third differ by one
    % abs(A - C) #= 1,
    apply_clue('The first and third differ by one', Vs),

    % 4. The first is greater than the second
    % A #> B,
    apply_clue('The first is greater than the second', Vs),

    % 5. The fourth digit is prime
    % is_prime(D, 1),
    apply_clue('The fourth digit is prime', Vs),

    label(Vs).
