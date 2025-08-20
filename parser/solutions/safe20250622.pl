:- use_module('../safe_cracker').
:- use_module('../parser').


solution_20250622(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    % 1. The third digit is prime
    apply_clue('The third digit is prime', Vs),

    % 2. The sum of the first and third exceeds 10
    apply_clue('The sum of the first and third exceeds 10', Vs),

    % 3. Only one digit is even
    apply_clue('Only one digit is even', Vs),

    % 4. The first and second total six
    apply_clue('The first and second total six', Vs),

    % 5. The fourth is three less than the third
    apply_clue('The fourth is three less than the third', Vs),


    label(Vs).
