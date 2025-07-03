:- use_module('../safe_cracker').
:- use_module('../parser').


solution_20250629(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    % 1. The first digit is divisible by three
    apply_clue('The first digit is divisible by three', Vs),

    % 2. The second and third digits total 11
    apply_clue('The second and third digits total 11', Vs),

    % 3. The sum of the first and third is greater than 13
    apply_clue('The sum of the first and third is greater than 13', Vs),

    % 4. The first and last digits differ by three
    apply_clue('The first and last digits differ by three', Vs),

    % 5. Exactly two digits are not prime
    apply_clue('Exactly two digits are prime', Vs),

    label(Vs).
