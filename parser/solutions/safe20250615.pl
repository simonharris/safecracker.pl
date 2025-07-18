:- use_module('../safe_cracker').


solution_20250615(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    % 1. The first digit is greater than five
    % A #> 5,
    apply_clue('The first digit is greater than five', Vs),

    % 2. The third is two less than the fourth
    % C #= D - 2,
    apply_clue('The third is two less than the fourth', Vs),

    % 3. The fourth is less than the second
    %D #< B,
    apply_clue('The fourth is less than the second', Vs),

    % 4. Only one digit is odd
    % include(is_odd, Vs, Odds),
    % length(Odds, 1),
    apply_clue('Only one digit is odd', Vs),

    % 5. The first and fourth differ by one
    % abs(A - D) #= 1,
    apply_clue('The first and fourth differ by one', Vs),

    label(Vs).
