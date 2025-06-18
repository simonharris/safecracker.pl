:- use_module('../safe_cracker').
:- use_module('../parser/parser').


solution_20250615(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    % 1. The first digit is greater than 5
    % A #> 5,
    apply('The first digit is greater than 5', Vs),

    % 2. The third is 2 less than the fourth
    % C #= D - 2,
    apply('The third is 2 less than the fourth', Vs),

    % 3. The fourth is less than the second
    %D #< B,
    apply('The fourth is less than the second', Vs),

    % 4. Only one digit is odd
    include(is_odd, Vs, Odds),
    length(Odds, 1),

    % 5. The first and fourth differ by 1
    % abs(A - D) #= 1,
    apply('The first and fourth differ by 1', Vs),

    label(Vs).
