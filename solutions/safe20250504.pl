:- use_module('../safe_cracker').


solution_20250504(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    % 1. The third digit is less than five
    % C #< 5,
    apply('The third digit is less than five', Vs),

    % 2. The second is twice the fourth
    %B #= 2 * D,
    apply('The second is twice the fourth', Vs),

    % 3. Exactly two digits are prime
    % maplist(is_prime, Vs, PrimeDigits),
    % sum(PrimeDigits, #=, 2),
    apply('Exactly two digits are prime', Vs),

    % 4. The second exceeds the first by more than two
    B #> A + 2,

    % 5. The first and third differ by three
    % abs(A - C) #= 3,
    apply('The first and third differ by three', Vs),

    label(Vs).


% count_primes([], 0).
% count_primes([H|T], Count) :-
%     count_primes(T, CountT),
%     (   is_prime(H)
%     ->  Count #= CountT + 1
%     ;   Count #= CountT
%     ).

% exactly_two_primes(List) :-
%     count_primes(List, 2).