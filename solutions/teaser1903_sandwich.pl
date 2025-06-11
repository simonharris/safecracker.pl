:- discontiguous didnt_eat/2.

/*
There has been a mix up in the Co. Fusion plc office and everyone has eaten the wrong lunch.

Katherine ate Jim's sandwich, and Tim packed tuna

*/
% facts -----------------------------------------------------------------------


packed(tim, tuna).

didnt_eat(jim, egg).
didnt_eat(jim, bacon).

% presumably
male(jim).
male(tim).


% rules -----------------------------------------------------------------------


solution1903(Eaters) :-

    People = [francine, jim, katherine, tim],
    Sandwiches = [bacon, cheese, egg, tuna],

    permutation(Sandwiches, Swperms),
    pairs(People, Swperms, Eaters),
    pairs(People, Swperms, Packers),

    no_one_eats_own(Eaters),
    check_didnt_eat(Eaters),

    writeln(Eaters),
    true.


no_one_eats_own(Eaters) :-
    \+ (member(P-S, Eaters), packed(P, S)).


check_didnt_eat(Eaters) :-
    \+ (member(P-S, Eaters), didnt_eat(P, S)).

didnt_eat(Person, cheese):-
    \+ male(Person).

pairs([], [], []).
pairs([P|Ps], [S|Ss], [P-S|Pairs]) :-
    pairs(Ps, Ss, Pairs).

count_solutions :-
    findall(Eaters, solution1903(Eaters), Solutions),
    length(Solutions, Count),
    writeln(Count).

