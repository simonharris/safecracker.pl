:- module(grammar, [
    clue//1,

    adj//1,
    position//1,
    operator//1,
    ord//1,
    safe_digit//1
]).

% eg. the third digit is less than 5
clue(clue(Ordinal, Operator, Number)) -->
    position(Ordinal),
    i,
    operator(Operator),
    safe_digit(Number),
    !.
% eg. the third digit is less than the secod
clue(clue(Ordinal, Operator, Arg2)) -->
    position(Ordinal),
    i,
    operator(Operator),
    position(Arg2),
    !.
% eg. the second digit is odd
clue(clue(Ordinal, Adj)) -->
    position(Ordinal),
    i,
    adj(Adj),
    !.

adj(Adj) --> [Adj], { member(Adj, [prime, odd, even ]) }.


position(Ordinal) --> det, ord(Ordinal).
position(Ordinal) --> det, ord(Ordinal), d.

det --> ['the'].

ord(first) --> ['first'].
ord(second) --> ['second'].
ord(third) --> ['third'].
ord(fourth) --> ['fourth'].

d --> ['digit'].

i --> ['is'].

operator(less_than) --> [less, than].
operator(greater_than) --> [greater, than].
operator(equal_to) --> [equal, to].

safe_digit(D) --> [C], { member(C, ['1','2','3','4','5','6','7','8','9']), atom_number(C, D) }.


