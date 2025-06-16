:- use_module(library(clpfd)).
:- ensure_loaded('../parser/grammar.pl').

:- begin_tests(parser).

test(clue_01) :-
    Sentence = [the, second, digit, is, less, than, '7'],
    parse_clue(Sentence, Clue),
    assertion(Clue = clue(second, less_than, 7)).

test(digit) :-
    assertion(phrase(digit(_), ['the', 'first'])),
    assertion(phrase(digit(_), ['the', 'second', 'digit'])),
    assertion(\+ phrase(digit(_), [])),
    assertion(\+ phrase(digit(_), ['9'])),
    assertion(\+ phrase(digit(_), ['the'])).

test(ord) :-
    assertion(phrase(ord(_), ['first'])),
    assertion(phrase(ord(_), ['fourth'])),
    assertion(\+ phrase(ord(_), [])),
    assertion(\+ phrase(ord(_), ['9'])),
    assertion(\+ phrase(ord(_), ['telescope'])).

test(operator) :-
    assertion(phrase(operator(_), ['greater', 'than'])),
    assertion(phrase(operator(_), ['equal', 'to'])),
    assertion(\+ phrase(operator(_), [])),
    assertion(\+ phrase(operator(_), ['9'])),
    assertion(\+ phrase(operator(_), ['telescope'])).

test(safe_digit) :-
    assertion(phrase(safe_digit(_), ['1'])),
    assertion(phrase(safe_digit(_), ['9'])),
    assertion(\+ phrase(safe_digit(_), [])),
    assertion(\+ phrase(safe_digit(_), ['123', '456'])),
    assertion(\+ phrase(safe_digit(_), ['99'])),
    assertion(\+ phrase(safe_digit(_), ['Hello', 'World'])).

test(some_clue) :-
    Vars = [A, B, C, D],
    assertion(clue_constraint(clue(first, less_than, 7), Vars, (A #< 7))),
    assertion(clue_constraint(clue(second, less_than, 7), Vars, (B #< 7))),
    assertion(clue_constraint(clue(third, greater_than, 2), Vars, (C #> 2))),
    assertion(clue_constraint(clue(fourth, equal_to, 4), Vars, (D #= 4))).


:- end_tests(parser).
