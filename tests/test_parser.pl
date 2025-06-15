:- ensure_loaded('../parser/grammar.pl').

:- begin_tests(parser).

test(clue_01) :-
    assertion(phrase(clue, ['the', 'second', 'digit', 'is', 'less', 'than', '7'])),
    assertion(\+ phrase(clue, ['not', 'a', 'valid', 'clue'])).

:- end_tests(parser).
