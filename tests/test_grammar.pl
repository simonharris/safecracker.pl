:- ensure_loaded('../parser/grammar').


:- begin_tests(grammar).

test(digit) :-
    assertion(phrase(position(_), ['the', 'first'])),
    assertion(phrase(position(_), ['the', 'second', 'digit'])),
    assertion(\+ phrase(position(_), [])),
    assertion(\+ phrase(position(_), ['9'])),
    assertion(\+ phrase(position(_), ['the'])).

test(ord) :-
    assertion(phrase(ord(_), ['first'])),
    assertion(phrase(ord(_), ['fourth'])),
    assertion(\+ phrase(ord(_), [])),
    assertion(\+ phrase(ord(_), ['9'])),
    assertion(\+ phrase(ord(_), ['telescope'])).

test(operator) :-
    assertion(phrase(operator(_), ['greater', 'than'])),
    %assertion(phrase(operator(_), ['equal', 'to'])),
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



:- end_tests(grammar).
