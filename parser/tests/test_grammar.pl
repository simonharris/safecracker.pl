:- ensure_loaded('../grammar').
:- use_module(library(plunit_assert)).
%:- use_module(plunit_assert).

:- begin_tests(grammar).

% atomic ordinals
test(ord) :-
    assert_output(phrase(ord(PosA), ['first']), [PosA], [first]),
    assert_output(phrase(ord(PosB), ['fourth']), [PosB], [fourth]),
    assert_false(phrase(ord(_), [])),
    assert_false(phrase(ord(_), ['9'])),
    assert_false(phrase(ord(_), ['telescope'])).

% ordinal clauses (see ord/1)
test(digit) :-
    assert_output(phrase(position(PosA), ['the', 'first']), [PosA], [first]),
    assert_output(phrase(position(PosB), ['the', 'second', 'digit']), [PosB], [second]),
    assert_output(phrase(position(PosC), ['third']), [PosC], [third]),
    assert_false(phrase(position(_), [])),
    assert_false(phrase(position(_), ['9'])),
    assert_false(phrase(position(_), ['the'])).

test(operator) :-
    assert_output(phrase(operator(Op), ['greater', 'than']), [Op], [greater_than]),
    assert_false(phrase(operator(_), [])),
    assert_false(phrase(operator(_), ['9'])),
    assert_false(phrase(operator(_), ['telescope'])).

% the domain is 1..9
test(safe_digit) :-
    assert_output(phrase(safe_digit(DigA), [1]), [DigA], [1]),
    assert_output(phrase(safe_digit(DigB), [9]), [DigB], [9]),
    assert_false(phrase(safe_digit(_), [])),
    assert_false(phrase(safe_digit(_), ['0'])),
    assert_false(phrase(safe_digit(_), ['123', '456'])),
    assert_false(phrase(safe_digit(_), ['99'])),
    assert_false(phrase(safe_digit(_), ['Hello', 'World'])).

test(position_fourth) :-
    assert_output(phrase(position(Ordinal), ['fourth'], []), [Ordinal], [fourth]),
    assert_output(phrase(position(Ordinal), ['the', 'fourth'], []), [Ordinal], [fourth]),
    assert_output(phrase(position(Ordinal), ['the', 'last', 'digit'], []), [Ordinal], [fourth]),
    !.

:- end_tests(grammar).
