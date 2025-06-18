:- module(grammar, [
    clue//1,
    adj//1,
    fun//1,
    operator//1,
    ord//1,
    position//1,
    safe_digit//1
]).

% eg. the third digit is less than 5
clue(clue(Ordinal, Operator, Number)) -->
    position(Ordinal),
    i,
    operator(Operator),
    safe_digit(Number),
    !.
% eg. the third digit is less than the second
% eg. the second is twice the fourth
clue(clue(Ordinal1, Operator, Ordinal2)) -->
    position(Ordinal1),
    i,
    operator(Operator),
    position(Ordinal2),
    !.
% eg. the second digit is odd
clue(clue(Ordinal, Adj)) -->
    position(Ordinal),
    i,
    adj(Adj),
    !.
% eg. the third and fourth differ by 2
% eg. the first and third total 13
clue(clue(Ordinal1, Ordinal2, Func, Howmany)) -->
    position(Ordinal1),
    and,
    position(Ordinal2),
    function(Func),
    numeric_string(Howmany),
    !.
% eg. The fourth is 3 more than the first
clue(clue(Ordinal1, Ordinal2, Func, Howmany)) -->
    position(Ordinal1),
    i,
    numeric_string(Howmany),
    function(Func),
    position(Ordinal2),
    !.

adj(Adj) --> [Adj], { member(Adj, ['prime', 'odd', 'even']) }.

position(Ordinal) --> det, ord(Ordinal), d.
position(Ordinal) --> det, ord(Ordinal).
position(Ordinal) --> ord(Ordinal).

det --> ['the'].
and --> ['and'].
d --> ['digit'].
i --> ['is'].

ord(first) --> ['first'].
ord(second) --> ['second'].
ord(third) --> ['third'].
ord(fourth) --> ['fourth'].

function(differ_by) --> ['differ', 'by'].
function(add_up_to) --> ['total'].
function(greater_than) --> ['more', 'than'].
function(less_than) --> ['less', 'than'].

fun(Fun) --> [Fun], { member(Fun, ['differ_by', 'add_up_to', 'greater_than', 'less_than']) }.

operator(less_than) --> ['less', 'than'].
operator(greater_than) --> ['greater', 'than'].
operator(twice) --> ['twice'].
% operator(equal_to) --> ['equal', 'to'].

safe_digit(D) --> [C], { member(C, ['1','2','3','4','5','6','7','8','9']), atom_number(C, D) }.

numeric_string(Num) --> [Num]. % yeah, you try doing better

