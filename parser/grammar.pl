:- module(grammar, [
    clue//1,

    adj//1,
    fun//1,
    % num//1,
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
% eg. The third and fourth differ by 2
% eg. he first and third total 13
clue(clue(Ordinal1, Ordinal2, Func, Howmany)) -->
    position(Ordinal1),
    n,
    position(Ordinal2),
    function(Func),
    numeric_string(Howmany),
    !.

adj(Adj) --> [Adj], { member(Adj, ['prime', 'odd', 'even']) }.


position(Ordinal) --> det, ord(Ordinal), d.
position(Ordinal) --> det, ord(Ordinal).
position(Ordinal) --> ord(Ordinal).

det --> ['the'].
n --> ['and'].
d --> ['digit'].
i --> ['is'].

ord(first) --> ['first'].
ord(second) --> ['second'].
ord(third) --> ['third'].
ord(fourth) --> ['fourth'].

function(differ_by) --> ['differ', 'by'].
function(add_up_to) --> ['total'].

fun(Fun) --> [Fun], { member(Fun, ['differ_by', 'add_up_to']) }.

operator(less_than) --> ['less', 'than'].
operator(greater_than) --> ['greater', 'than'].
operator(equal_to) --> ['equal', 'to'].

safe_digit(D) --> [C], { member(C, ['1','2','3','4','5','6','7','8','9']), atom_number(C, D) }.

numeric_string(Num) --> [Num]. % yeah, you try doing better

