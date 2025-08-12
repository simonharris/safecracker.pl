:- module(grammar, [
    clue_spec//1,
    adj//1,
    operator//1,
    ord//1,
    out//1,
    position//1,
    safe_digit//1
]).

% TODO: there's a lot of duplication of "greater than" in here

% eg. The third digit is less than five
clue_spec(clue(Ordinal, Operator, Number)) -->
    position(Ordinal),
    be,
    operator(Operator),
    safe_digit(Number), % TODO: this probably works by coincidence
    !.
% eg. The third digit is less than the second
% eg. The second is twice the fourth
clue_spec(clue(Ordinal1, Operator, Ordinal2)) -->
    position(Ordinal1),
    be,
    operator(Operator),
    position(Ordinal2),
    !.
% eg. The second digit is odd
% eg. The third digit is the greatest
clue_spec(clue(Ordinal, Adj)) -->
    position(Ordinal),
    be,
    adj_clause(Adj),
    !.
% eg. The first and second total the third
clue_spec(clue(Ordinal1, Ordinal2, Func, Ordinal3)) -->
    position(Ordinal1),
    and,
    position(Ordinal2),
    function(Func),
    position(Ordinal3),
    !.
% eg. The fourth is greater than the sum of the second and third
clue_spec(clue(sum, lt, Ordinal2,  Ordinal3, Ordinal1)) -->
    position(Ordinal1),
    gt,
    sum_clause(Ordinal2, Ordinal3),
    !.

% eg. The third and fourth differ by two
% eg. The first and third total 13
% eg. The first and last digits differ by three
% eg. The first and second differ by (no) more than four
clue_spec(clue(Ordinal1, Ordinal2, Func, Howmany)) -->
    position(Ordinal1),
    and,
    position(Ordinal2),
    function(Func),
    numeric_string(Howmany),
    !.

% Kind of a special case atm
% eg. The first two digits differ by four
clue_spec(clue(first, second, Func, Howmany)) -->
    first_two,
    function(Func),
    numeric_string(Howmany),
    !.

% eg. The fourth is three more than the first
% eg. The second is three greater than the first
clue_spec(clue(Ordinal1, Ordinal2, Func, Howmany)) -->
    position(Ordinal1),
    be,
    numeric_string(Howmany),
    function(Func),
    position(Ordinal2),
    !.
% eg. Exactly three digits are even
% eg  Exactly two digits are not prime
clue_spec(clue(Adj, Howmany)) -->
    quant(Howmany),
    be,
    adj_clause(Adj),
    !.
% eg. Exactly one of the digits is one
clue_spec(clue(Outcome, Howmany, Value)) -->
    quant(Howmany),
    outcome(Outcome),
    safe_digit(Value),
    !.
% eg. The sum of the second and third is a square
% eg. The sum of the first and fourth is square
% eg. The sum of the first and fourth is prime
% eg. The sum of the second and third is a two digit prime
clue_spec(clue(sum, Ordinal1, Ordinal2, Adj)) -->
    sum_clause(Ordinal1, Ordinal2),
    be,
    adj_clause(Adj),
    % { writeln(Adj) },
    !.
% eg. The sum of the first and second is less than the third
clue_spec(clue(sum, lt, Ordinal1, Ordinal2, Ordinal3)) -->
    sum_clause(Ordinal1, Ordinal2),
    lt,
    position(Ordinal3),
    !.
% eg. The sum of the first and third exceeds 10
% eg. The sum of the first and third is greater than 13
% eg. The sum of the first and second is less than seven
clue_spec(clue(sum, Operator, Ordinal1, Ordinal2, Howmany)) -->
    sum_clause(Ordinal1, Ordinal2),
    operator(Operator),
    numeric_string(Howmany), % serious TODO here
    !.
% eg. The sum of the second and fourth is divisible by five'
clue_spec(clue(sum, db, Ordinal1, Ordinal2, Howmany)) -->
    sum_clause(Ordinal1, Ordinal2),
    db,
    numeric_string(Howmany),
    !.
% eg. The second minus the first is less than three
clue_spec(clue(minus, lt, Ordinal1, Ordinal2, Howmany)) -->
    minus_clause(Ordinal1, Ordinal2),
    lt,
    numeric_string(Howmany),
    !.
% eg. Either the second or the third is odd, but not both
% eg. Exactly one of the second and third is odd
clue_spec(clue(either, Ordinal1, Ordinal2, Adj)) -->
    either_clause,
    position(Ordinal1),
    conj,
    position(Ordinal2),
    be,
    adj_clause(Adj),
    superfluous_waffle,
    !.
% eg. The second exceeds the first by more than two
clue_spec(clue(Ordinal1, Ordinal2, exceeds_by_more_than, Howmany)) -->
    position(Ordinal1),
    gt,
    position(Ordinal2),
    by,
    more,
    than,
    numeric_string(Howmany),
    !.

quant(Howmany) -->
    qmod,
    numeric_string(Howmany),
    part,
    d.

qmod --> ['exactly'].
qmod --> ['only'].
qmod --> [].

part --> ['of', 'the'].
part --> [].

d --> ['digit'].
d --> ['digits'].

adj_clause(Adj) --> det, adj(Adj).
adj_clause(Adj) --> adj(Adj).

adj(not_prime) --> [not, prime]. % we need to discuss negation
adj(two_digit_prime) --> [two, digit, prime]. % we need to discuss counting digits
adj(Adj) --> [Adj], { member(Adj, ['prime', 'not_prime', 'two_digit_prime', 'odd', 'even', 'square', 'greatest']) }.

position(Ordinal) --> det, ord(Ordinal), d.
position(Ordinal) --> det, ord(Ordinal).
position(Ordinal) --> ord(Ordinal), d.
position(Ordinal) --> ord(Ordinal).

det --> ['the'].
det --> ['a'].

and --> ['and'].
or --> ['or'].

conj --> and.
conj --> or.

be --> ['is'].
be --> ['are'].

outcome(divisible_by) --> be, [ 'divisible', 'by'].
outcome(equal) --> be.
out(Out) --> [Out], { member(Out, [divisible_by, equal]) }. % nb

ord(first) --> ['first'].
ord(second) --> ['second'].
ord(third) --> ['third'].
ord(fourth) --> ['fourth'].
ord(fourth) --> ['last'].

first_two --> ['the', 'first', 'two', 'digits'].

function(differ_by_more_than) --> ['differ', 'by', 'more', 'than'].
function(differ_by_no_more_than) --> ['differ', 'by', 'no', 'more', 'than'].
function(differ_by) --> ['differ', 'by'].
function(add_up_to) --> ['total'].
function(greater_than) --> gt.
function(less_than) --> ['less', 'than']. % nb

qualifier(more_than) --> ['more', 'than']. % nb

operator(less_than) --> ['less', 'than'].
operator(less_than) --> lt.
operator(greater_than) --> ['greater', 'than'].
operator(divisible_by) --> ['divisible', 'by'].
operator(twice) --> ['twice'].
operator(gt) --> gt.


sumof --> ['the', 'sum', 'of'].
minus --> ['minus'].

sum_clause(Ordinal1, Ordinal2) -->
    sumof,
    position(Ordinal1),
    and,
    position(Ordinal2).

minus_clause(Ordinal1, Ordinal2) -->
    position(Ordinal1),
    minus,
    position(Ordinal2).

either_clause --> qmod, ['one', 'of'].
either_clause --> ['either'].

safe_digit(D) --> [C], { member(C, ['1','2','3','4','5','6','7','8','9']), atom_number(C, D) }.

numeric_string(Num) --> [Num]. % yeah, you try doing better

superfluous_waffle --> ['but', 'not', 'both'].
superfluous_waffle --> [].

by --> ['by'].
more --> ['more'].
than --> ['than'].

/*
The duplication around these is running rife. Let's try defining some "atomic"
operators and see if we can use them throughout
*/

gt --> ['more', 'than'].
gt --> ['is', 'greater', 'than'].
gt --> ['greater', 'than'].
gt --> ['exceeds'].

lt --> ['is', 'less', 'than'].

lte --> ['no', 'more', 'than'].

db --> ['is', 'divisible', 'by'].
