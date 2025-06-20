:- use_module('safe_cracker').
:- use_module('parser/parser').
:- dynamic input_cached/1.


solution(A, B, C, D) :-
    read_input_once,
    input_cached(Input),

    Vs = [A, B, C, D],
    common_constraints(Vs),

    forall(member(Line, Input),
       (
        % writeln(Line),
        % writeln(Vs),
        apply(Line, Vs)
       )),
    label(Vs),
    format('~w~n', [Vs]).


solution_count(A, B, C, D, Count) :-
    findall(t, solution(A, B, C, D), Solutions),
    length(Solutions, Count).



% read_input_once :-
%     input_cached(Input), !.
% read_input_once :-
%     read_input_from_stdin(Input),
%     assert(input_cached(Input)).

read_input_once :-
    \+ input_cached(_),  % Check if input is already cached
    read_input_from_stdin(Input),
    assert(input_cached(Input)).

read_input_from_stdin(Lines) :-
    read_lines([], Lines). %,
    %writeln('Read lines:'), writeln(Lines).

read_lines(Lines, Lines) :-
    at_end_of_stream(user_input), !.
read_lines(Acc, Lines) :-
    \+ at_end_of_stream(user_input),
    read_line_to_string(user_input, Line),
    Line \= end_of_file,
    read_lines([Line|Acc], Lines).
