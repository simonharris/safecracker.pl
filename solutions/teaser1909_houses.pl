
% Crypticville neighbours are discussing where they live. "Fay lives next to
% May but not in the red house; Ray lives next to the grey house and No 8 is
% not the blue house," said Jay. "No 4 is between Jay and the white house, May
% does not live next to No 2 and Jay does not live at No 6 or the blue house,"
% said Fay. Where does each person live?


solution1909(Two, Four, Six, Eight) :-
    Street = [Two, Four, Six, Eight],

    Two = TwoN-TwoC,
    Four = FourN-FourC,
    Six = SixN-SixC,
    Eight = EightN-EightC,

    Names = [fay, jay, may, ray],
    Colours = [blue, grey, red, white],

    permutation(Names, [TwoN, FourN, SixN, EightN]),
    permutation(Colours, [TwoC, FourC, SixC, EightC]),

    % Fay lives next to May...
    next_to(fay, may, Street),

    % ...but not in the red house
    not_in_colour(fay, red, Street),

    % Ray lives next to the grey house
    next_to_colour(ray, grey, Street),

    % No 8 is not the blue house
    EightC \= blue,

    % No 4 is between Jay and the white house
    not_in_colour(jay, white, Street),
    (TwoN = jay ; SixN = jay),
    (TwoC = white ; SixC = white),

    % May does not live next to No 2
    FourN \= may,

    % Jay does not live at No 6...
    SixN \= jay,

    % ...or the blue house
    not_in_colour(jay, blue, Street).


next_to(Name1, Name2, List) :-
    append(_, [N1-_, N2-_|_], List),
    (N1 = Name1, N2 = Name2 ; N1 = Name2, N2 = Name1).


next_to_colour(Name, Colour, [H|T]) :-
    next_to_colour_helper([H|T], Name, Colour).

next_to_colour_helper([Name-_, _-Colour|_], Name, Colour).
next_to_colour_helper([_-Colour, Name-_|_], Name, Colour).
next_to_colour_helper([_|T], Name, Colour) :-
    next_to_colour_helper(T, Name, Colour).


not_in_colour(Person, Color, Street) :-
    \+ member(Person-Color, Street).
