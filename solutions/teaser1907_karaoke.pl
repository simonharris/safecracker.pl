
% On karaoke night, four friends took it in turns to sing a song. Jess's came
% after the Queen song, and the Madonna song came before Sue's but not third.
% Liam's came before Lisa's and before the Madonna song, and the Abba song came
% after the Oasis song, which was not sung by Sue. Who sung each song and in
% which slot?

% NOTE: the question is ambiguously-worded. It does not explicitly rule out
% Lisa having sung the Madonna song, which leads to three valid solutions


solution1907(First, Second, Third, Fourth) :-
    Karaoke = [First, Second, Third, Fourth],

    First = FirstN-FirstS,
    Second = SecondN-SecondS,
    Third = ThirdN-ThirdS,
    Fourth = FourthN-FourthS,

    Names = [jess, liam, lisa, sue],
    Songs = [abba, madonna, oasis, queen],

    permutation(Names, [FirstN, SecondN, ThirdN, FourthN]),
    permutation(Songs, [FirstS, SecondS, ThirdS, FourthS]),

    % Jess's came after the Queen song
    order(_, jess, queen, _, Karaoke),

    % the Madonna song came before Sue's...
    order(_, sue, madonna, _, Karaoke),

    % ...but not third
    ThirdS \= madonna,

    % Liam's came before Lisa's...
    order(liam, lisa, _, _, Karaoke),

    % ...and before the Madonna song
    order(liam, _, _, madonna, Karaoke),

    % see note above
    not_sung_by(lisa, madonna, Karaoke),

    % the Abba song came after the Oasis song...
    order(_, _, oasis, abba, Karaoke),

    % ...which was not sung by Sue.
    not_sung_by(sue, oasis, Karaoke).


not_sung_by(Person, Song, Karaoke) :-
    \+ member(Person-Song, Karaoke).


order(SingerA, SingerB, SongA, SongB, Karaoke) :-
    nth0(IndexA, Karaoke, SingerA-SongA),
    nth0(IndexB, Karaoke, SingerB-SongB),
    IndexA < IndexB.
