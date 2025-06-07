:- use_module(library(clpfd)).


% On karaoke night, four friends took it in turns to sing a song. Jess's came
% after the Queen song, and the Madonna song came before Sue's but not third.
% Liam's came before Lisa's and before the Madonna song, and the Abba song came
% after the Oasis song, which was not sung by Sue. Who sung each song and in
% which slot?


% facts -----------------------------------------------------------------------


singer(jess).
singer(liam).
singer(lisa).
singer(sue).

song(abba).
song(madonna).
song(oasis).
song(queen).


% rules -----------------------------------------------------------------------


%solution1907(Slots) :-


% [(Person1, Song1), (Person2, Song2), (Person3, Song3), (Person4, Song4)]