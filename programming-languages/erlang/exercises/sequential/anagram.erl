% is_an_anagram : string → string list → boolean that given a dictionary of strings
%   checks if the input string is an anagram of one or more of the strings in the dictionary;

-module(anagram).
-export([is_an_anagram/2]).
-export([is_an_anagram_test/0]).

is_an_anagram(Str, Arr) ->
    is_an_anagram(Str, Arr, false).
is_an_anagram(_, [], Acc) -> Acc;
is_an_anagram(Str, [Head | Rest], Acc) ->
    case Acc of
        true -> Acc;
        false -> is_an_anagram(Str, Rest, lists:sort(string:lowercase(Head)) == lists:sort(string:lowercase(Str)))
    end.

is_an_anagram_test() ->
    true  = is_an_anagram("cat", ["act", "tac"]),
    false = is_an_anagram("cat", ["test", "abcde"]),
    ok.