% is_palindrome: string → bool that checks if the string given as input is palindrome
%   a string is palindrome when the represented sentence can be read the same way
%   in either directions in spite of spaces, punctual and letter cases

-module(palindrome).
-export([is_palindrome/1]).
-export([palindrome_test/0]).

reverse(Arr) ->
    reverse(Arr, []).
reverse([], Acc) -> Acc;
reverse([Head | Rest], Acc) ->
    reverse(Rest, [Head | Acc]).

only_letters(Str) -> 
    reverse(only_letters(Str, [])).
only_letters([], Acc) -> Acc;
only_letters([Head | Rest], Acc) -> 
    case ($a =< Head andalso $z >= Head) orelse
        ($A =< Head andalso $Z >= Head) of
            true -> only_letters(Rest, [Head|Acc]);
            false -> only_letters(Rest, Acc)
    end.

is_palindrome(Str) ->
    Lw = string:to_lower(only_letters(Str)),
    Lw == reverse(Lw).

palindrome_test() ->
    true  = is_palindrome(""),
    true  = is_palindrome("a"),
    true  = is_palindrome("aa"),
    false = is_palindrome("abb"),
    true  = is_palindrome("aba"),
    true  = is_palindrome("Aba"),
    true  = is_palindrome("AbCdcba"),
    true  = is_palindrome("Madam I\'m Adam"),
    ok.