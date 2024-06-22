% squared_int that removes all non-integers from a polymorphic list and returns the resulting list of integers squared
%   e.g., squared_int([1, hello, 100, boo, “boo”, 9]) should return [1, 10000, 81].
% intersect that given two lists, returns a new list that is the intersection of the two lists
%   e.g., intersect([1,2,3,4,5], [4,5,6,7,8]) should return [4,5].
% symmetric_difference that given two lists, returns a new list that is the symmetric difference of the two lists.
%   e.g., symmetric_difference([1,2,3,4,5], [4,5,6,7,8]) should return [1,2,3,6,7,8].

-module(list_comprehension).
-export([squared_int/1, intersect/2, symmetric_difference/2]).
-export([squared_int_test/0, intersect_test/0, symmetric_difference_test/0]).

squared_int([]) -> [];
squared_int(Arr) -> squared_int(Arr, []).

squared_int([], Acc) -> lists:reverse(Acc);
squared_int([Head|Rest], Acc) when is_integer(Head) -> squared_int(Rest, [Head*Head | Acc]);
squared_int([_|Rest], Acc) -> squared_int(Rest, Acc).

squared_int_test() ->
    true = squared_int([1, hello, 100, boo, "boo", 9]) == [1, 10000, 81],
    false = squared_int([1, hello, 100, boo, "boo", 9]) == [1],
    ok.

intersect(Arr1, Arr2) -> intersect(Arr1, Arr2, []).
intersect([], _, Acc) -> lists:reverse(Acc);
intersect([Head|Rest], Arr2, Acc) -> case lists:member(Head, Arr2) of
    true -> intersect(Rest, Arr2, [Head|Acc]);
    false -> intersect(Rest, Arr2, Acc)
end.

intersect_test() ->
    true = intersect([1,2,3,4,5], [4,5,6,7,8]) == [4,5],
    true = intersect([], [4,5,6,7,8]) == [],
    ok.

symmetric_difference(Arr1, Arr2) ->
    lists:append(symmetric_difference(Arr1, Arr2, []), symmetric_difference(Arr2, Arr1, [])).
symmetric_difference([], _, Acc) -> lists:reverse(Acc);
symmetric_difference([Head|Rest], Arr2, Acc) -> case lists:member(Head, Arr2) of
    true -> symmetric_difference(Rest, Arr2, Acc);
    false -> symmetric_difference(Rest, Arr2, [Head|Acc])
end.

symmetric_difference_test() ->
    true = symmetric_difference([1,2,3,4,5], [4,5,6,7,8]) == [1,2,3,6,7,8],
    ok.