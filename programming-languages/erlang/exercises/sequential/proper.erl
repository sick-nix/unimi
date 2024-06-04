% is_proper: int → boolean that given a number calculates if it is a perfect number or not
%   where a perfect number is a positive integer equal to the sum of its proper positive divisors (excluding itself)
%   e.g., 6 is a perfect number since 1, 2 and 3 are the proper divisors of 6 and 6 is equal to 1+2+3;

-module(proper).
-export([is_proper/1]).
-export([is_proper_test/0]).

is_proper(Num) ->
    Num == sum(divisors(Num)).

sum(Arr) ->
    sum(Arr, 0).
sum([], Acc) ->
    Acc;
sum([Head | Rest], Acc) ->
    sum(Rest, Acc + Head).


mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0,_) -> 0.


divisors(Num) ->
    divisors(Num, 2, []).
divisors(Num, I, Acc) when Num == 1 orelse I > (Num div 2) ->
    [1 | Acc];
divisors(Num, I, Acc) ->
    case mod(Num, I) == 0 of
        true -> divisors(Num, I+1, [I | Acc]);
        false -> divisors(Num, I+1, Acc)
    end.

is_proper_test() ->
    io:format("is 6 (~w) proper? ~w~n", [divisors(6), is_proper(6)]),
    io:format("is 7 (~w) proper? ~w~n", [divisors(7), is_proper(7)]),
    io:format("is 28 (~w) proper? ~w~n", [divisors(28), is_proper(28)]),
    io:format("is 496 (~w) proper? ~w~n", [divisors(496), is_proper(496)]),
    io:format("is 69 (~w) proper? ~w~n", [divisors(69), is_proper(69)]),
    io:format("is 8128 (~w) proper? ~w~n", [divisors(8128), is_proper(8128)]),
    ok.