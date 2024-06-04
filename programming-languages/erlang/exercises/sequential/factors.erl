% factors: int → int list that given a number calculates all its prime factors;

-module(factors).
-export([factors/1]).
-export([factors_test/0]).

mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0,_) -> 0.


factors(Num) ->
    factors(Num, 2, []).
factors(Num, _, Acc) when Num == 1 ->
    [1 | Acc];
factors(Num, I, Acc) ->
    case mod(Num, I) == 0 of
        true -> factors(Num div I, I, [I | Acc]);
        false -> factors(Num, I+1, Acc)
    end.

factors_test() ->
    io:format("factors of 63: ~w~n", [factors(63)]),
    io:format("factors of 7: ~w~n", [factors(7)]),
    io:format("factors of 28: ~w~n", [factors(28)]),
    io:format("factors of 128: ~w~n", [factors(128)]),
    io:format("factors of 69: ~w~n", [factors(69)]),
    ok.