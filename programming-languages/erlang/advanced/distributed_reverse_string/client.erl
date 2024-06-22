-module(client).
-export([start/0, reverse_string/1, reverse_string/2, stop/0]).

start() ->
    register(master_proc, spawn(master, start, [])).

reverse_string(Str) -> reverse_string(Str, 10).
reverse_string(Str, M) ->
    master_proc ! {long_reverse_string, self(), Str, M},
    listen().

listen() ->
    receive
        {long_reverse_string, Reversed} ->
            io:format("Master process reversed the string ...~n"),
            io:format("~p~n", [Reversed])
    end.

stop() ->
    master_proc ! {stop},
    ok.