-module(master).
-export([start/0]).

start() -> listen().

listen() ->
    receive
        {long_reverse_string, SenderPid, Str, M} ->
            Sub_Size = string:length(Str) div M,
            to_slave(Str, Sub_Size, M),
            listen(SenderPid, Str, M, #{});
        {stop} -> io:format("Stopping master process ... ~n")
    end.

listen(SenderPid, Str, Parts, Map) ->
    case length(maps:keys(Map)) == Parts of
        true ->
            lists:foreach(fun (T) -> io:format("~p", [T]) end, maps:to_list(Map)),
            SenderPid ! {long_reverse_string, recompose_string(Map, Parts)},
            listen();
        false -> receive
            {reversed, I, Rev_Sub} -> listen(SenderPid, Str, Parts, Map#{I => Rev_Sub})
        end
    end.

to_slave(Str, Size, Parts) -> to_slave(Str, Size, Parts, 0).
to_slave(_, _, Parts, I) when I == Parts -> ok;
to_slave(Str, Size, Parts, I) ->
    Slice = case (I+1) == Parts of
        true -> string:slice(Str, I*Size);
        false -> string:slice(Str, I*Size, Size)
    end,
    io:format("~p~n", [Slice]),
    spawn(slave, start, [self(), I, Slice]),
    to_slave(Str, Size, Parts, I+1).

recompose_string(Map, Parts) -> recompose_string(Map, Parts - 1, []).
recompose_string(_, I, Acc) when I == -1 -> Acc;
recompose_string(Map, I, Acc) -> recompose_string(Map, I-1, lists:append(Acc, maps:get(I, Map))).