-module(sleepsort).
-export([start/0, loop/0, child/2, sleepsort/1]).

start() ->
    case whereis(server) of
        undefined -> ok;
        _ -> unregister(server)
    end,
    register(server, spawn(sleepsort, loop, [])),
    io:format("Sleepsort server started~n").    

loop() ->
receive
    {stop, Pid} -> Pid ! {stopped};
    {sort, Pid, List} -> 
        io:format("sleepsort started: ~p~n", [List]),
        Pid ! {sorted, sleepsort(List)},
        loop()
end.

sleepsort(List) -> spawn_children(List), waitForSorted(List).

gethostname() ->
    case net:gethostname() of
        {ok, Hostname} -> Hostname;
        {error, Reason} -> throw(Reason)
    end.
get_child_node(I) -> list_to_atom("sleep" ++ string:reverse(string:slice([integer_to_list(I)] ++ "0", 0, 2)) ++ "@" ++ gethostname()).

spawn_children(List) -> spawn_children(List, 1).
spawn_children([], _) -> ok;
spawn_children([Head | Rest], I) ->
    spawn(get_child_node(I), sleepsort, child, [Head, self()]),
    spawn_children(Rest, I+1).

waitForSorted(List) -> waitForSorted(List, []).
waitForSorted(List, NewList) ->
if
    length(List) == length(NewList) -> lists:reverse(NewList);
    true ->
        receive
            {sorted, N} -> waitForSorted(List, [N | NewList])
        end
end.

child(N, Pid) ->
io:format(self(), "I am node ~p~n", [N]),
receive
after
    N ->
        io:format(self(), "I have waited for ~p milliseconds~n", [N]),
        Pid ! {sorted, N}
end.