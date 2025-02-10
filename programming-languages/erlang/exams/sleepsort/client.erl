-module(client).
-export([connect_server/1, client_proc/1, queries/1]).

connect_server(Server) ->
    register(client, spawn(client, client_proc, [{server, Server}])),
    case erlang:get_cookie(Server) of
        nocookie -> io:format("connection error~n", []);
        _ -> io:format("connection successfull~n", [])
    end.


client_proc(ServerPid) ->
receive
    Any -> ServerPid ! Any, client_proc(ServerPid)
end.

is_connected() ->
    case whereis(client) of
        undefined -> false;
        _ -> true
    end.

queries(Query) ->
    IsConnected = is_connected(),
    if
        IsConnected -> 
            case Query of
                quit -> client ! {stop, self()};
                List when is_list(List) -> client ! {sort, self(), List}
            end,
            waitForResponse();
        true -> throw("not connected to any server")
    end.

waitForResponse() ->
receive
    {sorted, SortedList} -> io:format("sorted list: ~p~n", [SortedList]);
    {stopped} -> io:format("server stopped~n"), unregister(client)
end.