% Write a module counting which provides the functionality for interacting with a server
%   that counts how many times its services has been requested.
% It has to implement several services dummy1, ... dummyn
%   (doesn't matter what they do or their real interface)
%   and a service tot that returns a list of records indexed on each service (tot included)
%   containing also how many times such a service has been requested. Test it from the shell.

-module(server).
-export([start/0]).

start() ->
    start(#{dummy1 => 0, dummy2 => 0, dummy3 => 0, dummy4 => 0, tot => 0}).

start(Count_Map) ->
    receive
        {dummy1} ->
            io:format("You called service dummy1~n"),
            start(Count_Map#{dummy1 := maps:get(dummy1, Count_Map) + 1});
        {dummy2} ->
            io:format("You called service dummy1~n"),
            start(Count_Map#{dummy2 := maps:get(dummy2, Count_Map) + 1});
        {dummy3} ->
            io:format("You called service dummy1~n"),
            start(Count_Map#{dummy3 := maps:get(dummy3, Count_Map) + 1});
        {dummy4} ->
            io:format("You called service dummy1~n"),
            start(Count_Map#{dummy4 := maps:get(dummy4, Count_Map) + 1});
        {tot} ->
            Map1 = Count_Map#{tot := maps:get(tot, Count_Map) + 1},
            io:format("You called service tot~n"),
            io:format("Printing total accesses to services ...~n"),
            lists:foreach(fun (T) -> io:format("~p", [T]) end, maps:to_list(Map1)),
            io:format("~n"),
            start(Map1);
        {stop} -> io:format("Stopping server ...~n");
        _ ->
            io:format("The requested service doesn't exist.~n"),
            start(Count_Map)
    end.