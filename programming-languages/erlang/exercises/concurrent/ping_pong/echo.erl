-module(echo).
-export([start/0, print/1, stop/0]).

start() ->
    Pid = spawn(server, start, []),
    register(pong, Pid),
    ok.

print(Term) ->
    pong ! {print, Term},
    ok.

stop() ->
    init:stop().