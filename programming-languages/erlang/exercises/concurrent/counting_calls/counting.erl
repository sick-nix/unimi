-module(counting).
-export([start/0, service/1, stop/0]).

start() ->
    Pid = spawn(server, start, []),
    register(serv, Pid),
    ok.

service(Service) ->
    serv ! {Service},
    ok.

stop() ->
    serv ! {stop},
    unregister(serv),
    ok.