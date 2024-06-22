-module(slave).
-export([start/3]).

rev(Str) -> rev(Str, []).
rev([], Acc) -> Acc;
rev([Head|Rest], Acc) -> rev(Rest, [Head|Acc]).

start(ServerPid, I, Sub) -> ServerPid ! {reversed, I, rev(Sub)}.