% Write a program that will create N processes connected in a ring.
% Once started, these processes will send M number of messages around the ring
%   and then terminate gracefully when they receive a quit message.
% You can start the ring with the call ring:start(M, N, Message).

% There are two basic strategies to tackling this exercise.
% The first one is to have a central process that sets up the ring and initiates sending the message.
% The second strategy consists of the new process spawning the next process in the ring.
% With this strategy, you have to find a method to connect the first process to the second process.

% Try to solve the exercise in both manners.
% Note, when writing your program, make sure your code has many io:format statements in every loop iteration;
% this will give you a complete overview of what is happening (or not happening) and should help you solve the exercise.

-module(ring).
-export([start/3, child/1, child/2]).

start(M, N, Message) ->
    First = spawn(ring, child, [N]),
    First ! {message, self(), M, Message},
    ok.


child(Count) ->
    io:format("~p: Spawned ~p~n", [self(), Count]),
    Pid = spawn(ring, child, [Count-1, self()]),
    loop(Pid, self()).

child(1, First) ->
    io:format("~p: Spawned 1~n", [self()]),
    loop(First, First);
child(Count, First) ->
    io:format("~p: Spawned ~p~n", [self(), Count]),
    Pid = spawn(ring, child, [Count-1, First]),
    loop(Pid, First).


loop(NextPid, First) ->
    receive
        {message, Pid, M, Message} ->
            io:format("~p: Received message (~w) from Pid ~w: ~p~n", [self(), M, Pid, Message]),
            if
                % last child in ring
                First == NextPid andalso M > 0 -> First ! {message, self(), M - 1, Message};
                First == NextPid -> First ! {stop, self()};
                true -> NextPid ! {message, self(), M, Message}
            end,
            loop(NextPid, First);
        {stop, Pid} ->
            io:format("Received stop from Pid ~w~n", [Pid]),
            if
                NextPid /= First -> NextPid ! {stop, self()};
                true -> ok
            end
    end.