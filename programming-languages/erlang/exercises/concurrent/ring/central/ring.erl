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
-export([start/3, manager/0, child/1]).

start(M, N, Message) ->
    register(manager, spawn(ring, manager, [])),
    io:format("Manager is ~w~n", [manager]),
    manager ! {create, N},
    manager ! {send, M, Message},
    manager ! {stop},
    ok.

manager() ->
    receive
        {create, N} ->
            Children = spawn_children(N, []),
            [First | _] = Children,
            setup_pids(First, Children),
            register(ring, First),
            manager();
        {send, M, Message} ->
            io:format("Manager starts sending messages~n"),
            send(ring, M, Message),
            io:format("Manager finished sending messages~n"),
            manager();
        {stop} ->
            ring ! {stop, self()}
    end.


spawn_children(0, Child_List) -> Child_List;
spawn_children(N, Child_List) ->
    spawn_children(N - 1, [spawn(ring, child, [[]])|Child_List]).


setup_pids(First, [Head | []]) ->
    Head ! {setNext, First};
setup_pids(First, [Head | Child_List]) ->
    [Next | _] = Child_List,
    Head ! {setNext, Next},
    setup_pids(First, Child_List).


send(_, 0, _) -> ok;
send(First, M, Message) ->
    First ! {message, self(), Message},
    io:format("Sending message Number ~w: ~w~n", [M, Message]),
    send(First, M-1, Message).


child(Next) ->
    receive
        {setNext, NextPid} ->
            io:format("~w linked to ~w~n",[self(), NextPid]),
            child([NextPid | Next]);
        {message, Pid, Message} ->
            io:format("Received message from Pid ~w: ~p~n", [Pid, Message]),
            [NextPid | _] = Next,
            NextPid ! {message, self(), Message},
            timer:sleep(500),
            child(Next);
        {stop, Pid} ->
            io:format("Received stop from Pid ~w~n", [Pid]),
            [NextPid | _] = Next,
            NextPid ! {stop, self()}
    end.