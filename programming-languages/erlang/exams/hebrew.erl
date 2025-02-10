% Create a distributed erlang program with N nodes
% There is a main process that spawns each of the N nodes
% The nodes are all on different beam nodes
% After spawning all processes the main process tells the nodes that each K node should be killed
% After a node is killed, it should still remain active
% After all nodes except one are dead we should print the ID of that node