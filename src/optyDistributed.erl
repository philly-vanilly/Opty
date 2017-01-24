-module(optyDistributed).
-export([startServer/1, startAllClients/4,  stop/2]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)

startServer(NumberOfEntries) -> % start with arguments: erl -name server@127.0.0.1 -setcookie secret
    Pid = spawn('server@127.0.0.1', server, start, [NumberOfEntries]),
    global:register_name(serverPid, Pid),
    io:format("Server running on: ~w Server PID: ~w~n", [node(), global:whereis_name(serverPid)]).

startAllClients(Subsets, Reads, Writes, Time) ->
    {ok, {StartingAt, Range}} = list_max(Subsets),
    Max = StartingAt+Range-1,
    {ok,OutFd} = file:open("output.txt", [write, append]),
    io:format("Print on ~w~n", [OutFd]),
    L = startClients(Subsets, [], Reads, Writes, OutFd),
    io:format(OutFd, "~w CLIENTS, ~w SUBSETS, ~w ENTRIES, ~w RDxTR, ~w WRxTR, DURATION ~w s~n", [length(Subsets), Subsets, Max, Reads, Writes, Time]),
    io:format("Starting: ~w CLIENTS, ~w SUBSETS, ~w ENTRIES, ~w RDxTR, ~w WRxTR, DURATION ~w s~n", [length(Subsets), Subsets, Max, Reads, Writes, Time]),
    timer:sleep(Time*1000),
    stop(L, OutFd).

startClients([], L, _, _, _) -> L;
startClients(Subsets, L, Reads, Writes, OutFd) ->
    net_kernel:connect_node('server@127.0.0.1'),
    io:format("Connected to: ~w~n", [nodes()]),
    ServerPid = global:whereis_name(serverPid),
    Server = {ServerPid, 'server@127.0.0.1'},
    io:format("Server PID on Client: ~w~n", [Server]),
    ClientPid = spawn('clients@127.0.0.1', client, start, [length(Subsets), hd(Subsets), Reads, Writes, Server, OutFd]),
    startClients(tl(Subsets), [ClientPid|L], Reads, Writes, OutFd).

stopClients([]) ->
    ok;
stopClients([Pid|L]) ->
    Pid ! {stop, self()},	
    stopClients(L).

waitClients([]) ->
    ok;
waitClients(L) ->
    receive
        {done, Pid} ->
            waitClients(lists:delete(Pid, L))
    end.

stop(L, OutFd) ->
    io:format(OutFd, "Stopping...~n", []),
    io:format("Stopping...~n"),
    stopClients(L),
    waitClients(L),
    %global:whereis_name(serverPid) ! stop,
    io:format(OutFd, "Stopped~n~n", []),
    io:format("Stopped~n"),
    file:close(OutFd).

list_max([]) -> empty;
list_max([H|T]) -> {ok, list_max(H, T)}.
list_max(X,[]) -> X;
list_max({StartingAtOne, RangeOne}, [{StartingAtTwo, RangeTwo}|T])
    when (StartingAtOne+RangeOne) < (StartingAtTwo+RangeTwo) ->
    list_max({StartingAtTwo, RangeTwo}, T);
list_max(X,[_|T]) -> list_max(X, T).