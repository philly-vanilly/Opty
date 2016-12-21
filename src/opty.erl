-module(opty).
-export([start/5, stop/2]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)

start(Clients, Entries, Reads, Writes, Time) ->
    register(s, server:start(Entries)),
    {ok,OutFd} = file:open("output.txt", [write, append]),
    L = startClients(Clients, [], Entries, Reads, Writes, OutFd),
    io:format(OutFd, "Starting: ~w CLIENTS, ~w ENTRIES, ~w RDxTR, ~w WRxTR, DURATION ~w s~n", [Clients, Entries, Reads, Writes, Time]),
    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w RDxTR, ~w WRxTR, DURATION ~w s~n", [Clients, Entries, Reads, Writes, Time]),
    timer:sleep(Time*1000),
    stop(L, OutFd).

stop(L, OutFd) ->
    io:format(OutFd, "Stopping...~n", []),
    io:format("Stopping...~n"),
    stopClients(L),
    waitClients(L),
    s ! stop,
    io:format(OutFd, "Stopped~n~n", []),
    io:format("Stopped~n"),
    file:close(OutFd).

startClients(0, L, _, _, _, _) -> L;
startClients(Clients, L, Entries, Reads, Writes, OutFd) ->
    Pid = client:start(Clients, Entries, Reads, Writes, s, OutFd),
    startClients(Clients-1, [Pid|L], Entries, Reads, Writes, OutFd).

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
