-module(opty).
-export([start/4, stop/2]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)

start(Subsets, Reads, Writes, Time) ->
    {ok, {StartingAt, Range}} = list_max(Subsets),
    Max = StartingAt+Range-1,
    register(s, server:start(Max)),
    {ok,OutFd} = file:open("output.txt", [write, append]),
    L = startClients(Subsets, [], Reads, Writes, OutFd),
    io:format(OutFd, "~w CLIENTS, ~w SUBSETS, ~w ENTRIES, ~w RDxTR, ~w WRxTR, DURATION ~w s~n", [length(Subsets), Subsets, Max, Reads, Writes, Time]),
    io:format("Starting: ~w CLIENTS, ~w SUBSETS, ~w ENTRIES, ~w RDxTR, ~w WRxTR, DURATION ~w s~n", [length(Subsets), Subsets, Max, Reads, Writes, Time]),
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

startClients([], L, _, _, _) -> L;
startClients(Subsets, L, Reads, Writes, OutFd) ->
    Pid = client:start(length(Subsets), hd(Subsets), Reads, Writes, s, OutFd),
    startClients(tl(Subsets), [Pid|L], Reads, Writes, OutFd).

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

list_max([]) -> empty;
list_max([H|T]) -> {ok, list_max(H, T)}.
list_max(X,[]) -> X;
list_max({StartingAtOne, RangeOne}, [{StartingAtTwo, RangeTwo}|T])
    when (StartingAtOne+RangeOne) < (StartingAtTwo+RangeTwo) ->
    list_max({StartingAtTwo, RangeTwo}, T);
list_max(X,[_|T]) -> list_max(X, T).