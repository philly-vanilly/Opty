-module(opty).
-export([start/5, stop/2]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)

start(NumberOfClients, Entries, Reads, Writes, Time) ->
    spawn_link(fun() -> Printer = printer:start(self(), NumberOfClients) end),
    register(s, server:start(Entries)),
    L = startClients(NumberOfClients, [], Entries, Reads, Writes, Printer),
    Printer ! {starting, NumberOfClients, Entries, Reads, Writes, Time},
    timer:sleep(Time*1000),
    stop(L,Printer).

stop(L, Printer) ->
    Printer ! stopping,
    stopClients(L),
    waitClients(L),
    s ! stop,
    Printer ! stop.

startClients(0, L, _, _, _, Printer) -> L;
startClients(NumberOfClients, L, Entries, Reads, Writes, Printer) ->
    Pid = client:start(NumberOfClients, Entries, Reads, Writes, s, Printer),
    startClients(NumberOfClients -1, [Pid|L], Entries, Reads, Writes, Printer).

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
