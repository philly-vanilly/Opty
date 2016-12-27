-module(printer).
-export([start/2]).

start(Caller, NumberOfClients) ->
  {ok, FileRef} = file:open("output.txt", [write, append]),
  receive
    {starting, Clients, Entries, Reads, Writes, Time} ->
      io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w RDxTR, ~w WRxTR, DURATION ~w s~n",
        [Clients, Entries, Reads, Writes, Time]);
    stopping ->
      io:format("Stopping...~n");
    stop ->
      file:close(),
      io:format("Stopped~n");
    {clientExecution, ClientID, Total, Ok} ->
      io:format("~w: Transactions TOTAL:~w, OK:~w, -> ~w % ~n", [ClientID, Total, Ok, 100*Ok/Total]),
      updateStats(ClientID, Total, Ok, NumberOfClients, FileRef)
  end.

updateStats(ClientID, Total, Ok, NumberOfClients, FileRef) ->
  



