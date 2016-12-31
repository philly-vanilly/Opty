-module(client).
-export([start/6]).

% Client starts a transaction by creating a Handler and by retrieving Validator and Store from the Server. Those two are
% passed to the Handler

start(ClientID, Subset, Reads, Writes, Server, OutFd) ->
    spawn(fun() -> open(ClientID, Subset, Reads, Writes, Server, 0, 0, OutFd) end).

open(ClientID, Subset, Reads, Writes, Server, Total, Ok, OutFd) ->
    Server ! {open, self()},
    receive
        {stop, From} ->
            io:format(OutFd, "~w,~w,~w,~w~n", [ClientID, Total, Ok, 100*Ok/Total]),
            io:format("~w: Transactions TOTAL:~w, OK:~w, -> ~w % ~n", [ClientID, Total, Ok, 100*Ok/Total]),
            From ! {done, self()},
            ok;
        {transaction, Validator, Store} ->
            Handler = handler:start(self(), Validator, Store),
            case do_transaction(ClientID, Subset, Reads, Writes, Handler) of
                ok ->
                    open(ClientID, Subset, Reads, Writes, Server, Total+1, Ok+1, OutFd);
                abort ->
                    open(ClientID, Subset, Reads, Writes, Server, Total+1, Ok, OutFd)
            end
    end.

do_transaction(_, _, 0, 0, Handler) ->
    do_commit(Handler);
do_transaction(ClientID, Subset, 0, Writes, Handler) ->
    do_write(Subset, Handler, ClientID),
    do_transaction(ClientID, Subset, 0, Writes-1, Handler);
do_transaction(ClientID, Subset, Reads, 0, Handler) ->
    do_read(Subset, Handler),
    do_transaction(ClientID, Subset, Reads-1, 0, Handler);
do_transaction(ClientID, Subset, Reads, Writes, Handler) ->
    Op = rand:uniform(),
    if Op >= 0.5 ->
         do_read(Subset, Handler),
         do_transaction(ClientID, Subset, Reads-1, Writes, Handler);
       true -> 
         do_write(Subset, Handler, ClientID),
         do_transaction(ClientID, Subset, Reads, Writes-1, Handler)
    end.

do_read(Subset, Handler) ->
    Ref = make_ref(),
    {StartingAt, _} = Subset,
    {_, Range} = Subset,
    Num = rand:uniform(Range)-1+StartingAt, % -1 because rand:uniform(i) produces result from 1 to i, not 0 to i
    Handler ! {read, Ref, Num},
    receive
        {value, Ref, Value} -> Value
    end.

do_write(Subset, Handler, Value) ->
    {StartingAt, _} = Subset,
    {_, Range} = Subset,
    Num = rand:uniform(Range)-1+StartingAt,
    Handler ! {write, Num, Value}.

do_commit(Handler) ->
    Ref = make_ref(),
    Handler ! {commit, Ref},
    receive
        {Ref, Value} -> Value
    end.


    
