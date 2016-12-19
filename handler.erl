-module(handler).
-export([start/3]).

% Handles read and write requests from the client (to the local store). When the transaction is closed, the handler
% sends the read and write sets to the validator (commit is the request permanent change of remote store).

% A client should never access the store directly. It will perform all operations through a transaction handler. A
% transaction handler is created for a specific client and holds the store and the process identifier of the validator.
% We will implement the handler so that a client can make asynchronous reads to the store. If latencies are high, there
% is no point in waiting for one read operation to complete before initiating a second operation.

% The task of the transaction handler is to record all read operations (and at what time these took place) and make
% write operations only visible in a local store. In order to achieve this, the handler will keep two sets: the read set
% (Reads) and the write set (Writes). The read set is a list of tuples {Entry, Time} and the write set is a list of
% tuples {N, Entry, Value}. When it is time to commit, the handler sends the read and write sets to the validator.

start(Client, Validator, Store) ->
    spawn_link(fun() -> init(Client, Validator, Store) end).

init(Client, Validator, Store) ->
    handler(Client, Validator, Store, [], []).

handler(Client, Validator, Store, Reads, Writes) ->         
    receive
        % a read request from the client containing a reference that we should use in the reply message. The integer N
        % is the index  of the entry in the store. The handler should rst look through the write set to see if entry N has
        % been written. In this case, the written value is returned to the client. If no matching operation is found, a
        % message is sent to the Nth entry process in the store. This entry will reply to the handler since we need to
        % record the read time.
        {read, Ref, N} ->
            case lists:keyfind(N, 1, Writes) of
                {N, _, Value} ->
                    Client ! {value, Ref, Value},
                    handler(Client, Validator, Store, Reads, Writes);
                false ->
                    Entry = store:lookup(N, Store),
                    Entry ! {read, Ref, self()},
                    handler(Client, Validator, Store, Reads, Writes)
            end;
        % reply from entry, which is forwarded to client AND {Entry, Time} saved in read set
        {Ref, Entry, Value, Time} ->
            Client ! {value, Ref, Value},
            handler(Client, Validator, Store, [{Entry, Time}|Reads], Writes);
        % a write request from the client. The integer N is the index of the entry in the store and Value, the new value.
        {write, N, Value} ->
            Entry = store:lookup(N, Store),
            Added = lists:keystore(N, 1, Writes, {N, Entry, Value}),
            handler(Client, Validator, Store, Reads, Added);
        % a commit request from the client. This is the time to contact the validator and see if there are any conicts
        % in our read set. If not, the validator will perform the write operations in the write set and reply directly
        % to the client.
        {commit, Ref} ->
            Validator ! {validate, Ref, Reads, Writes, Client};
        abort ->
            ok
    end.
