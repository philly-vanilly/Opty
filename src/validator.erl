-module(validator).
-export([start/0]).

% Validator tells if entries have been changed since the beginning of transaction. There is only one validator in system,
% the only one actually writing something to the actual remote entry (a store contains only local backup entries and the
% validator has no access to it)


% When we start the validator, we also link it to the process that creates it. This is to ensure that we don't have any
% zombie process.
start() ->
    spawn_link(fun() -> init() end).

init()->
    validator().

% Ref: a unique reference to tag the reply message.
% Reads: the list of read operations that have been performed. The validator must ensure that the entries of the read
% operations have not been changed.
% Writes: the pending write operations that, if the transaction is valid, should be applied to the store.
% Client: the process identifier of the client to whom we should return the reply.
validator() ->
    receive
        {validate, Ref, Reads, Writes, Client} ->
            Tag = make_ref(),
            send_read_checks(Reads, Tag),
            case check_reads(length(Reads), Tag) of
                ok ->
                    update(Writes),
                    Client ! {Ref, ok};
                abort ->
                    Client ! {Ref, abort}
            end,
            validator();
        stop ->
            ok;
        % Old messages that are still in the queue must be removed somehow, this
        % is why, and this is very important, the main loop of the validator includes
        % a catch all clause.
        _Old ->
            validator()
    end.

% Write operation is a result of a successful (not aborted) transaction
update(Writes) ->
    lists:foreach(
        fun({_, Entry, Value}) ->
            Entry ! {write, Value}
        end,
        Writes).

% For better performance the validator can first send check messages to all the entries in the read set and then collect
% the replies. As soon as one entry replies with an abort message, we're done. Tags used to be sure we count the right
% replies (not from a previous request)
send_read_checks(Reads, Tag) ->
    Self = self(),
    lists:foreach(fun({Entry, Time}) ->
        % Since a read operation is represented with a tuple {Entry, Time}, the validator needs only to send a check
        % message to the entry and make sure that the current timestamp of the entry is the same.
        Entry ! {check, Tag, Time, Self}
        end,
        Reads).

check_reads(0, _) ->
    ok;
check_reads(N, Tag) ->
    receive
        {Tag, ok} ->
            check_reads(N-1, Tag);
        {Tag, abort} ->
            abort
    end.
