-module(entry).
-export([new/1]).

new(Value) ->
    spawn_link(fun() -> init(Value) end). % with spawn_link(), if the creator of entry (store) dies, the entry dies too

init(Value) ->
    entry(Value, make_ref()). % second parameter(timestamp) is just for uniqueness

% looping process function receiving read/write/check messages from validator and sending responses back
% note that only the write operation makes a recursive call with a new timestamp
entry(Value, Time) ->
    receive
        {read, Ref, From} ->
            % a read request tagged with a reference. We will return a message tagged with the
            % reference so that the requester can identify the correct message. The reply will contain also the process
            % identifier of the entry, the value, and the current timestamp. The PID is needed for the asynchronous handler
            From ! {Ref, self(), Value, Time},
            entry(Value, Time); %
        {write, New} ->
            % update the current value of the entry, no reply needed. The timestamp of the entry will be updated.
            entry(New , make_ref()); % this is just a virtual entry, so only a reference is set, nothing written to DB
        {check, Ref, Readtime, From} ->
            % check if the timestamp of the entry has changed since we read the value (at time Readtime).
            if
            % tell the validator if the timestamp is still the same or if the client has to abort.
                Readtime == Time ->
                    From ! {Ref, ok};
                true ->
                    From ! {Ref, abort}
            end,
            entry(Value, Time);
        stop ->
            ok
    end.
