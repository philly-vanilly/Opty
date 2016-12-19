-module(entry).
-export([new/1]).

new(Value) ->
    spawn_link(fun() -> init(Value) end). % with spawn_link(), if the creator of entry (store) dies, the entry dies too

init(Value) ->
    entry(Value, make_ref()). % second parameter(timestamp) is just for uniqueness

% looping process function receiving read/write/check messages from validator and sending responses back
entry(Value, Time) ->
    %note that only the write operation makes a recursive call with a new timestamp
    receive
        {read, Ref, From} ->
            % a read request tagged with a reference. We will return a message tagged with the
            % reference so that the requester can identify the correct message. The reply will contain also the process
            % identifier of the entry, the value, and the current timestamp. The PID is needed for the asynchronous handler

            %% TODO: ADD SOME CODE
            entry(Value, Time); %
        {write, New} ->
            % update the current value of the entry, no reply needed. The timestamp of the entry will be updated.
            entry(... , make_ref());  %% TODO: COMPLETE
        {check, Ref, Readtime, From} ->
            % check if the timestamp of the entry has changed since we read the value (at time Readtime).
            if 
                 ... == ... ->   %% TODO: COMPLETE
                    %% TODO: ADD SOME CODE
                true ->
                    From ! {Ref, abort} %tell the validator if the timestamp is still the same or if the client has to abort.
            end,
            entry(Value, Time);
        stop ->
            ok
    end.
