-module(store).
-export([new/1, stop/1, lookup/2]).

% Store is used for backup in case there was a change in an accessed entry during the access. Holds a set of tuples
% containing a value and a unique reference to an actual Entries. The reference is updated with every write operation and
% can be compared with the value to check if such an operation occurred in the meantime.
% One store can be referenced by multiple Servers/Handlers, who send read messages to the actual entries.

new(N) ->
    list_to_tuple(entries(N, [])).

stop(Store) ->
    lists:foreach(fun(E) -> 
                    E ! stop 
                  end, 
                  tuple_to_list(Store)).

lookup(I, Store) ->
    element(I, Store). % this is a builtin function

entries(0, ListSoFar) ->
    ListSoFar;
entries(N, ListSoFar) ->
    Entry = entry:new(0),
    entries(N-1, [Entry|ListSoFar]).
