-module(server).
-export([start/1]).

% Transaction Server to be used by the client. Creates and serves (strong relationship) Store and Validator to the Client.
% Server does not use Store or Client itself.

start(N) ->
    spawn(fun() -> init(N) end).

init(N) ->
    Store = store:new(N),
    Validator = validator:start(),
    server(Validator, Store).
    
server(Validator, Store) ->
    receive 
        {open, Client} ->
            %% TODO: ADD SOME CODE
            server(Validator, Store);
        stop ->
            Validator ! stop,
            store:stop(Store)
    end.
