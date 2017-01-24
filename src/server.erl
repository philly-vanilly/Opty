-module(server).
-export([start/1]).

% Transaction Server to be used by the client. Creates and serves (strong relationship) Store and Validator to the Client.
% Server does not use Store or Client itself.


% If the server creates the transaction handler, we must let the handler
% be independent from the server (not linked), because if the handler dies we
% don't want the server to die. On the other hand, if the client dies we do
% want the handler to die. The solution is to let the process of the client create
% the handler when it receives the info about the validator and the store from
% the server. Then, it moves to a loop where it executes transactions until a
% stop message is received.

start(N) ->
    Pid = spawn(fun() -> init(N) end),
    Pid.

init(N) ->
    Store = store:new(N),
    Validator = validator:start(),
    server(Validator, Store).

server(Validator, Store) ->
    receive 
        {open, Client} ->
            Client ! {transaction, Validator, Store},
            server(Validator, Store);
        stop ->
            Validator ! stop,
            store:stop(Store)
    end.