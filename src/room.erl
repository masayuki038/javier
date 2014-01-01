-module(room).
-export([loop/1]).

-include("message.hrl").

loop(Clients) ->
    receive
        {join, {Pid, User}} ->
            lager:info("~p joined~n", [Pid]),
            NewClients = [{Pid, User} | Clients],
            publish([{Pid, User}], storage:get_messages()),
            ok = send_message(NewClients, <<"joined.">>, User),
            loop(NewClients);
        {message, {Content, User}} ->
            lager:info("~p(~p) received~n", [Content, User]),
            ok = send_message(Clients, Content, User),
            loop(Clients);
        {quit, Pid} ->
            lager:info("~p left~n", [Pid]),
            {NewClients, Removed} = lists:partition(
                fun({E, _User}) -> not (Pid =:= E) end, Clients
            ),
            lists:foreach(fun({_R, User}) -> send_message(NewClients, <<"quit.">>, User) end, Removed),
            loop(NewClients)
    end.

send_message(Clients, Content, User) ->
    Message = storage:create_message(Content, User),
    storage:update_message(Message),
    publish(Clients, [Message]).

publish([Client | Clients], Messages) ->
    lager:info("publish([Pid | Pids], Message)"),
    lager:info("Client: ~p", [Client]),
    {Pid, _Receiver} = Client,
    Pid ! {publish, Messages},
    publish(Clients, Messages);
publish([], _Messages) ->
    ok.


    
