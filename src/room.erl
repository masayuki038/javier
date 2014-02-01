-module(room).
-export([loop/1]).
-export([update_status/1]).

-include("message.hrl").

loop(Clients) ->
    receive
        {join, {Pid, User}} ->
            lager:info("~p joined~n", [Pid]),
            NewClients = [{Pid, User} | Clients],
            publish([{Pid, User}], storage:get_messages()),
            ok = send_member_status({join, User}, NewClients),
            loop(NewClients);
        {message, {Content, User}} ->
            lager:info("~p(~p) received~n", [Content, User]),
            ok = send_message(Clients, Content, User),
            loop(Clients);
        {authenticate, {Pid, Input}} ->
            #member{mail = Mail, password = Password, name = Name} = Input,
            lager:info("~p try authentication~n", [Name]),
            case storage:get_member(Mail) of
                {ok, Member} ->
                    %% password check
                    #member{password = Password2} = Member,
                    case  Password =:= Password2 of
                        true -> 
                            Pid ! {authenticated, Member};
                        false ->
                            Pid ! {unauthenticated, Input}
                    end;
                {ng, _} ->
                    storage:update_member(Input),
                    Pid ! {authenticated, Input}
            end,
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

send_member_status(Event, Clients) ->
    States = update_status(Clients),
    lists:foreach(fun(Client) -> {Pid, _Member} = Client, Pid ! {update_status, Event, States} end, Clients),
    ok.

publish([Client | Clients], Messages) ->
    lager:info("publish([Pid | Pids], Message)"),
    lager:info("Client: ~p", [Client]),
    {Pid, _Receiver} = Client,
    Pid ! {publish, Messages},
    publish(Clients, Messages);
publish([], _Messages) ->
    ok.

update_status(Clients) ->
    Names = lists:map(fun(X) -> {_, User} = X, User end, Clients), 
    Members = lists:map(fun(X) -> #member{name = Name} = X, Name end, storage:get_members()),
    States = lists:map(fun(X) -> {X, lists:member(X, Names)} end, Members),
    lists:reverse(States).
