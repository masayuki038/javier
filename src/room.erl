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

publish([Client | Clients], Messages) ->
    lager:info("publish([Pid | Pids], Message)"),
    lager:info("Client: ~p", [Client]),
    {Pid, _Receiver} = Client,
    Pid ! {publish, Messages},
    publish(Clients, Messages);
publish([], _Messages) ->
    ok.


    
