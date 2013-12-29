-module(room).
-export([loop/1]).

-include("message.hrl").

loop(Pids) ->
    receive
        {join, Pid} ->
            lager:info("~p joined~n", [Pid]),
            loop([Pid | Pids]);
        {message, {Content, User}} ->
            lager:info("~p(~p) received~n", [Content, User]),
            Message = storage:create_message(Content, User),
            ok = publish(Pids, Message),
            loop(Pids)
    end.

publish([Pid | Pids], Message) ->
    lager:info("publish([Pid | Pids], Message)"),
    #message{content = Content, user = User, at = At} = Message,
    Pid ! {publish, {Content, User, At}},
    lager:info("~p(~p) published", [Content, User]),
    publish(Pids, Message);
publish([], _Message) ->
    ok.


    
