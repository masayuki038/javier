-module(eventsource_agent).

-export([loop/1]).

-include("message.hrl").

loop(State) ->
    receive
        {join, {Pid,User}} ->
            lager:info("~p joined~n", [Pid]),
            room ! {join, {self(), User}},
            loop(State);
        {message, {Content, User}} ->
            lager:info("~p(~p) received~n", [Content, User]),
            room ! {message, {Content, User}},
            loop(State);
        {reconnect, {_Pid, Mail, Token}} ->
            lager:info("~p(~p) reconnected~n", [Mail, Token]),
            room ! {reconnect, {self(), Mail, Token}},
            loop(State);
        {update_status, Event, States} ->
            lager:info("update_status, Evnet:~p, States:~p", [Event, States]),
            loop(State#state{member_states = States});
        {publish, Messages} ->
            lager:info("publish"),
            NewMessages = lists:append(Messages, State#state.messages),
            loop(State#state{messages = NewMessages});
        {pull, Pid} ->
            lager:info("pull"),
            Pid ! {State#state.messages, State#state.member_states},
            loop(State#state{messages = [], member_states = undefined});
        {quit, _Pid} ->
            lager:info("~p(~p) quit"),
            room ! {quit, self()},
            loop(State)
    end.
