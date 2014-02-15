-module(eventsource_request_handler).

-export([init/3, handle/2, terminate/3]).
-include("message.hrl").

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    Token = cowboy_req:header(Req, <<"token">>),
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}]),
    {ok, PostVals, Req3} = cowboy_req:body_qs(Req2),
    Json = proplists:get_value(<<"request">>, PostVals),
    Decoded = jsonx:decode(Json),
    lager:info("Decoded: ~p~n", [Decoded]),
    case Decoded of
        {[{<<"event">>, <<"send_message">>}, {<<"data">>, {[{<<"message">>, Content}, {<<"user">>, User}]}}]} ->
            whereis(Token) ! {message, {Content, User}},
            {ok, Req4} = on_sent_message(Req3),
            {ok, Req4, State};
        {[{<<"event">>, <<"authenticate">>}, {<<"data">>, Data}]} ->
            {[{<<"mail">>, Mail}, {<<"password">>, Password}, {<<"name">>, Name}, {<<"update">>, Update}]} = Data,
            room ! {authenticate, {self(), #member{mail = Mail, password = Password, name = Name}, Update}},
            receive
                {authenticated, Member} ->
                    {ok, Req4} = on_authenticated(Member, Token, Req3),
                    {ok, Req4, State};
                {unauthenticated, Member} ->
                    {ok, Req4} = on_unauthenticated(Member, Req3),
                    {ok, Req4, State}
            end;
        {[{<<"event">>, <<"reconnect">>}, {<<"data">>, {[{<<"mail">>, Mail}]}}]} ->
            room ! {reconnect, {self(), Mail, Token}},
            receive
                {authenticated, Member} ->
                    {ok, Req4} = on_authenticated(Member, Token, Req3),
                    {ok, Req4, State};
                {unauthenticated, Member} ->
                    {ok, Req4} = on_unauthenticated(Member, Req3),
                    {ok, Req4, State}
            end
    end.

terminate(_Reason, _Req, _State) ->
    ok.

on_authenticated(Member, Token, Req) ->
    lager:info("websocket_info({authenticated, Member}, Req, State)"),
    lager:info("Member: ~p", [Member]),
    Encoded = jsonx:encode([{<<"event">>, <<"authenticated">>}, {<<"data">>, [{<<"token">>, Member#member.token}, {<<"mail">>, Member#member.mail}, {<<"name">>, Member#member.name}]}]),
    lager:info("Encoded: ~p", [Encoded]),
    agent(Token) ! {join, {self(), Member#member.name}},
    Encoded = jsonx:encode([{<<"event">>, <<"authenticated">>}, {<<"data">>, []}]),
    cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], Encoded, Req).

on_unauthenticated(Member, Req) ->
    lager:info("websocket_info({unauthenticated, Member}, Req, State)"),
    lager:info("Member: ~p", [Member]),
    Encoded = jsonx:encode([{<<"event">>, <<"unauthenticated">>}, {<<"data">>, [{<<"error">>, <<"Authentication Failed">>}]}]),
    cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], Encoded, Req).

on_sent_message(Req) ->
    lager:info("on_sent_message(Req)"),
    Encoded = jsonx:encode([{<<"event">>, <<"sent_message">>}, {<<"data">>, []}]),
    cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], Encoded, Req).    

agent(Token) ->
    Pid = whereis(Token),
    if Pid =:= undefined ->
            register(Token, spawn(fun() -> eventsource_agent:loop(#state{messages = [], member_states = undefined}) end));
        true ->
            Pid
    end.
