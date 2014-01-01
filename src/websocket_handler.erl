-module(websocket_handler).

-behaviour(cowboy_websocket_handler).
-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    lager:info("init/3"),
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    lager:info("websocket_init/3"),
    {ok, Req, undefined_state}.
 
websocket_handle({text, Msg}, Req, State) ->
    lager:info("websocket_handle/3"),
    lager:info(Msg),
    Decoded = jsonx:decode(Msg),
    lager:info("Decoded: ~p~n", [Decoded]),
    case Decoded of
        {[{<<"event">>, <<"send_message">>}, {<<"data">>, {[{<<"message">>, Content}, {<<"user">>, User}]}}]} ->
            room ! {message, {Content, User}};
        {[{<<"event">>, <<"join">>}, {<<"data">>, {[{<<"user">>, User}]}}]} ->
            room ! {join, {self(), User}}
    end,
    {ok, Req, State}.

websocket_info({publish, Messages}, Req, State) ->
    lager:info("websocket_info({publish, Messages}, Req, State)"),
    lager:info("Messages: ~p", [Messages]),
    Encoded = jsonx:encode([{<<"event">>, <<"message">>}, {<<"data">>, to_binary_list(Messages)}]),
    lager:info("Encoded: ~p", [Encoded]),
    {reply, {text, Encoded}, Req, State};
websocket_info(_Info, Req, State) ->    
    lager:info("websocket_info/3"),
    {ok, Req, State}.
    
websocket_terminate(_Reason, _Req, _State) -> 
    lager:info("websocket_terminate/3"),
    room ! {quit, self()},
    ok.

to_binary_list(Messages) ->
    to_binary_list(Messages, []).

to_binary_list([Message | Messages], Ret) ->
    {message, _, Content, User, At} = Message,
    to_binary_list(Messages, [{[{<<"message">>, Content}, {<<"user">>, User}, {<<"at">>, At}]} | Ret]);
to_binary_list([], Ret) ->
    Ret.
