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
    erlang:start_timer(1000, self(), Msg),
    {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    Decoded = jsonx:decode(Msg),
    lager:info("Decoded: ~p~n", [Decoded]),
    case Decoded of
        {[{<<"event">>, <<"send_message">>}, {<<"data">>, {[{<<"message">>, Content}, {<<"user">>, User}]}}]} ->
            {reply, {text, jsonx:encode([{<<"event">>, <<"receive_message">>}, {<<"data">>, {[{<<"message">>, Content}, {<<"user">>, User}]}}])}, Req, State}
    end;
websocket_info(_Info, Req, State) ->    
    lager:info("websocket_info/3"),
    {ok, Req, State}.
    
websocket_terminate(_Reason, _Req, _State) -> 
    lager:info("websocket_terminate/3"),
    ok.
