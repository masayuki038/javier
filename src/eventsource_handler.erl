-module(eventsource_handler).

-export([init/3, info/3, terminate/3]).
-include("message.hrl").

init(_Transport, Req, []) ->
    Headers = [{<<"content-type">>, <<"text/event-stream">>}],
    {ok, Req2} = cowboy_req:chunked_reply(200, Headers, Req),
    Token = cowboy_req:header(Req, <<"token">>),
    if Token /= undefined ->
            erlang:send_after(1000, self(), {tick, Token}),
            {loop, Req2, undefined, 5000};
        true ->
            {shutdown, Req2, undefined}
    end.

info({tick, Token}, Req, State) ->
    Pid = whereis(Token),
    Pid ! {pull, self()},
    receive
        {Messages, MemberStates} ->
            if length(Messages) > 0 ->
                    Encoded = jsonx:encode([{<<"event">>, <<"message">>}, {<<"data">>, websocket_handler:to_binary_list(Messages)}]),
                    lager:info("Encoded: ~p", [Encoded]),
                    ok = cowboy_req:chunk(["id: ", id(), "\ndata: ", Encoded, "\n\n"], Req)
            end,
            if MemberStates /= undefined ->
                    Deployed = lists:map(fun({Member, Joined}) -> [{<<"member">>, Member}, {<<"joined">>, Joined}] end, MemberStates),
                    Encoded2 = jsonx:encode([{<<"event">>, <<"update_status">>}, {<<"data">>, [{<<"states">>, Deployed}]}]),
                    lager:info("Encoded: ~p", [Encoded2]),
                    ok = cowboy_req:chunk(["id: ", id(), "\ndata: ", Encoded2, "\n\n"], Req)
            end
    end,
    erlang:send_after(1000, self(), tick),
    {loop, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

id() ->
    {Mega, Sec, Micro} = erlang:now(),
    Id = (Mega * 1000000 + Sec) * 1000000 + Micro,
    integer_to_list(Id, 16).
