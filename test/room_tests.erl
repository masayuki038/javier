-module(room_tests).
-include_lib("eunit/include/eunit.hrl").
-include("message.hrl").

update_status_test_() ->
    {setup, fun start/0, fun stop/1,
        fun(_) -> 
            Clients = [{dummy1, <<"Foo">>}, {dummy2, <<"Bar">>}],
            storage:update_member(create_member_foo()),
            storage:update_member(create_member_bar()),
            storage:update_member(create_member_hoge()),
            States = room:update_status(Clients),
            ?_assertEqual(3, length(States)),
            [Ret1, Ret2, Ret3] = States,
            {Name1, Online1} = Ret1,
            ?_assertEqual(<<"Bar">>, Name1),
            ?_assertEqual(true, Online1),
            {Name2, Online2} = Ret2,
            ?_assertEqual(<<"Foo">>, Name2),
            ?_assertEqual(true, Online2),
            {Name3, Online3} = Ret3,
            ?_assertEqual(<<"Hog">>, Name3),
            ?_assertEqual(false, Online3)
        end
    }.
    
create_member_foo() ->
    #member{mail = "foo@foo.com", password = "foo", name = "Foo"}.

create_member_bar() ->
    #member{mail = "bar@bar.com", password = "bar", name = "Bar"}.

create_member_hoge() ->
    #member{mail = "hoge@hoge.co.jp", password = "hoge", name = "Hoge"}.

start() ->
    ?debugMsg("room_test:start called"),
    storage:clean_start(ram_copies).

stop(_) ->
    ?debugMsg("room_test:stop called"),
    storage:stop().
