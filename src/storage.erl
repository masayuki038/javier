-module(storage).
-compile(export_all).

-include("message.hrl").
-include_lib("stdlib/include/qlc.hrl").

clean_start() ->
    Node = node(),
    mnesia:delete_schema([Node]),
        start().

start() ->
    Node = node(),
        case mnesia:create_schema([Node]) of
        ok ->
            prepare_db(true, Node);
        {error, {Node, {already_exists, Node}}} ->
            prepare_db(false, Node); 
        _ -> error
    end.

stop() ->
    mnesia:stop().

-spec prepare_db(_, _) -> any().
prepare_db(CreateTables, Node) ->
    ok = mnesia:start(),
    case CreateTables of
        true -> 
            ok = create_tables(Node);
        false -> ok
     end,
    mnesia:wait_for_tables([message], 20000).

-spec create_tables(_) -> ok.
create_tables(Node) ->
    mnesia:create_table(message, [{attributes, record_info(fields, message)}, {disc_copies, [Node]}]),
    ok.

-spec create_message(undefined | binary(), _) -> message().
create_message(Content, User) ->
    Now = erlang:localtime(),
    #message{message_id = generate_message_id(Content, Now), content = Content, user = User, at = iso8601:format(Now)}.

-spec generate_message_id(binary(), _) -> [integer()]. 
generate_message_id(Content, Seed) ->
    Md5_bin = erlang:md5(lists:concat([binary_to_list(Content), ";", tuple_to_list_deeply(Seed)])),
    Md5_list = binary_to_list(Md5_bin),
    lists:flatten(list_to_hex(Md5_list)).

-spec get_messages() -> list().
get_messages() ->
    take(10, do(qlc:sort(qlc:q([X || X <- mnesia:table(message)]),[{order,  
        fun(M1, M2) ->
            #message{at = At1} = M1,
            #message{at = At2} = M2,
            At1 > At2
        end}])
    )).

take(0, _) -> [];
take(_, []) -> [];
take(N, [X | Xs]) when N > 0 -> [X | take(N-1, Xs)].

-spec do(_) -> any().
do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
               
-spec update_message(_) -> {aborted, _} | {atomic, _}.
update_message(M) ->
    mnesia:transaction(fun() ->mnesia:write(M) end).

-spec list_to_hex([byte()]) -> [[integer(),...]].
list_to_hex(L) ->    
    lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0 + N;
hex(N) when N >= 10, N < 16 ->
    $a + (N-10).
                      
tuple_to_list_deeply([]) -> [];
tuple_to_list_deeply(T) when is_tuple(T) ->
    tuple_to_list_deeply(tuple_to_list(T));
tuple_to_list_deeply([H|L]) ->
    lists:concat([tuple_to_list_deeply(H), tuple_to_list_deeply(L)]);
tuple_to_list_deeply(T) ->
    T.
