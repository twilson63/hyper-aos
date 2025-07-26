-module(test_with_conversion).
-export([test/0]).

test() ->
    LuaState0 = luerl:init(),
    {ok, AosContent} = file:read_file("../aos.lua"),
    {_, LuaState1} = luerl:do(binary_to_list(AosContent), LuaState0),
    
    % Create state using the same structure as simple_security_test
    State = #{
        <<"results">> => #{
            <<"outbox">> => [],
            <<"output">> => #{<<"data">> => <<"">>, <<"prompt">> => <<"">>}
        }
    },
    
    Message = #{
        <<"id">> => <<"test">>,
        <<"from">> => <<"test">>,
        <<"owner">> => <<"test">>,
        <<"tags">> => [],
        <<"action">> => <<"eval">>,
        <<"data">> => <<"return 42">>
    },
    
    Assignment = #{
        <<"body">> => Message
    },
    
    % Convert using the helper function from simple_security_test
    StateTable = erlang_to_lua_table(State),
    AssignmentTable = erlang_to_lua_table(Assignment),
    
    io:format("StateTable: ~p~n", [StateTable]),
    io:format("AssignmentTable: ~p~n", [AssignmentTable]),
    
    % Call compute
    {[Status, Result], _} = luerl:call_function([compute], [StateTable, AssignmentTable], LuaState1),
    io:format("~nStatus: ~p~n", [Status]),
    
    % Convert result and check
    ErlangResult = lua_table_to_erlang(Result),
    case maps:get(<<"results">>, ErlangResult, undefined) of
        undefined ->
            io:format("No results~n");
        Results ->
            case maps:get(<<"output">>, Results, undefined) of
                undefined ->
                    io:format("No output~n");
                Output ->
                    Data = maps:get(<<"data">>, Output, <<"no data">>),
                    io:format("Output data: ~p~n", [Data])
            end
    end,
    
    ok.

% Helper functions from simple_security_test
erlang_to_lua_table(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) ->
        Key = if
            is_binary(K) -> binary_to_list(K);
            is_atom(K) -> atom_to_list(K);
            true -> K
        end,
        Value = erlang_to_lua_value(V),
        [{Key, Value} | Acc]
    end, [], Map);
erlang_to_lua_table(List) when is_list(List) ->
    List.

erlang_to_lua_value(V) when is_map(V) ->
    erlang_to_lua_table(V);
erlang_to_lua_value([]) ->
    [];
erlang_to_lua_value(V) when is_list(V) ->
    case V of
        [H|_] when is_map(H) ->
            lists:map(fun erlang_to_lua_value/1, V);
        _ ->
            V
    end;
erlang_to_lua_value(V) when is_binary(V) ->
    binary_to_list(V);
erlang_to_lua_value(V) when is_atom(V) ->
    atom_to_list(V);
erlang_to_lua_value(V) ->
    V.

lua_table_to_erlang(Table) when is_list(Table) ->
    lists:foldl(fun
        ({K, V}, Acc) when is_integer(K) ->
            Acc;
        ({K, V}, Acc) when is_list(K) ->
            case catch list_to_binary(K) of
                Key when is_binary(Key) ->
                    Value = lua_value_to_erlang(V),
                    maps:put(Key, Value, Acc);
                _ ->
                    Acc
            end;
        ({K, V}, Acc) ->
            Value = lua_value_to_erlang(V),
            maps:put(K, Value, Acc)
    end, #{}, Table);
lua_table_to_erlang(Value) ->
    Value.

lua_value_to_erlang(V) when is_list(V) ->
    case is_lua_table(V) of
        true -> lua_table_to_erlang(V);
        false -> 
            try list_to_binary(V)
            catch _:_ -> V
            end
    end;
lua_value_to_erlang(V) ->
    V.

is_lua_table([]) -> true;
is_lua_table([{_, _} | _]) -> true;
is_lua_table(_) -> false.