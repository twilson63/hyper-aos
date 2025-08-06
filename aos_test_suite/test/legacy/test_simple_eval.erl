-module(test_simple_eval).
-export([test/0]).

test() ->
    LuaState0 = luerl:init(),
    {ok, AosContent} = file:read_file("../aos.lua"),
    {_, LuaState1} = luerl:do(binary_to_list(AosContent), LuaState0),
    
    %% Simple state
    State = #{
        <<"results">> => #{
            <<"outbox">> => [],
            <<"output">> => #{<<"data">> => <<"">>, <<"prompt">> => <<"">>}
        }
    },
    
    %% Simple message
    Message = #{
        <<"id">> => <<"test-msg">>,
        <<"from">> => <<"test">>,
        <<"owner">> => <<"test">>,
        <<"tags">> => [],
        <<"action">> => <<"eval">>,
        <<"data">> => <<"return 'hello'">>
    },
    
    Assignment = #{
        <<"process-id">> => <<"test-process-id">>,
        <<"timestamp">> => 1234567890,
        <<"block-height">> => 1000,
        <<"owner">> => <<"test-owner">>,
        <<"body">> => Message
    },
    
    %% Convert to Lua
    StateTable = erlang_to_lua_table(State),
    AssignmentTable = erlang_to_lua_table(Assignment),
    
    %% Call compute
    Result = luerl:call_function([compute], [StateTable, AssignmentTable], LuaState1),
    io:format("Raw result: ~p~n", [Result]),
    
    case Result of
        {ok, Values, _} ->
            io:format("Return values: ~p~n", [Values]),
            case Values of
                [Status, State] ->
                    io:format("Status: ~p~n", [Status]),
                    io:format("State (raw): ~p~n", [State]);
                _ ->
                    io:format("Unexpected return format~n")
            end;
        Error ->
            io:format("Error: ~p~n", [Error])
    end.

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
erlang_to_lua_value(V) when is_list(V) ->
    V;
erlang_to_lua_value(V) when is_binary(V) ->
    binary_to_list(V);
erlang_to_lua_value(V) when is_atom(V) ->
    atom_to_list(V);
erlang_to_lua_value(V) ->
    V.