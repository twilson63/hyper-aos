-module(verify_conversion).
-export([test/0]).

test() ->
    LuaState0 = luerl:init(),
    
    % Test the conversion with a simple example
    TestMap = #{<<"body">> => #{<<"action">> => <<"eval">>}},
    ConvertedTable = erlang_to_lua_table(TestMap),
    
    io:format("Original map: ~p~n", [TestMap]),
    io:format("Converted table: ~p~n", [ConvertedTable]),
    
    % Set it in Lua and test access
    TestCode = "
        function test_access(t)
            print('Type of t: ' .. type(t))
            print('Type of t.body: ' .. type(t.body))
            if t.body then
                print('Type of t.body.action: ' .. type(t.body.action))
                print('Value of t.body.action: ' .. tostring(t.body.action))
            end
            return 'done'
        end
    ",
    
    {_, LuaState1} = luerl:do(TestCode, LuaState0),
    
    % Call the test function
    {[Result], _} = luerl:call_function([test_access], [ConvertedTable], LuaState1),
    io:format("Result: ~p~n", [Result]),
    
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
    end, [], Map).

erlang_to_lua_value(V) when is_map(V) ->
    erlang_to_lua_table(V);
erlang_to_lua_value(V) when is_binary(V) ->
    binary_to_list(V);
erlang_to_lua_value(V) ->
    V.