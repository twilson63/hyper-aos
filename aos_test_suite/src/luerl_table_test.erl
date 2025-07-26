-module(luerl_table_test).
-export([test/0]).

test() ->
    LuaState0 = luerl:init(),
    
    % Test different ways to pass tables to LUERL
    TestCode = "
        function inspect_table(name, t)
            print(name .. ':')
            print('  Type: ' .. type(t))
            if type(t) == 'table' then
                for k, v in pairs(t) do
                    print('  [' .. tostring(k) .. '] = ' .. tostring(v))
                end
            end
            return t
        end
    ",
    
    {_, LuaState1} = luerl:do(TestCode, LuaState0),
    
    % Test 1: Empty table/list
    io:format("Test 1: Empty list~n"),
    {_, _} = luerl:call_function([inspect_table], [<<"Empty">>, []], LuaState1),
    
    % Test 2: List with tuples (proplist style)
    io:format("~nTest 2: Proplist style~n"),
    {_, _} = luerl:call_function([inspect_table], ["Proplist", [{"key", "value"}]], LuaState1),
    
    % Test 3: List with integer keys
    io:format("~nTest 3: Array style~n"),
    {_, _} = luerl:call_function([inspect_table], ["Array", [{1, "first"}, {2, "second"}]], LuaState1),
    
    % Test 4: Check LUERL table construction
    io:format("~nTest 4: Using luerl:make_table~n"),
    case catch luerl:make_table([{"key", "value"}], LuaState1) of
        {Table, NewState} ->
            {_, _} = luerl:call_function([inspect_table], ["MakeTable", Table], NewState);
        Error ->
            io:format("make_table error: ~p~n", [Error])
    end,
    
    % Test 5: Manual construction in Lua
    io:format("~nTest 5: Manual Lua construction~n"),
    ManualCode = "
        local t = {}
        t.key = 'value'
        t.nested = {}
        t.nested.action = 'eval'
        inspect_table('Manual', t)
        return t
    ",
    {[ManualTable], LuaState5} = luerl:do(ManualCode, LuaState1),
    
    % Test 6: Pass the manual table back
    io:format("~nTest 6: Passing Lua table back~n"),
    {_, _} = luerl:call_function([inspect_table], ["Passed back", ManualTable], LuaState5),
    
    ok.