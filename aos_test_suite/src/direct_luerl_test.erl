-module(direct_luerl_test).
-export([test/0]).

test() ->
    LuaState0 = luerl:init(),
    
    % Test different table formats
    io:format("Testing LUERL table formats:~n~n"),
    
    % Test 1: Direct proplist
    io:format("Test 1: Proplist format~n"),
    Table1 = [{"key", "value"}],
    {_, LuaState1} = luerl:set_table([test1], Table1, LuaState0),
    {[Result1], _} = luerl:do("return test1.key", LuaState1),
    io:format("  test1.key = ~p~n", [Result1]),
    
    % Test 2: Using luerl:encode
    io:format("~nTest 2: Using luerl:encode~n"),
    case catch luerl:encode(#{<<"key">> => <<"value">>}, LuaState0) of
        {ok, Encoded, _} ->
            io:format("  Encoded: ~p~n", [Encoded]);
        Error ->
            io:format("  Encode error: ~p~n", [Error])
    end,
    
    % Test 3: Manual table creation
    io:format("~nTest 3: Manual table creation~n"),
    TestCode = "
        test3 = {}
        test3.body = {}
        test3.body.action = 'eval'
        test3.body.data = 'return 42'
        return test3.body.action
    ",
    {[Result3], LuaState3} = luerl:do(TestCode, LuaState0),
    io:format("  Result: ~p~n", [Result3]),
    
    % Test 4: Check how aos.lua creates the same structure
    {ok, AosContent} = file:read_file("../aos.lua"),
    {_, LuaState4} = luerl:do(binary_to_list(AosContent), LuaState3),
    
    % Create assignment manually in Lua
    CreateAssignment = "
        local assignment = {}
        assignment.body = {
            action = 'eval',
            data = 'return 99',
            id = 'test',
            from = 'test',
            owner = 'test',
            tags = {}
        }
        
        local state = {
            results = {
                outbox = {},
                output = {data = '', prompt = ''}
            }
        }
        
        return compute(state, assignment)
    ",
    
    {[Status, Result], _} = luerl:do(CreateAssignment, LuaState4),
    io:format("~nTest 4: Direct Lua creation~n"),
    io:format("  Status: ~p~n", [Status]),
    
    ok.