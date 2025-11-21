-module(exact_scenario_test).
-include_lib("eunit/include/eunit.hrl").

%% Test the exact scenario from the LUERL stack trace
exact_failing_scenario_test() ->
    LuaState = luerl:init(),
    
    % Load dump.lua exactly like the failing test
    {ok, DumpBin} = file:read_file("../src/dump.lua"),
    DumpSrc = binary_to_list(DumpBin),
    LoadCode = "do local dump_module = function() " ++ DumpSrc ++ " end; _G.dump = dump_module() end",
    {ok, _, LuaState2} = luerl:do(LoadCode, LuaState),
    
    % Use the exact table structure from the failing test
    Code = "
        local t = {
            user = {
                name = 'Alice',
                age = 30,
                address = {
                    street = '123 Main St',
                    city = 'NYC'
                }
            }
        }
        
        -- Call dump.dump and see what happens
        local success, result = pcall(dump.dump, t)
        return success, result
    ",
    
    Result = luerl:do(Code, LuaState2),
    io:format("Exact scenario test result: ~p~n", [Result]).

% Simplified test to verify dump module loads correctly
debug_dump_execution_test() ->
    LuaState = luerl:init(),

    % Load dump.lua
    {ok, DumpBin} = file:read_file("../src/dump.lua"),
    DumpSrc = binary_to_list(DumpBin),

    % Simple load test
    LoadCode = "do local dump_module = function() " ++ DumpSrc ++ " end; _G.dump = dump_module() end",
    case luerl:do(LoadCode, LuaState) of
        {ok, _, LuaState2} ->
            % Test basic dump functionality
            Code = "
                local t = {user = {name = 'Alice'}}
                return dump.dump(t)
            ",
            case luerl:do(Code, LuaState2) of
                {ok, [Result], _} when is_binary(Result) ->
                    io:format("Dump test passed: ~s~n", [Result]),
                    ok;
                {error, Error, _} ->
                    io:format("Dump execution error: ~p~n", [Error]),
                    {error, Error};
                Other ->
                    io:format("Dump execution unexpected result: ~p~n", [Other]),
                    {error, {unexpected_result, Other}}
            end;
        {error, LoadError, _} ->
            io:format("Dump load error: ~p~n", [LoadError]),
            {error, LoadError};
        Other ->
            io:format("Dump load unexpected result: ~p~n", [Other]),
            {error, {unexpected_load_result, Other}}
    end.

% Let's try to replicate the exact context that fails
context_replication_test() ->
    % The LUERL stack shows we're inside dumptbl at line 124 with specific args
    LuaState = luerl:init(),
    
    % Load dump module
    {ok, DumpBin} = file:read_file("../src/dump.lua"),
    DumpSrc = binary_to_list(DumpBin),
    LoadCode = "do local dump_module = function() " ++ DumpSrc ++ " end; _G.dump = dump_module() end",
    {ok, _, LuaState2} = luerl:do(LoadCode, LuaState),
    
    % Test the exact values from the stack trace: fieldIndent='    ', key='user'
    Code = "
        -- Simulate the exact context from line 124
        local fieldIndent = '    '
        local key = 'user'
        local val = {name = 'Alice'}  -- This gets passed to dumptbl recursively
        local depth = 2
        local nestIndent = '    '
        local ctx = {
            LF = '\n',
            circular = {},
            filter = function(v) return v end,
            udata = nil
        }
        
        -- This should be the exact call that fails
        local nested_result = dumptbl(val, depth, fieldIndent, nestIndent, ctx)
        print('Nested result type: ' .. type(nested_result))
        print('Nested result value: ' .. tostring(nested_result))
        
        -- This is line 124 that fails
        local kv = string.format('%s%s = %s', fieldIndent, key, nested_result)
        return kv
    ",
    
    Result = luerl:do(Code, LuaState2),
    io:format("Context replication result: ~p~n", [Result]).