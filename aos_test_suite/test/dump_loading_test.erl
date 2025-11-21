-module(dump_loading_test).
-include_lib("eunit/include/eunit.hrl").

%% Test different ways of loading dump.lua
direct_load_test() ->
    LuaState = luerl:init(),
    
    {ok, DumpBin} = file:read_file("../src/dump.lua"),
    DumpSrc = binary_to_list(DumpBin),
    
    % Load directly without wrapping
    try
        {ok, [DumpModule], LuaState2} = luerl:do(DumpSrc, LuaState),
        io:format("Direct load result: ~p~n", [DumpModule]),
        
        % Try to use the dump function
        Code = "_G.dump = ...; local t = {a = 1}; return dump.dump(t)",
        Result = luerl:do(Code, [DumpModule], LuaState2),
        io:format("Usage result: ~p~n", [Result])
    catch
        Error:Reason ->
            io:format("Direct load failed: ~p:~p~n", [Error, Reason])
    end.

wrapped_load_test() ->
    LuaState = luerl:init(),
    
    {ok, DumpBin} = file:read_file("../src/dump.lua"),
    DumpSrc = binary_to_list(DumpBin),
    
    % Load with wrapping like in aos_dump_test
    LoadCode = "do local dump_module = function() " ++ DumpSrc ++ " end; return dump_module() end",
    
    try
        Result = luerl:do(LoadCode, LuaState),
        io:format("Wrapped load result: ~p~n", [Result])
    catch
        Error:Reason ->
            io:format("Wrapped load failed: ~p:~p~n", [Error, Reason])
    end.

simple_load_test() ->
    LuaState = luerl:init(),
    
    {ok, DumpBin} = file:read_file("../src/dump.lua"),
    DumpSrc = binary_to_list(DumpBin),
    
    % Simple load and assign
    LoadCode = "local dump = (" ++ DumpSrc ++ "); _G.dump = dump",
    
    try
        {ok, _, LuaState2} = luerl:do(LoadCode, LuaState),
        
        % Try to use it
        TestCode = "local t = {a = 1}; return dump.dump(t)",
        Result = luerl:do(TestCode, LuaState2),
        io:format("Simple load test result: ~p~n", [Result])
    catch
        Error:Reason ->
            io:format("Simple load failed: ~p:~p~n", [Error, Reason])
    end.

% Test what happens when we try to load just the problematic parts
strformat_in_context_test() ->
    LuaState = luerl:init(),
    
    % Test the specific strformat usage from dump.lua
    Code = "
        local strformat = string.format
        local function test_func(val)
            if val == nil then return 'nil value' end
            return strformat('%q', val)
        end
        return test_func('hello')
    ",
    
    Result = luerl:do(Code, LuaState),
    io:format("Strformat in context result: ~p~n", [Result]).