-module(final_diagnosis_test).
-include_lib("eunit/include/eunit.hrl").

%% Final diagnosis of the LUERL issue
diagnose_exact_error_test() ->
    % Load dump exactly like aos_dump_test
    LuaState = luerl:init(),
    {ok, DumpBin} = file:read_file("../src/dump.lua"),
    DumpSrc = binary_to_list(DumpBin),
    LoadCode = "do local dump_module = function() " ++ DumpSrc ++ " end; _G.dump = dump_module() end",
    {ok, _, LuaState2} = luerl:do(LoadCode, LuaState),
    
    % Use the exact same test case
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
        local result = dump.dump(t)
        return result
    ",
    
    % DON'T pattern match, just see what we get
    RawResult = luerl:do(Code, LuaState2),
    io:format("Raw result from luerl:do: ~p~n", [RawResult]),
    
    case RawResult of
        {ok, [Result], _} ->
            io:format("SUCCESS: Result = ~p~n", [Result]),
            ResultStr = binary_to_list(iolist_to_binary(Result)),
            io:format("Result as string: ~s~n", [ResultStr]);
        {lua_error, Error, _} ->
            io:format("LUA ERROR: ~p~n", [Error]);
        Other ->
            io:format("OTHER RESULT TYPE: ~p~n", [Other])
    end.

check_dump_function_exists_test() ->
    LuaState = luerl:init(),
    {ok, DumpBin} = file:read_file("../src/dump.lua"),
    DumpSrc = binary_to_list(DumpBin),
    LoadCode = "do local dump_module = function() " ++ DumpSrc ++ " end; _G.dump = dump_module() end",
    {ok, _, LuaState2} = luerl:do(LoadCode, LuaState),
    
    % Check if dump.dump actually exists and is callable
    CheckCode = "
        return {
            dump_exists = dump ~= nil,
            dump_type = type(dump),
            dump_dump_exists = dump.dump ~= nil,
            dump_dump_type = type(dump.dump),
            version = dump._version
        }
    ",
    
    Result = luerl:do(CheckCode, LuaState2),
    io:format("Dump function check result: ~p~n", [Result]).

check_simple_call_test() ->
    LuaState = luerl:init(),
    {ok, DumpBin} = file:read_file("../src/dump.lua"),
    DumpSrc = binary_to_list(DumpBin),
    LoadCode = "do local dump_module = function() " ++ DumpSrc ++ " end; _G.dump = dump_module() end",
    {ok, _, LuaState2} = luerl:do(LoadCode, LuaState),
    
    % Try the simplest possible call
    SimpleCode = "return dump.dump({a = 1})",
    Result = luerl:do(SimpleCode, LuaState2),
    io:format("Simple call result: ~p~n", [Result]).