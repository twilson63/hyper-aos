-module(debug_dump_test).
-include_lib("eunit/include/eunit.hrl").

%% Helper function to load dump module exactly like aos_dump_test
load_dump_module(LuaState) ->
    {ok, DumpBin} = file:read_file("../src/dump.lua"),
    DumpSrc = binary_to_list(DumpBin),
    
    % Load the dump module - wrap it in a function to capture the return value
    LoadCode = "do local dump_module = function() " ++ DumpSrc ++ " end; _G.dump = dump_module() end",
    {ok, _, LuaState2} = luerl:do(LoadCode, LuaState),
    LuaState2.

%% Test setup exactly like aos_dump_test
setup() ->
    LuaState = luerl:init(),
    load_dump_module(LuaState).

%% Debug version of nested_table_dump_test that shows more info
debug_nested_table_test() ->
    LuaState = setup(),
    
    % First, let's verify dump is loaded correctly
    CheckCode = "return type(dump), type(dump.dump)",
    {ok, CheckResult, _} = luerl:do(CheckCode, LuaState),
    io:format("Dump module check: ~p~n", [CheckResult]),
    
    % Test with a very simple table first
    SimpleCode = "
        local t = {a = 1}
        return dump.dump(t)
    ",
    
    try
        SimpleResult = luerl:do(SimpleCode, LuaState),
        io:format("Simple dump result: ~p~n", [SimpleResult])
    catch
        Error:Reason ->
            io:format("Simple dump failed: ~p:~p~n", [Error, Reason])
    end,
    
    % Now try the nested table that fails
    NestedCode = "
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
        return dump.dump(t)
    ",
    
    try
        NestedResult = luerl:do(NestedCode, LuaState),
        io:format("Nested dump result: ~p~n", [NestedResult])
    catch
        Error2:Reason2 ->
            io:format("Nested dump failed: ~p:~p~n", [Error2, Reason2])
    end.

%% Test individual dump.lua functions to see which one fails
debug_dump_functions_test() ->
    LuaState = setup(),
    
    % Test tostring
    Code1 = "local t = {a = 1}; return tostring(t)",
    Result1 = luerl:do(Code1, LuaState),
    io:format("tostring result: ~p~n", [Result1]),
    
    % Test string.format with %q
    Code2 = "return string.format('%q', 'hello')",
    Result2 = luerl:do(Code2, LuaState),
    io:format("string.format %%q result: ~p~n", [Result2]),
    
    % Test pairs()
    Code3 = "local t = {a = 1, b = 2}; local result = {}; for k, v in pairs(t) do table.insert(result, k .. '=' .. tostring(v)) end; return result",
    Result3 = luerl:do(Code3, LuaState),
    io:format("pairs() result: ~p~n", [Result3]),
    
    % Test table.sort
    Code4 = "local t = {{key='b', val=2}, {key='a', val=1}}; table.sort(t, function(a, b) return a.key < b.key end); return t",
    Result4 = luerl:do(Code4, LuaState),
    io:format("table.sort result: ~p~n", [Result4]).

%% Test the exact dump function structure
debug_dump_structure_test() ->
    LuaState = setup(),
    
    % Check what's in the dump module
    Code = "
        local result = {}
        result.dump_type = type(dump)
        result.dump_keys = {}
        if type(dump) == 'table' then
            for k, v in pairs(dump) do
                table.insert(result.dump_keys, k .. ':' .. type(v))
            end
        end
        result.version = dump._version
        result.dump_func = type(dump.dump)
        return result
    ",
    
    {ok, [Result], _} = luerl:do(Code, LuaState),
    io:format("Dump structure: ~p~n", [Result]).