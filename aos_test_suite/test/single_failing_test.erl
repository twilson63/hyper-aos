-module(single_failing_test).
-include_lib("eunit/include/eunit.hrl").

%% Helper function to load dump module exactly like aos_dump_test
load_dump_module(LuaState) ->
    % Load dump module directly using Lua's require-like functionality
    LoadCode = "dump = dofile('../src/dump.lua')",
    {ok, _, LuaState2} = luerl:do(LoadCode, LuaState),
    LuaState2.

%% Test setup exactly like aos_dump_test
setup() ->
    LuaState = luerl:init(),
    load_dump_module(LuaState).

%% Exact copy of the failing nested_table_dump_test 
nested_table_dump_exact_copy_test() ->
    LuaState = setup(),
    
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
    
    % This is the exact line that fails in the original test (line 77)
    {ok, [Result], _} = luerl:do(Code, LuaState),
    ResultStr = binary_to_list(iolist_to_binary(Result)),
    
    % Check nested structure is preserved
    ?assert(string:str(ResultStr, "user = {") > 0),
    ?assert(string:str(ResultStr, "name = \"Alice\"") > 0),
    ?assert(string:str(ResultStr, "age = 30") > 0),
    ?assert(string:str(ResultStr, "address = {") > 0),
    ?assert(string:str(ResultStr, "street = \"123 Main St\"") > 0),
    ?assert(string:str(ResultStr, "city = \"NYC\"") > 0).