-module(debug_progressive_dump).
-include_lib("eunit/include/eunit.hrl").

%% Helper function to load dump module
load_dump_module(LuaState) ->
    LoadCode = "dump = dofile('../src/dump.lua')",
    {ok, _, LuaState2} = luerl:do(LoadCode, LuaState),
    LuaState2.

%% Test setup
setup() ->
    LuaState = luerl:init(),
    load_dump_module(LuaState).

%% Progressive tests to find where it breaks
simple_table_test() ->
    LuaState = setup(),
    Code = "return dump.dump({a = 1})",
    {ok, [Result], _} = luerl:do(Code, LuaState),
    ?assertMatch(<<_/binary>>, Result).

nested_one_level_test() ->
    LuaState = setup(),
    Code = "return dump.dump({user = {name = 'Alice'}})",
    {ok, [Result], _} = luerl:do(Code, LuaState),
    ?assertMatch(<<_/binary>>, Result).

nested_two_levels_test() ->
    LuaState = setup(),
    Code = "return dump.dump({user = {name = 'Alice', address = {city = 'NYC'}}})",
    {ok, [Result], _} = luerl:do(Code, LuaState),
    ?assertMatch(<<_/binary>>, Result).

exact_failing_case_test() ->
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
        return dump.dump(t)
    ",
    {ok, [Result], _} = luerl:do(Code, LuaState),
    ?assertMatch(<<_/binary>>, Result).