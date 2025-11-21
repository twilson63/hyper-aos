-module(debug_minimal_dump).
-include_lib("eunit/include/eunit.hrl").

%% Minimal test to debug the dump issue
minimal_dump_debug_test() ->
    LuaState = luerl:init(),
    
    % First try to load dump and test if it loads correctly
    LoadCode = "dump = dofile('../src/dump.lua')",
    {ok, _, LuaState2} = luerl:do(LoadCode, LuaState),
    
    % Test if dump function exists
    TestCode = "return type(dump)",
    {ok, [DumpType], _} = luerl:do(TestCode, LuaState2),
    ?assertEqual(<<"table">>, DumpType),
    
    % Test if dump.dump function exists  
    TestCode2 = "return type(dump.dump)",
    {ok, [DumpDumpType], _} = luerl:do(TestCode2, LuaState2),
    ?assertEqual(<<"function">>, DumpDumpType),
    
    % Test with simple value first
    SimpleCode = "return dump.dump('hello')",
    {ok, [SimpleResult], _} = luerl:do(SimpleCode, LuaState2),
    ?assertMatch(<<_/binary>>, SimpleResult),
    
    % Test with simple table
    SimpleTableCode = "return dump.dump({a = 1})",
    {ok, [SimpleTableResult], _} = luerl:do(SimpleTableCode, LuaState2),
    ?assertMatch(<<_/binary>>, SimpleTableResult).