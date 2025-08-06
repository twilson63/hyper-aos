-module(debug_eval).
-export([test/0]).

test() ->
    LuaState0 = luerl:init(),
    {ok, AosContent} = file:read_file("../aos.lua"),
    {_, LuaState1} = luerl:do(binary_to_list(AosContent), LuaState0),
    
    % Check if eval function exists
    case luerl:get([eval], LuaState1) of
        {ok, Value} ->
            io:format("eval exists: ~p~n", [Value]);
        Error ->
            io:format("eval not found: ~p~n", [Error])
    end,
    
    % Check _G
    case luerl:get(['_G', eval], LuaState1) of
        {ok, Value2} ->
            io:format("_G.eval exists: ~p~n", [Value2]);
        Error2 ->
            io:format("_G.eval not found: ~p~n", [Error2])
    end,
    
    % Try calling eval directly
    case luerl:call_function([eval], [{[{"data", "return 42"}]}], LuaState1) of
        {ok, Result, _} ->
            io:format("Direct eval result: ~p~n", [Result]);
        Error3 ->
            io:format("Direct eval error: ~p~n", [Error3])
    end.