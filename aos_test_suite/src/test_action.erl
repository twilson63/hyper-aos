-module(test_action).
-export([test/0]).

test() ->
    LuaState0 = luerl:init(),
    {ok, AosContent} = file:read_file("../aos.lua"),
    {_, LuaState1} = luerl:do(binary_to_list(AosContent), LuaState0),
    
    % Create minimal state
    StateTable = [
        {"results", [
            {"outbox", []},
            {"output", [{"data", ""}, {"prompt", ""}]}
        ]}
    ],
    
    % Test 1: Direct eval action
    io:format("~nTest 1: Direct eval action~n"),
    AssignmentEval = [
        {"body", [
            {"action", "eval"},
            {"data", "return 42"}
        ]}
    ],
    
    {[Status1, Result1], _} = luerl:call_function([compute], [StateTable, AssignmentEval], LuaState1),
    io:format("Status: ~p~n", [Status1]),
    
    % Extract and print the output data
    case Result1 of
        [{_, ResultsTable}] when is_list(ResultsTable) ->
            case lists:keyfind("output", 1, ResultsTable) of
                {"output", OutputTable} ->
                    case lists:keyfind("data", 1, OutputTable) of
                        {"data", Data} ->
                            io:format("Output data: ~p~n", [Data]);
                        _ ->
                            io:format("No data in output~n")
                    end;
                _ ->
                    io:format("No output in results~n")
            end;
        _ ->
            io:format("Unexpected result format: ~p~n", [Result1])
    end,
    
    % Test 2: Check if eval is a global function
    io:format("~nTest 2: Checking if eval is global~n"),
    {[IsFunction], _} = luerl:call_function([eval], [<<"return type(_G.eval)">>], LuaState1),
    io:format("type(_G.eval) = ~p~n", [IsFunction]),
    
    ok.