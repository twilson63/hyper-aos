-module(minimal_eval_test).
-export([test/0]).

test() ->
    % Load aos.lua
    LuaState0 = luerl:init(),
    {ok, AosContent} = file:read_file("../aos.lua"),
    {_, LuaState1} = luerl:do(binary_to_list(AosContent), LuaState0),
    
    % Create minimal state and assignment
    StateTable = [
        {"results", [
            {"outbox", []},
            {"output", [{"data", ""}, {"prompt", ""}]}
        ]}
    ],
    
    AssignmentTable = [
        {"body", [
            {"action", "eval"},
            {"data", "return 'hello'"}
        ]}
    ],
    
    % Call compute
    Result = luerl:call_function([compute], [StateTable, AssignmentTable], LuaState1),
    io:format("Result: ~p~n", [Result]),
    
    % Try calling eval directly with a simple message
    EvalMsg = [{"data", "return 'direct eval'"}],
    EvalResult = luerl:call_function([eval], [EvalMsg], LuaState1),
    io:format("Direct eval result: ~p~n", [EvalResult]),
    
    % Check if eval function exists as global
    case luerl:get(['_G', eval], LuaState1) of
        {ok, EvalFunc} ->
            io:format("_G.eval exists: ~p~n", [EvalFunc]);
        Error ->
            io:format("_G.eval not found: ~p~n", [Error])
    end.