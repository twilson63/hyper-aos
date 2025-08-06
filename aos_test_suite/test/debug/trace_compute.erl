-module(trace_compute).
-export([test/0]).

test() ->
    LuaState0 = luerl:init(),
    {ok, AosContent} = file:read_file("../aos.lua"),
    {_, LuaState1} = luerl:do(binary_to_list(AosContent), LuaState0),
    
    % Add debugging to compute function
    DebugCode = "
        local original_compute = compute
        compute = function(state, assignment)
            print('DEBUG: compute called')
            local msg = assignment.body or {}
            local action = msg.action or ''
            print('DEBUG: action = ' .. tostring(action))
            action = string.lower(action)
            print('DEBUG: lowercase action = ' .. tostring(action))
            print('DEBUG: type(_G[action]) = ' .. tostring(type(_G[action])))
            
            -- Call original
            return original_compute(state, assignment)
        end
    ",
    
    {_, LuaState2} = luerl:do(DebugCode, LuaState1),
    
    % Create test data
    StateTable = [
        {"results", [
            {"outbox", []},
            {"output", [{"data", ""}, {"prompt", ""}]}
        ]}
    ],
    
    AssignmentTable = [
        {"body", [
            {"action", "eval"},
            {"data", "return 42"}
        ]}
    ],
    
    % Capture output
    {_, LuaState3} = luerl:do("_OUTPUT = ''", LuaState2),
    
    % Call compute
    {[Status, _Result], LuaState4} = luerl:call_function([compute], [StateTable, AssignmentTable], LuaState3),
    io:format("Status: ~p~n", [Status]),
    
    % Get debug output
    {[Output], _} = luerl:do("return _OUTPUT", LuaState4),
    io:format("Debug output:~n~s~n", [Output]),
    
    ok.