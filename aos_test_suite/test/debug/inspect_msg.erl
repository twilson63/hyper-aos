-module(inspect_msg).
-export([test/0]).

test() ->
    LuaState0 = luerl:init(),
    {ok, AosContent} = file:read_file("../aos.lua"),
    {_, LuaState1} = luerl:do(binary_to_list(AosContent), LuaState0),
    
    % Override compute to inspect the message
    InspectCode = "
        local original_compute = compute
        compute = function(state, assignment)
            local msg = assignment.body or {}
            
            -- Store inspection data in global for retrieval
            _G.inspect_data = {
                has_body = assignment.body ~= nil,
                msg_type = type(msg),
                action_raw = msg.action,
                action_type = type(msg.action),
                data_raw = msg.data,
                data_type = type(msg.data)
            }
            
            -- Also check if msg is empty
            local count = 0
            for k,v in pairs(msg) do
                count = count + 1
            end
            _G.inspect_data.msg_field_count = count
            
            return original_compute(state, assignment)
        end
    ",
    
    {_, LuaState2} = luerl:do(InspectCode, LuaState1),
    
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
    
    % Call compute
    {[Status, _], LuaState3} = luerl:call_function([compute], [StateTable, AssignmentTable], LuaState2),
    io:format("Status: ~p~n", [Status]),
    
    % Get inspection data
    {[HasBody], _} = luerl:do("return _G.inspect_data.has_body", LuaState3),
    {[MsgType], _} = luerl:do("return _G.inspect_data.msg_type", LuaState3),
    {[ActionRaw], _} = luerl:do("return _G.inspect_data.action_raw", LuaState3),
    {[ActionType], _} = luerl:do("return _G.inspect_data.action_type", LuaState3),
    {[FieldCount], _} = luerl:do("return _G.inspect_data.msg_field_count", LuaState3),
    
    io:format("~nInspection results:~n"),
    io:format("  has_body: ~p~n", [HasBody]),
    io:format("  msg_type: ~p~n", [MsgType]),
    io:format("  action_raw: ~p~n", [ActionRaw]),
    io:format("  action_type: ~p~n", [ActionType]),
    io:format("  msg_field_count: ~p~n", [FieldCount]),
    
    ok.