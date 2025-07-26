-module(aos_test).
-include_lib("eunit/include/eunit.hrl").

%% Test basic message without action
basic_message_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process first
    LuaState2 = aos_test_helpers:initialize_process(LuaState, State),
    
    %% Create message without action
    Message = #{
        <<"id">> => <<"test-msg">>,
        <<"data">> => <<"Hello">>  
    },
    Assignment = aos_test_helpers:create_assignment(Message),
    
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    Output = aos_test_helpers:extract_output_data(Result),
    
    %% Verify message was added to inbox
    ?assertEqual(<<"New Message">>, Output).

%% Test eval action with math
eval_math_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    LuaState2 = aos_test_helpers:initialize_process(LuaState, State),
    
    %% Create eval assignment
    Assignment = aos_test_helpers:create_eval_assignment(<<"return 2 + 2">>),
    
    %% Call compute and verify result
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertEqual(<<"4">>, Output).

%% Test require .process._version
require_version_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    LuaState2 = aos_test_helpers:initialize_process(LuaState, State),
    
    %% Create eval assignment
    Assignment = aos_test_helpers:create_eval_assignment(<<"return require('.process')._version">>),
    
    %% Call compute and verify version
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertEqual(<<"dev">>, Output).

%% Test 1 + 1 evaluation
one_plus_one_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    LuaState2 = aos_test_helpers:initialize_process(LuaState, State),
    
    %% Create eval assignment
    Assignment = aos_test_helpers:create_eval_assignment(<<"return 1 + 1">>),
    
    %% Call compute and verify result
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertEqual(<<"2">>, Output).

%% Test send/outbox functionality
send_outbox_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    LuaState2 = aos_test_helpers:initialize_process(LuaState, State),
    
    %% Create eval assignment with send
    Assignment = aos_test_helpers:create_eval_assignment(<<"send({target='ID', data='hello'}); return 'sent'">>),
    
    %% Call compute - for now just verify it doesn't crash
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertEqual(<<"sent">>, Output).

%% Test print function
print_function_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    LuaState2 = aos_test_helpers:initialize_process(LuaState, State),
    
    %% Create eval assignment with print
    Assignment = aos_test_helpers:create_eval_assignment(<<"print('Hello from test'); return 'ok'">>),
    
    %% Call compute and verify printed output
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertMatch(<<"Hello from test", _/binary>>, Output).

%% Test multiple sends
multiple_sends_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    LuaState2 = aos_test_helpers:initialize_process(LuaState, State),
    
    %% Create eval assignment with multiple sends
    Assignment = aos_test_helpers:create_eval_assignment(<<"send({target='user1', data='msg1'}); send({target='user2', data='msg2'}); return 'sent both'">>),
    
    %% Call compute - for now just verify it doesn't crash
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertEqual(<<"sent both">>, Output).