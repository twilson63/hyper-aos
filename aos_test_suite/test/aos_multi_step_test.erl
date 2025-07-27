-module(aos_multi_step_test).
-include_lib("eunit/include/eunit.hrl").

%% Test multi-step evaluation scenarios with saved state
multi_step_eval_test() ->
    %% Step 1: Initialize AOS and create process
    LuaState1 = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process with owner
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    ProcessMsg = aos_test_helpers:create_process_message(Owner),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState1, State, ProcessAssignment),
    
    %% Step 2: First eval - define a function
    EvalMsg1 = aos_test_helpers:create_eval_message(
        <<"function add(a, b) return a + b end; return 'function defined'">>, 
        Owner
    ),
    Assignment1 = aos_test_helpers:create_assignment(EvalMsg1),
    {Result1, LuaState3} = aos_test_helpers:call_compute(LuaState2, State, Assignment1),
    Output1 = aos_test_helpers:extract_output_data({Result1, LuaState3}),
    ?assertEqual(<<"function defined">>, Output1),
    
    %% Step 3: Second eval - use the function
    EvalMsg2 = aos_test_helpers:create_eval_message(
        <<"return add(5, 3)">>, 
        Owner
    ),
    Assignment2 = aos_test_helpers:create_assignment(EvalMsg2),
    {Result2, LuaState4} = aos_test_helpers:call_compute(LuaState3, State, Assignment2),
    Output2 = aos_test_helpers:extract_output_data({Result2, LuaState4}),
    ?assertEqual(<<"8">>, Output2),
    
    %% Step 4: Third eval - modify global state
    EvalMsg3 = aos_test_helpers:create_eval_message(
        <<"GlobalCounter = (GlobalCounter or 0) + 1; return GlobalCounter">>, 
        Owner
    ),
    Assignment3 = aos_test_helpers:create_assignment(EvalMsg3),
    {Result3, LuaState5} = aos_test_helpers:call_compute(LuaState4, State, Assignment3),
    Output3 = aos_test_helpers:extract_output_data({Result3, LuaState5}),
    ?assertEqual(<<"1">>, Output3),
    
    %% Step 5: Fourth eval - verify state persists
    {Result4, LuaState6} = aos_test_helpers:call_compute(LuaState5, State, Assignment3),
    Output4 = aos_test_helpers:extract_output_data({Result4, LuaState6}),
    ?assertEqual(<<"2">>, Output4).

%% Test authorities persistence across multiple messages
authorities_multi_step_test() ->
    %% Step 1: Initialize with authorities
    LuaState1 = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    Authority1 = <<"Auth123456789012345678901234567890123456789">>,
    Authority2 = <<"Auth223456789012345678901234567890123456789">>,
    
    ProcessMsg = #{
        <<"id">> => <<"test-process">>,
        <<"type">> => <<"process">>,
        <<"authority">> => <<Authority1/binary, ",", Authority2/binary>>,
        <<"commitments">> => #{
            <<"key1">> => #{
                <<"type">> => <<"RSA-PSS-512">>,
                <<"committer">> => Owner
            }
        }
    },
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState1, State, ProcessAssignment),
    
    %% Verify authorities were set in _G
    {MetaAuthorities, _} = luerl:get_table([<<"_G">>, <<"authorities">>], LuaState2),
    %% Authorities are stored as a Lua array with {index, value} tuples
    ?assertEqual([{1, Authority1}, {2, Authority2}], MetaAuthorities),
    
    %% Step 2: Test that state persists across multiple evals
    EvalMsg1 = aos_test_helpers:create_eval_message(
        <<"GlobalTest = 'test1'; return GlobalTest">>, 
        Owner
    ),
    Assignment1 = aos_test_helpers:create_assignment(EvalMsg1),
    {Result1, LuaState3} = aos_test_helpers:call_compute(LuaState2, State, Assignment1),
    Output1 = aos_test_helpers:extract_output_data({Result1, LuaState3}),
    ?assertEqual(<<"test1">>, Output1),
    
    %% Step 3: Verify state persists
    EvalMsg2 = aos_test_helpers:create_eval_message(
        <<"return GlobalTest">>, 
        Owner
    ),
    Assignment2 = aos_test_helpers:create_assignment(EvalMsg2),
    {Result2, _LuaState4} = aos_test_helpers:call_compute(LuaState3, State, Assignment2),
    Output2 = aos_test_helpers:extract_output_data({Result2, _LuaState4}),
    ?assertEqual(<<"test1">>, Output2).

%% Test state isolation between process instances
state_isolation_test() ->
    %% Create two separate Lua states
    LuaState1 = aos_test_helpers:initialize_aos(),
    LuaState2 = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize first process with owner1
    Owner1 = <<"Owner1_123456789012345678901234567890123456">>,
    ProcessMsg1 = aos_test_helpers:create_process_message(Owner1),
    ProcessAssignment1 = aos_test_helpers:create_assignment(ProcessMsg1),
    {_, LuaStateA} = aos_test_helpers:call_compute(LuaState1, State, ProcessAssignment1),
    
    %% Initialize second process with owner2
    Owner2 = <<"Owner2_123456789012345678901234567890123456">>,
    ProcessMsg2 = aos_test_helpers:create_process_message(Owner2),
    ProcessAssignment2 = aos_test_helpers:create_assignment(ProcessMsg2),
    {_, LuaStateB} = aos_test_helpers:call_compute(LuaState2, State, ProcessAssignment2),
    
    %% Set value in first state
    EvalMsg1 = aos_test_helpers:create_eval_message(<<"MyValue = 'state1'; return MyValue">>, Owner1),
    Assignment1 = aos_test_helpers:create_assignment(EvalMsg1),
    {Result1, LuaStateA2} = aos_test_helpers:call_compute(LuaStateA, State, Assignment1),
    Output1 = aos_test_helpers:extract_output_data({Result1, LuaStateA2}),
    ?assertEqual(<<"state1">>, Output1),
    
    %% Set different value in second state
    EvalMsg2 = aos_test_helpers:create_eval_message(<<"MyValue = 'state2'; return MyValue">>, Owner2),
    Assignment2 = aos_test_helpers:create_assignment(EvalMsg2),
    {Result2, LuaStateB2} = aos_test_helpers:call_compute(LuaStateB, State, Assignment2),
    Output2 = aos_test_helpers:extract_output_data({Result2, LuaStateB2}),
    ?assertEqual(<<"state2">>, Output2),
    
    %% Verify values remain isolated
    EvalCheck1 = aos_test_helpers:create_eval_message(<<"return MyValue">>, Owner1),
    AssignmentCheck1 = aos_test_helpers:create_assignment(EvalCheck1),
    ResultCheck1Full = aos_test_helpers:call_compute(LuaStateA2, State, AssignmentCheck1),
    OutputCheck1 = aos_test_helpers:extract_output_data(ResultCheck1Full),
    ?assertEqual(<<"state1">>, OutputCheck1),
    
    EvalCheck2 = aos_test_helpers:create_eval_message(<<"return MyValue">>, Owner2),
    AssignmentCheck2 = aos_test_helpers:create_assignment(EvalCheck2),
    ResultCheck2Full = aos_test_helpers:call_compute(LuaStateB2, State, AssignmentCheck2),
    OutputCheck2 = aos_test_helpers:extract_output_data(ResultCheck2Full),
    ?assertEqual(<<"state2">>, OutputCheck2).