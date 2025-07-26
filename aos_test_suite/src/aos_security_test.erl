-module(aos_security_test).
-include_lib("eunit/include/eunit.hrl").

%% Test eval with valid commitments
eval_with_valid_commitments_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    LuaState2 = aos_test_helpers:initialize_process(LuaState, State),
    
    %% Create eval with valid commitments
    Assignment = aos_test_helpers:create_eval_assignment(<<"return 'authorized'">>),
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    
    ?assertEqual(<<"authorized">>, aos_test_helpers:extract_output_data(Result)).

%% Test eval with invalid commitments
eval_with_invalid_commitments_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    LuaState2 = aos_test_helpers:initialize_process(LuaState, State),
    
    %% Create eval with unauthorized user
    UnauthorizedMsg = aos_test_helpers:create_unauthorized_eval_message(<<"return 'should fail'">>),
    Assignment = aos_test_helpers:create_assignment(UnauthorizedMsg),
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertMatch(<<"Unauthorized:", _/binary>>, Output).

%% Test eval without commitments
eval_without_commitments_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    LuaState2 = aos_test_helpers:initialize_process(LuaState, State),
    
    %% Create eval without commitments
    NoCommitMsg = aos_test_helpers:create_message_without_commitments(<<"return 'should fail'">>),
    Assignment = aos_test_helpers:create_assignment(NoCommitMsg),
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertMatch(<<"Unauthorized:", _/binary>>, Output).

%% Test process initialization sets owner
process_initialization_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Create process message with custom owner
    CustomOwner = <<"CustomOwner123456789012345678901234567890A">>,
    ProcessMsg = aos_test_helpers:create_process_message(CustomOwner),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    
    %% Initialize process
    {[_, _], LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    
    %% Try eval with matching owner
    EvalMsg = aos_test_helpers:create_eval_message(<<"return 'owner matches'">>, CustomOwner),
    EvalAssignment = aos_test_helpers:create_assignment(EvalMsg),
    Result = aos_test_helpers:call_compute(LuaState2, State, EvalAssignment),
    
    ?assertEqual(<<"owner matches">>, aos_test_helpers:extract_output_data(Result)).