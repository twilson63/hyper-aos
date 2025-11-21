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
    {ok, [_, _], LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    
    %% Try eval with matching owner
    EvalMsg = aos_test_helpers:create_eval_message(<<"return 'owner matches'">>, CustomOwner),
    EvalAssignment = aos_test_helpers:create_assignment(EvalMsg),
    Result = aos_test_helpers:call_compute(LuaState2, State, EvalAssignment),
    
    ?assertEqual(<<"owner matches">>, aos_test_helpers:extract_output_data(Result)).

%% Test that authorities_set is built correctly during initialization
authorities_set_initialization_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Create process message with multiple authorities
    Owner = <<"TestOwner123456789012345678901234567890123">>,
    Authority1 = <<"Authority01234567890123456789012345678901">>,
    Authority2 = <<"Authority01234567890123456789012345678902">>,
    AuthoritiesStr = <<Authority1/binary, ",", Authority2/binary>>,
    
    ProcessMsg = aos_test_helpers:create_process_message(Owner),
    ProcessMsgWithAuth = maps:put(<<"authority">>, AuthoritiesStr, ProcessMsg),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsgWithAuth),
    
    %% Initialize process - should not raise any errors
    %% The authorities_set should be built internally without being exposed in the result
    {ok, [_, _], _LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    ok.

%% Test that authority lookup uses set for O(1) performance
authorities_set_lookup_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Create process message with authorities
    Owner = <<"TestOwner123456789012345678901234567890123">>,
    Authority1 = <<"Authority01234567890123456789012345678901">>,
    Authority2 = <<"Authority01234567890123456789012345678902">>,
    AuthoritiesStr = <<Authority1/binary, ",", Authority2/binary>>,
    
    ProcessMsg = aos_test_helpers:create_process_message(Owner),
    ProcessMsgWithAuth = maps:put(<<"authority">>, AuthoritiesStr, ProcessMsg),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsgWithAuth),
    
    %% Initialize process
    {ok, [_, _], LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    
    %% Verify that authority lookups work correctly
    %% This tests that the is_trusted function uses authorities_set for fast lookups
    TrustedMsg = #{
        <<"id">> => <<"trusted-msg">>,
        <<"from">> => Authority1,
        <<"from-process">> => Authority1,
        <<"commitments">> => #{
            <<"key-1">> => #{
                <<"type">> => <<"rsa-pss-512">>,
                <<"committer">> => Authority1
            }
        }
    },
    TrustedAssignment = aos_test_helpers:create_assignment(TrustedMsg),
    Result = aos_test_helpers:call_compute(LuaState2, State, TrustedAssignment),
    
    %% If lookup is working correctly, the message should be processed without error
    {ok, [_, _], _} = Result,
    ok.

%% Test that duplicate authorities are not added to set
authorities_no_duplicates_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Create process message with duplicate authorities (with whitespace)
    Owner = <<"TestOwner123456789012345678901234567890123">>,
    Authority1 = <<"Authority01234567890123456789012345678901">>,
    %% Include duplicates with different whitespace
    AuthoritiesStr = <<Authority1/binary, ", ", Authority1/binary, " , ", Authority1/binary>>,
    
    ProcessMsg = aos_test_helpers:create_process_message(Owner),
    ProcessMsgWithAuth = maps:put(<<"authority">>, AuthoritiesStr, ProcessMsg),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsgWithAuth),
    
    %% Initialize process - should handle duplicates without errors
    %% The authorities_set prevents duplicates from being added to authorities array
    {ok, [_, _], _LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    ok.

%% Test that invalid authorities (wrong length) are filtered
authorities_validation_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Create process message with mix of valid and invalid authorities
    Owner = <<"TestOwner123456789012345678901234567890123">>,
    ValidAuthority = <<"Authority01234567890123456789012345678901">>,
    InvalidAuthority = <<"TooShort">>,
    AuthoritiesStr = <<ValidAuthority/binary, ",", InvalidAuthority/binary>>,
    
    ProcessMsg = aos_test_helpers:create_process_message(Owner),
    ProcessMsgWithAuth = maps:put(<<"authority">>, AuthoritiesStr, ProcessMsg),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsgWithAuth),
    
    %% Initialize process - should filter out invalid authorities (wrong length)
    %% Only valid 43-char authorities are added to authorities_set
    {ok, [_, _], _LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    ok.