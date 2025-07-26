-module(aos_eval_security_test).
-include_lib("eunit/include/eunit.hrl").

%% Test eval with matching commitment
eval_with_matching_commitment_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process with owner
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    ProcessMsg = aos_test_helpers:create_process_message(Owner),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    
    %% Create message with matching commitment
    EvalMsg = aos_test_helpers:create_eval_message(<<"return 'secure eval'">>, Owner),
    Assignment = aos_test_helpers:create_assignment(EvalMsg),
    
    %% Call compute and verify
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertEqual(<<"secure eval">>, Output).

%% Test eval with non-matching commitment
eval_with_non_matching_commitment_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process with owner
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    LuaState2 = initialize_with_owner(LuaState, State, Owner),
    
    %% Message with different committer
    DifferentCommitter = <<"DifferentUser123456789012345678901234567890">>,
    EvalMsg = aos_test_helpers:create_eval_message(<<"return 'should fail'">>, DifferentCommitter),
    Assignment = aos_test_helpers:create_assignment(EvalMsg),
    
    %% Call compute and verify
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertMatch(<<"Unauthorized:", _/binary>>, Output).

%% Test eval with missing commitment
eval_with_missing_commitment_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process with owner
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    LuaState2 = initialize_with_owner(LuaState, State, Owner),
    
    %% Message without commitments
    NoCommitMsg = aos_test_helpers:create_message_without_commitments(<<"return 'should fail'">>),
    Assignment = aos_test_helpers:create_assignment(NoCommitMsg),
    
    %% Call compute and verify
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertMatch(<<"Unauthorized:", _/binary>>, Output).

%% Test eval with wrong type (but should work since committer matches)
eval_with_wrong_type_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process with owner
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    LuaState2 = initialize_with_owner(LuaState, State, Owner),
    
    %% Message with different commitment type but same committer
    WrongTypeMsg = #{
        <<"id">> => <<"test-msg">>,
        <<"action">> => <<"eval">>,
        <<"data">> => <<"return 'should work'">>,
        <<"commitments">> => #{
            <<"key1">> => #{
                <<"type">> => <<"RSA-PSS-256">>, % Different type
                <<"committer">> => Owner
            }
        }
    },
    Assignment = aos_test_helpers:create_assignment(WrongTypeMsg),
    
    %% Call compute and verify - should work since committer matches
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertEqual(<<"should work">>, Output).

%% Test eval with no owner set - should fail without commitments
eval_with_no_owner_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Don't initialize process (no owner set)
    %% Create eval message without commitments
    EvalMsg = #{
        <<"id">> => <<"test-msg">>,
        <<"action">> => <<"eval">>,
        <<"data">> => <<"return 'should fail'">>
    },
    Assignment = aos_test_helpers:create_assignment(EvalMsg),
    
    %% Call compute and verify - should fail without commitments
    Result = aos_test_helpers:call_compute(LuaState, State, Assignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertMatch(<<"Unauthorized:", _/binary>>, Output).

%% Helper function to initialize with owner
initialize_with_owner(LuaState, State, Owner) ->
    ProcessMsg = aos_test_helpers:create_process_message(Owner),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    LuaState2.