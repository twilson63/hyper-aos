-module(simple_security_test).
-include_lib("eunit/include/eunit.hrl").

%% Test security with no owner initialized - should fail without commitments
no_owner_initialized_test() ->
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

%% Test security with matching RSA-PSS-512 commitment
matching_commitment_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process with owner
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    ProcessMsg = aos_test_helpers:create_process_message(Owner),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    
    %% Create message with matching commitment
    EvalMsg = aos_test_helpers:create_eval_message(<<"return 'secure'">>, Owner),
    Assignment = aos_test_helpers:create_assignment(EvalMsg),
    
    %% Call compute and verify
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertEqual(<<"secure">>, Output).

%% Test security with missing commitment
missing_commitment_test() ->
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

%% Test security with different committer
different_committer_test() ->
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

%% Test with different commitment type (but should work since committer matches)
wrong_commitment_type_test() ->
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

%% Test with HMAC commitment (should work since committer matches)
hmac_commitment_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process with owner
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    LuaState2 = initialize_with_owner(LuaState, State, Owner),
    
    %% Message with hmac-sha256 commitment
    HmacMsg = #{
        <<"id">> => <<"test-msg">>,
        <<"action">> => <<"eval">>,
        <<"data">> => <<"return 'should work'">>,
        <<"commitments">> => #{
            <<"key1">> => #{
                <<"type">> => <<"hmac-sha256">>,
                <<"committer">> => Owner
            }
        }
    },
    Assignment = aos_test_helpers:create_assignment(HmacMsg),
    
    %% Call compute and verify - should work since committer matches
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertEqual(<<"should work">>, Output).

%% Helper function to initialize with owner
initialize_with_owner(LuaState, State, Owner) ->
    ProcessMsg = aos_test_helpers:create_process_message(Owner),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    LuaState2.