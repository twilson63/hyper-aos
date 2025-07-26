-module(aos_owner_test).
-include_lib("eunit/include/eunit.hrl").

%% Test eval with matching committer
matching_committer_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process with owner
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    ProcessMsg = aos_test_helpers:create_process_message(Owner),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    
    %% Message with matching committer
    EvalMsg = aos_test_helpers:create_eval_message(<<"return 'authorized'">>, Owner),
    Assignment = aos_test_helpers:create_assignment(EvalMsg),
    
    %% Call compute and verify
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertEqual(<<"authorized">>, Output).

%% Test eval with different committer
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

%% Test eval with no owner set - should fail without commitments
no_owner_set_test() ->
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

%% Test eval with only HMAC commitment (should fail - no valid from)
hmac_committer_ignored_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process with owner
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    LuaState2 = initialize_with_owner(LuaState, State, Owner),
    
    %% Message with only hmac-sha256 commitment (no committer)
    HmacMsg = #{
        <<"id">> => <<"test-msg">>,
        <<"action">> => <<"eval">>,
        <<"data">> => <<"return 'should fail'">>,
        <<"commitments">> => #{
            <<"key1">> => #{
                <<"type">> => <<"hmac-sha256">>
                % HMAC commitments don't have committers
            }
        }
    },
    Assignment = aos_test_helpers:create_assignment(HmacMsg),
    
    %% Call compute and verify - should fail since no valid from can be derived
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertMatch(<<"Unauthorized:", _/binary>>, Output).

%% Test eval with multiple commitments
multiple_commitments_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process with owner
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    LuaState2 = initialize_with_owner(LuaState, State, Owner),
    
    %% Message with multiple commitments
    %% HMAC commitments don't have committers
    MultiMsg = #{
        <<"id">> => <<"test-msg">>,
        <<"action">> => <<"eval">>,
        <<"data">> => <<"return 'should work'">>,
        <<"commitments">> => #{
            <<"key1">> => #{
                <<"type">> => <<"hmac-sha256">>
                % No committer for HMAC
            },
            <<"key2">> => #{
                <<"type">> => <<"RSA-PSS-512">>,
                <<"committer">> => Owner
            },
            <<"key3">> => #{
                <<"type">> => <<"RSA-PSS-512">>,
                <<"committer">> => <<"DifferentUser123456789012345678901234567890">>
            }
        }
    },
    Assignment = aos_test_helpers:create_assignment(MultiMsg),
    
    %% Call compute and verify
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertEqual(<<"should work">>, Output).

%% Helper function to initialize with owner
initialize_with_owner(LuaState, State, Owner) ->
    ProcessMsg = aos_test_helpers:create_process_message(Owner),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    LuaState2.