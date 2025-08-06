-module(aos_from_process_test).
-include_lib("eunit/include/eunit.hrl").

%% Test from-process takes precedence over commitments
from_process_precedence_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process with owner
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    ProcessMsg = aos_test_helpers:create_process_message(Owner),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    
    %% Message with from-process matching owner and different committer
    MsgWithFromProcess = #{
        <<"id">> => <<"test-msg">>,
        <<"action">> => <<"eval">>,
        <<"data">> => <<"return 'from-process works'">>,
        <<"from-process">> => Owner,  % This should take precedence
        <<"commitments">> => #{
            <<"key1">> => #{
                <<"type">> => <<"RSA-PSS-512">>,
                <<"committer">> => <<"DifferentUser123456789012345678901234567890">>
            }
        }
    },
    Assignment = aos_test_helpers:create_assignment(MsgWithFromProcess),
    
    %% Should work because from-process matches owner
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertEqual(<<"from-process works">>, Output).

%% Test from-process with wrong value fails
from_process_wrong_value_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process with owner
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    ProcessMsg = aos_test_helpers:create_process_message(Owner),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    
    %% Message with from-process NOT matching owner
    MsgWithWrongFromProcess = #{
        <<"id">> => <<"test-msg">>,
        <<"action">> => <<"eval">>,
        <<"data">> => <<"return 'should fail'">>,
        <<"from-process">> => <<"WrongUser123456789012345678901234567890123">>,  % Wrong
        <<"commitments">> => #{
            <<"key1">> => #{
                <<"type">> => <<"RSA-PSS-512">>,
                <<"committer">> => Owner  % Even though committer matches, from-process takes precedence
            }
        }
    },
    Assignment = aos_test_helpers:create_assignment(MsgWithWrongFromProcess),
    
    %% Should fail because from-process doesn't match owner
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertMatch(<<"Unauthorized:", _/binary>>, Output).

%% Test existing 'from' field is preserved
existing_from_preserved_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process with owner
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    ProcessMsg = aos_test_helpers:create_process_message(Owner),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    
    %% Message with existing 'from' field
    MsgWithFrom = #{
        <<"id">> => <<"test-msg">>,
        <<"action">> => <<"eval">>,
        <<"data">> => <<"return 'existing from works'">>,
        <<"from">> => Owner,  % Already has 'from'
        <<"from-process">> => <<"SomeOtherUser12345678901234567890123456789">>,  % Should be ignored
        <<"commitments">> => #{
            <<"key1">> => #{
                <<"type">> => <<"RSA-PSS-512">>,
                <<"committer">> => <<"YetAnotherUser1234567890123456789012345678">>
            }
        }
    },
    Assignment = aos_test_helpers:create_assignment(MsgWithFrom),
    
    %% Should work because existing 'from' matches owner
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertEqual(<<"existing from works">>, Output).

%% Test first non-HMAC committer is used when no from-process
first_non_hmac_committer_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process with owner
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    ProcessMsg = aos_test_helpers:create_process_message(Owner),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    
    %% Message with only commitments (no from-process)
    %% Order in maps is not guaranteed, so we'll use a single RSA commitment
    MsgWithCommitments = #{
        <<"id">> => <<"test-msg">>,
        <<"action">> => <<"eval">>,
        <<"data">> => <<"return 'committer works'">>,
        <<"commitments">> => #{
            <<"key1">> => #{
                <<"type">> => <<"hmac-sha256">>
                % No committer for HMAC
            },
            <<"key2">> => #{
                <<"type">> => <<"RSA-PSS-512">>,
                <<"committer">> => Owner
            }
        }
    },
    Assignment = aos_test_helpers:create_assignment(MsgWithCommitments),
    
    %% Should work because RSA committer matches owner
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertEqual(<<"committer works">>, Output).

%% Test no valid from derivation fails
no_valid_from_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process with owner
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    ProcessMsg = aos_test_helpers:create_process_message(Owner),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    
    %% Message with no from-process and no commitments
    MsgNoFrom = #{
        <<"id">> => <<"test-msg">>,
        <<"action">> => <<"eval">>,
        <<"data">> => <<"return 'should fail'">>
    },
    Assignment = aos_test_helpers:create_assignment(MsgNoFrom),
    
    %% Should fail because no valid from can be derived
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertMatch(<<"Unauthorized:", _/binary>>, Output).