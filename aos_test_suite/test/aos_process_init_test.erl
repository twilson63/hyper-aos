-module(aos_process_init_test).
-include_lib("eunit/include/eunit.hrl").

%% Test process message sets owner
process_message_sets_owner_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Process message with commitments
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    ProcessMsg = aos_test_helpers:create_process_message(Owner),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    
    %% Verify owner was set by testing eval with matching committer
    EvalMsg = aos_test_helpers:create_eval_message(<<"return 'owner initialized'">>, Owner),
    EvalAssignment = aos_test_helpers:create_assignment(EvalMsg),
    Result = aos_test_helpers:call_compute(LuaState2, State, EvalAssignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertEqual(<<"owner initialized">>, Output).

%% Test non-process message ignored
non_process_message_ignored_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Non-process message with commitments
    NonProcessMsg = #{
        <<"id">> => <<"regular-msg">>,
        <<"type">> => <<"message">>, % Not "process"
        <<"commitments">> => #{
            <<"key1">> => #{
                <<"type">> => <<"RSA-PSS-512">>,
                <<"committer">> => <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>
            }
        }
    },
    Assignment = aos_test_helpers:create_assignment(NonProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, Assignment),
    
    %% Verify owner was NOT set by testing eval (should fail without commitments)
    EvalMsg = #{
        <<"id">> => <<"eval-test">>,
        <<"action">> => <<"eval">>,
        <<"data">> => <<"return 'should fail'">>
    },
    EvalAssignment = aos_test_helpers:create_assignment(EvalMsg),
    Result = aos_test_helpers:call_compute(LuaState2, State, EvalAssignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertMatch(<<"Unauthorized:", _/binary>>, Output).

%% Test hmac commitment skipped
hmac_commitment_skipped_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Process message with mixed commitments
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    MixedCommitMsg = #{
        <<"id">> => <<"process-msg">>,
        <<"type">> => <<"process">>,
        <<"commitments">> => #{
            <<"key1">> => #{
                <<"type">> => <<"hmac-sha256">>,
                <<"committer">> => <<"ShouldBeIgnored123456789012345678901234567890">>
            },
            <<"key2">> => #{
                <<"type">> => <<"RSA-PSS-512">>,
                <<"committer">> => Owner
            }
        }
    },
    Assignment = aos_test_helpers:create_assignment(MixedCommitMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, Assignment),
    
    %% Verify owner was set from RSA-PSS-512 by testing eval
    EvalMsg = aos_test_helpers:create_eval_message(<<"return 'correct owner'">>, Owner),
    EvalAssignment = aos_test_helpers:create_assignment(EvalMsg),
    Result = aos_test_helpers:call_compute(LuaState2, State, EvalAssignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertEqual(<<"correct owner">>, Output).

%% Test initialized flag prevents reset
initialized_flag_prevents_reset_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% First initialize with original owner
    OriginalOwner = <<"OriginalOwner123456789012345678901234567890">>,
    ProcessMsg1 = aos_test_helpers:create_process_message(OriginalOwner),
    Assignment1 = aos_test_helpers:create_assignment(ProcessMsg1),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, Assignment1),
    
    %% Another process message trying to set owner
    NewOwner = <<"NewOwner123456789012345678901234567890">>,
    ProcessMsg2 = aos_test_helpers:create_process_message(NewOwner),
    Assignment2 = aos_test_helpers:create_assignment(ProcessMsg2),
    {_, LuaState3} = aos_test_helpers:call_compute(LuaState2, State, Assignment2),
    
    %% Verify owner was NOT changed by testing eval with original owner
    EvalMsg = aos_test_helpers:create_eval_message(<<"return 'original owner'">>, OriginalOwner),
    EvalAssignment = aos_test_helpers:create_assignment(EvalMsg),
    Result = aos_test_helpers:call_compute(LuaState3, State, EvalAssignment),
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertEqual(<<"original owner">>, Output).

%% Test eval after process init
eval_after_process_init_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% First: Process message to set owner
    Owner = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
    ProcessMsg = aos_test_helpers:create_process_message(Owner),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState, State, ProcessAssignment),
    
    %% Second: Eval message with matching committer
    EvalMsg = aos_test_helpers:create_eval_message(<<"return 'authorized'">>, Owner),
    EvalAssignment = aos_test_helpers:create_assignment(EvalMsg),
    Result = aos_test_helpers:call_compute(LuaState2, State, EvalAssignment),
    
    %% Verify eval succeeded with matching committer
    Output = aos_test_helpers:extract_output_data(Result),
    ?assertEqual(<<"authorized">>, Output).

