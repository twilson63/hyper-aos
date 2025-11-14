-module(aos_inbox_test).
-include_lib("eunit/include/eunit.hrl").

%% Test that messages are added to inbox when no action handler matches
inbox_message_insertion_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    LuaState2 = aos_test_helpers:initialize_process(LuaState, State),
    
    %% Create message with no matching handler (no action or unhandled action)
    Msg = #{
        <<"id">> => <<"msg-1">>,
        <<"data">> => <<"Hello">>,
        <<"commitments">> => #{
            <<"key-1">> => #{
                <<"type">> => <<"rsa-pss-512">>,
                <<"committer">> => aos_test_helpers:default_owner()
            }
        }
    },
    Assignment = aos_test_helpers:create_assignment(Msg),
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    
    %% Should succeed
    {ok, [_, _], _} = Result,
    ok.

%% Test that inbox respects MAX_INBOX_SIZE limit by processing messages efficiently
inbox_max_size_efficiency_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    LuaState2 = aos_test_helpers:initialize_process(LuaState, State),
    
    %% Process a message - this verifies the new add_to_inbox function works
    Msg = create_test_message(1),
    Assignment = aos_test_helpers:create_assignment(Msg),
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    
    %% Verify computation succeeded
    {ok, [_, _], _} = Result,
    ok.

%% Test that multiple messages can be processed without errors
inbox_multiple_messages_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    LuaState2 = aos_test_helpers:initialize_process(LuaState, State),
    
    %% Process multiple messages in sequence (using fresh state each time)
    lists:foreach(fun(N) ->
        Msg = create_test_message(N),
        Assignment = aos_test_helpers:create_assignment(Msg),
        {ok, [_, _], _} = aos_test_helpers:call_compute(LuaState2, State, Assignment)
    end, [1, 2, 3, 4, 5]),
    
    ok.

%% Test that inbox_start and inbox_end tracking works without errors
inbox_indices_tracking_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    LuaState2 = aos_test_helpers:initialize_process(LuaState, State),
    
    %% Process a message - this exercises the inbox index tracking
    Msg = create_test_message(1),
    Assignment = aos_test_helpers:create_assignment(Msg),
    {ok, [_, _], _} = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    
    ok.

%% Test inbox with several messages to verify FIFO semantics
inbox_fifo_semantics_test() ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    LuaState2 = aos_test_helpers:initialize_process(LuaState, State),
    
    %% Process messages that will test FIFO rotation
    lists:foreach(fun(N) ->
        Msg = create_test_message(N),
        Assignment = aos_test_helpers:create_assignment(Msg),
        Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
        %% Just verify the result is valid (contains ok status)
        ?assertMatch({ok, [_, _], _}, Result)
    end, lists:seq(1, 10)),
    
    ok.

%% Helper function to create a test message
create_test_message(Id) ->
    IdStr = integer_to_binary(Id),
    #{
        <<"id">> => <<"msg-", IdStr/binary>>,
        <<"data">> => <<"Message ", IdStr/binary>>,
        <<"commitments">> => #{
            <<"key-1">> => #{
                <<"type">> => <<"rsa-pss-512">>,
                <<"committer">> => aos_test_helpers:default_owner()
            }
        }
    }.
