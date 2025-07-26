-module(aos_outbox_test).
-export([test_outbox_functionality/0]).

test_outbox_functionality() ->
    io:format("=== Testing Outbox/Send Functionality ===~n~n"),
    
    %% Setup LUERL sandbox
    {ok, LuaState0} = luerl:init(),
    
    %% Load aos.lua
    {ok, AosContent} = file:read_file("../aos.lua"),
    {ok, _, LuaState1} = luerl:do(binary_to_list(AosContent), LuaState0),
    
    %% Create base state
    State = #{
        <<"results">> => #{
            <<"outbox">> => [],
            <<"output">> => #{
                <<"data">> => <<"">>,
                <<"prompt">> => <<"">>
            }
        }
    },
    
    %% Test 1: Single send message
    io:format("Test 1: Single send message~n"),
    test_single_send(State, LuaState1),
    
    %% Test 2: Multiple send messages
    io:format("~nTest 2: Multiple send messages~n"),
    test_multiple_sends(State, LuaState1),
    
    %% Test 3: Send with complex data
    io:format("~nTest 3: Send with complex data~n"),
    test_complex_send(State, LuaState1),
    
    io:format("~n=== Outbox Tests Complete ===~n").

test_single_send(State, LuaState) ->
    Assignment = create_assignment(#{
        <<"action">> => <<"eval">>,
        <<"data">> => <<"send({target='ID', data='hello'})">>
    }),
    
    {ok, Result} = call_compute_and_verify(State, Assignment, LuaState),
    
    case extract_outbox(Result) of
        {ok, [Message]} ->
            verify_message(Message, <<"ID">>, <<"hello">>);
        {ok, Outbox} ->
            io:format("  ✗ ERROR: Expected 1 message, got ~p~n", [length(Outbox)]);
        {error, Reason} ->
            io:format("  ✗ ERROR: ~s~n", [Reason])
    end.

test_multiple_sends(State, LuaState) ->
    Assignment = create_assignment(#{
        <<"action">> => <<"eval">>,
        <<"data">> => <<"send({target='user1', data='msg1'}); send({target='user2', data='msg2'}); send({target='user3', data='msg3'})">>
    }),
    
    {ok, Result} = call_compute_and_verify(State, Assignment, LuaState),
    
    case extract_outbox(Result) of
        {ok, Outbox} when length(Outbox) =:= 3 ->
            io:format("  ✓ SUCCESS: 3 messages in outbox~n"),
            lists:foreach(fun({Idx, Msg}) ->
                Target = <<"user", (integer_to_binary(Idx))/binary>>,
                Data = <<"msg", (integer_to_binary(Idx))/binary>>,
                io:format("  Message ~p: ", [Idx]),
                verify_message(Msg, Target, Data)
            end, lists:zip(lists:seq(1, 3), Outbox));
        {ok, Outbox} ->
            io:format("  ✗ ERROR: Expected 3 messages, got ~p~n", [length(Outbox)]);
        {error, Reason} ->
            io:format("  ✗ ERROR: ~s~n", [Reason])
    end.

test_complex_send(State, LuaState) ->
    Assignment = create_assignment(#{
        <<"action">> => <<"eval">>,
        <<"data">> => <<"send({target='complex-target', data='Complex message with spaces and special chars!', tags={action='notify', type='test'}})">>
    }),
    
    {ok, Result} = call_compute_and_verify(State, Assignment, LuaState),
    
    case extract_outbox(Result) of
        {ok, [Message]} ->
            Target = maps:get(<<"target">>, Message, undefined),
            Data = maps:get(<<"data">>, Message, undefined),
            Tags = maps:get(<<"tags">>, Message, undefined),
            
            if
                Target =:= <<"complex-target">>,
                Data =:= <<"Complex message with spaces and special chars!">> ->
                    io:format("  ✓ SUCCESS: Complex message sent correctly~n"),
                    case Tags of
                        undefined ->
                            io:format("  ! Note: Tags were not preserved~n");
                        _ ->
                            io:format("  ✓ Tags included: ~p~n", [Tags])
                    end;
                true ->
                    io:format("  ✗ ERROR: Message content doesn't match~n")
            end;
        {ok, Outbox} ->
            io:format("  ✗ ERROR: Expected 1 message, got ~p~n", [length(Outbox)]);
        {error, Reason} ->
            io:format("  ✗ ERROR: ~s~n", [Reason])
    end.

create_assignment(Body) ->
    #{
        <<"process-id">> => <<"test-process">>,
        <<"timestamp">> => 1234567890,
        <<"block-height">> => 1000,
        <<"owner">> => <<"test-owner">>,
        <<"body">> => Body#{
            <<"id">> => <<"test-msg-001">>,
            <<"from">> => <<"test-sender">>,
            <<"owner">> => <<"test-owner">>,
            <<"tags">> => []
        }
    }.

call_compute_and_verify(State, Assignment, LuaState) ->
    StateTable = aos_sandbox_test:erlang_to_lua_table(State),
    AssignmentTable = aos_sandbox_test:erlang_to_lua_table(Assignment),
    
    case luerl:call_function([compute], [StateTable, AssignmentTable], LuaState) of
        {ok, [_Status, ReturnedState], _NewLuaState} ->
            ErlangState = aos_sandbox_test:lua_table_to_erlang(ReturnedState),
            {ok, ErlangState};
        {error, Reason, _} ->
            {error, Reason}
    end.

extract_outbox(State) ->
    case maps:get(<<"results">>, State, undefined) of
        undefined ->
            {error, "No results in response"};
        Results ->
            case maps:get(<<"outbox">>, Results, undefined) of
                undefined ->
                    {error, "No outbox in results"};
                Outbox when is_list(Outbox) ->
                    {ok, Outbox};
                _ ->
                    {error, "Outbox is not a list"}
            end
    end.

verify_message(Message, ExpectedTarget, ExpectedData) ->
    case Message of
        Map when is_map(Map) ->
            Target = maps:get(<<"target">>, Map, undefined),
            Data = maps:get(<<"data">>, Map, undefined),
            
            if
                Target =:= ExpectedTarget, Data =:= ExpectedData ->
                    io:format("✓ Correct (target='~s', data='~s')~n", [ExpectedTarget, ExpectedData]);
                true ->
                    io:format("✗ Mismatch (expected target='~s', data='~s', got target='~s', data='~s')~n", 
                             [ExpectedTarget, ExpectedData, Target, Data])
            end;
        _ ->
            io:format("✗ Invalid message format~n")
    end.