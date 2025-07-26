-module(aos_require_test).
-export([test_require_process_version/0]).

test_require_process_version() ->
    io:format("=== Testing require('.process')._version ===~n~n"),
    
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
    
    %% Create assignment with require action
    Assignment = #{
        <<"process-id">> => <<"test-process">>,
        <<"timestamp">> => 1234567890,
        <<"block-height">> => 1000,
        <<"owner">> => <<"test-owner">>,
        <<"body">> => #{
            <<"id">> => <<"test-msg-001">>,
            <<"from">> => <<"test-sender">>,
            <<"owner">> => <<"test-owner">>,
            <<"tags">> => [],
            <<"action">> => <<"eval">>,
            <<"data">> => <<"return require('.process')._version">>
        }
    },
    
    %% Convert to Lua tables
    StateTable = aos_sandbox_test:erlang_to_lua_table(State),
    AssignmentTable = aos_sandbox_test:erlang_to_lua_table(Assignment),
    
    %% Call compute function
    io:format("Calling compute with require('.process')._version...~n"),
    case luerl:call_function([compute], [StateTable, AssignmentTable], LuaState1) of
        {ok, [Status, ReturnedState], _NewLuaState} ->
            io:format("Status: ~p~n", [Status]),
            
            %% Convert back to Erlang
            ErlangState = aos_sandbox_test:lua_table_to_erlang(ReturnedState),
            
            %% Extract and verify version
            Version = extract_version(ErlangState),
            io:format("~nExtracted version: ~p~n", [Version]),
            
            case Version of
                <<"dev">> ->
                    io:format("~n✓ SUCCESS: Version 'dev' returned as expected!~n");
                undefined ->
                    io:format("~n✗ ERROR: Could not extract version from response~n"),
                    io:format("Full response: ~p~n", [ErlangState]);
                Other ->
                    io:format("~n! UNEXPECTED: Version '~p' returned (expected 'dev')~n", [Other])
            end;
        {error, Reason, _} ->
            io:format("~n✗ ERROR calling compute: ~p~n", [Reason])
    end,
    
    io:format("~n=== Test Complete ===~n").

extract_version(State) ->
    case maps:get(<<"results">>, State, undefined) of
        undefined -> undefined;
        Results ->
            case maps:get(<<"output">>, Results, undefined) of
                undefined -> undefined;
                Output ->
                    maps:get(<<"data">>, Output, undefined)
            end
    end.