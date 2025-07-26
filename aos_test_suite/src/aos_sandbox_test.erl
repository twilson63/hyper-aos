-module(aos_sandbox_test).
-export([run_tests/0, test_compute/0, setup_sandbox/0]).

setup_sandbox() ->
    {ok, State} = luerl:init(),
    
    %% Load the aos.lua file
    {ok, AosContent} = file:read_file("../aos.lua"),
    
    %% Execute aos.lua in the sandbox
    case luerl:do(binary_to_list(AosContent), State) of
        {ok, _Result, NewState} ->
            {ok, NewState};
        {error, Reason, _} ->
            {error, Reason}
    end.

create_base_message() ->
    #{
        <<"id">> => <<"test-msg-001">>,
        <<"from">> => <<"test-process">>,
        <<"owner">> => <<"test-owner">>,
        <<"tags">> => [],
        <<"block-height">> => 1000,
        <<"timestamp">> => 1234567890,
        <<"module">> => <<"test-module">>
    }.

create_assignment(Body) ->
    #{
        <<"process-id">> => <<"test-process-id">>,
        <<"timestamp">> => 1234567890,
        <<"block-height">> => 1000,
        <<"owner">> => <<"test-owner">>,
        <<"body">> => Body
    }.

test_compute() ->
    case setup_sandbox() of
        {ok, LuaState} ->
            %% Create initial state structure
            InitialState = #{
                <<"results">> => #{
                    <<"outbox">> => [],
                    <<"output">> => #{<<"data">> => <<"">>, <<"prompt">> => <<"">>}
                }
            },
            
            %% Test 1: Basic message without action
            io:format("Test 1: Basic message without action~n"),
            BaseMsg = create_base_message(),
            Assignment1 = create_assignment(BaseMsg),
            
            {ok, Result1, LuaState2} = call_compute(LuaState, InitialState, Assignment1),
            io:format("Result 1: ~p~n", [Result1]),
            
            %% Test 2: Message with eval action
            io:format("~nTest 2: Message with eval action~n"),
            EvalMsg = BaseMsg#{
                <<"action">> => <<"eval">>,
                <<"data">> => <<"return 2 + 2">>
            },
            Assignment2 = create_assignment(EvalMsg),
            
            {ok, Result2, LuaState3} = call_compute(LuaState2, InitialState, Assignment2),
            io:format("Result 2: ~p~n", [Result2]),
            
            %% Test 3: Message with print action
            io:format("~nTest 3: Testing print function~n"),
            PrintMsg = BaseMsg#{
                <<"action">> => <<"eval">>,
                <<"data">> => <<"print('Hello from sandbox'); return 'ok'">>
            },
            Assignment3 = create_assignment(PrintMsg),
            
            {ok, Result3, LuaState4} = call_compute(LuaState3, InitialState, Assignment3),
            io:format("Result 3: ~p~n", [Result3]),
            
            %% Test 4: Testing send function
            io:format("~nTest 4: Testing send function~n"),
            SendMsg = BaseMsg#{
                <<"action">> => <<"eval">>,
                <<"data">> => <<"send({target='recipient', data='test message'}); return 'sent'">>
            },
            Assignment4 = create_assignment(SendMsg),
            
            {ok, Result4, LuaState5} = call_compute(LuaState4, InitialState, Assignment4),
            io:format("Result 4: ~p~n", [Result4]),
            
            %% Test 5: Testing require with .process._version
            io:format("~nTest 5: Testing require('.process')._version~n"),
            RequireMsg = BaseMsg#{
                <<"action">> => <<"eval">>,
                <<"data">> => <<"return require('.process')._version">>
            },
            Assignment5 = create_assignment(RequireMsg),
            
            {ok, Result5, LuaState6} = call_compute(LuaState5, InitialState, Assignment5),
            io:format("Result 5: ~p~n", [Result5]),
            
            %% Verify the version response
            case maps:get(<<"results">>, Result5, undefined) of
                undefined ->
                    io:format("ERROR: No results in response~n");
                Results ->
                    case maps:get(<<"output">>, Results, undefined) of
                        undefined ->
                            io:format("ERROR: No output in results~n");
                        Output ->
                            case maps:get(<<"data">>, Output, undefined) of
                                undefined ->
                                    io:format("ERROR: No data in output~n");
                                <<"dev">> ->
                                    io:format("SUCCESS: Version 'dev' returned as expected~n");
                                OtherVersion ->
                                    io:format("Version returned: ~p~n", [OtherVersion])
                            end
                    end
            end,
            
            %% Test 6: Testing 1 + 1 evaluation
            io:format("~nTest 6: Testing 1 + 1 evaluation~n"),
            MathMsg = BaseMsg#{
                <<"action">> => <<"eval">>,
                <<"data">> => <<"return 1 + 1">>
            },
            Assignment6 = create_assignment(MathMsg),
            
            {ok, Result6, LuaState7} = call_compute(LuaState6, InitialState, Assignment6),
            io:format("Result 6: ~p~n", [Result6]),
            
            %% Verify the math result
            case maps:get(<<"results">>, Result6, undefined) of
                undefined ->
                    io:format("ERROR: No results in response~n");
                Results6 ->
                    case maps:get(<<"output">>, Results6, undefined) of
                        undefined ->
                            io:format("ERROR: No output in results~n");
                        Output6 ->
                            case maps:get(<<"data">>, Output6, undefined) of
                                undefined ->
                                    io:format("ERROR: No data in output~n");
                                <<"2">> ->
                                    io:format("SUCCESS: 1 + 1 = 2 as expected!~n");
                                OtherResult ->
                                    io:format("UNEXPECTED: Got ~p instead of 2~n", [OtherResult])
                            end
                    end
            end,
            
            %% Test 7: Testing send/outbox functionality
            io:format("~nTest 7: Testing send/outbox functionality~n"),
            SendOutboxMsg = BaseMsg#{
                <<"action">> => <<"eval">>,
                <<"data">> => <<"send({target='ID', data='hello'})">>
            },
            Assignment7 = create_assignment(SendOutboxMsg),
            
            {ok, Result7, _LuaState8} = call_compute(LuaState7, InitialState, Assignment7),
            io:format("Result 7: ~p~n", [Result7]),
            
            %% Verify the outbox contains our message
            case maps:get(<<"results">>, Result7, undefined) of
                undefined ->
                    io:format("ERROR: No results in response~n");
                Results7 ->
                    case maps:get(<<"outbox">>, Results7, undefined) of
                        undefined ->
                            io:format("ERROR: No outbox in results~n");
                        Outbox when is_list(Outbox) ->
                            case length(Outbox) of
                                0 ->
                                    io:format("ERROR: Outbox is empty~n");
                                _ ->
                                    io:format("Outbox contains ~p message(s)~n", [length(Outbox)]),
                                    [FirstMsg | _] = Outbox,
                                    io:format("First message in outbox: ~p~n", [FirstMsg]),
                                    
                                    %% Verify message structure
                                    case FirstMsg of
                                        Map when is_map(Map) ->
                                            Target = maps:get(<<"target">>, Map, undefined),
                                            Data = maps:get(<<"data">>, Map, undefined),
                                            io:format("Target: ~p, Data: ~p~n", [Target, Data]),
                                            
                                            if
                                                Target =:= <<"ID">>, Data =:= <<"hello">> ->
                                                    io:format("SUCCESS: Message correctly added to outbox!~n");
                                                true ->
                                                    io:format("ERROR: Message content doesn't match expected values~n")
                                            end;
                                        _ ->
                                            io:format("ERROR: Unexpected message format in outbox~n")
                                    end
                            end;
                        _ ->
                            io:format("ERROR: Outbox is not a list~n")
                    end
            end,
            
            ok;
        {error, Reason} ->
            io:format("Failed to setup sandbox: ~p~n", [Reason]),
            error
    end.

call_compute(LuaState, State, Assignment) ->
    %% Convert Erlang maps to Lua tables
    StateTable = erlang_to_lua_table(State),
    AssignmentTable = erlang_to_lua_table(Assignment),
    
    %% Call the compute function
    case luerl:call_function([compute], [StateTable, AssignmentTable], LuaState) of
        {ok, [<<"ok">>, ReturnedState], NewLuaState} ->
            %% Convert Lua table back to Erlang map
            ErlangState = lua_table_to_erlang(ReturnedState),
            {ok, ErlangState, NewLuaState};
        {error, Reason, _} ->
            {error, Reason}
    end.

erlang_to_lua_table(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) ->
        Key = if
            is_binary(K) -> binary_to_list(K);
            is_atom(K) -> atom_to_list(K);
            true -> K
        end,
        Value = erlang_to_lua_value(V),
        [{Key, Value} | Acc]
    end, [], Map);
erlang_to_lua_table(List) when is_list(List) ->
    List.

erlang_to_lua_value(V) when is_map(V) ->
    erlang_to_lua_table(V);
erlang_to_lua_value(V) when is_list(V) ->
    V;
erlang_to_lua_value(V) when is_binary(V) ->
    binary_to_list(V);
erlang_to_lua_value(V) when is_atom(V) ->
    atom_to_list(V);
erlang_to_lua_value(V) ->
    V.

lua_table_to_erlang(Table) when is_list(Table) ->
    lists:foldl(fun({K, V}, Acc) ->
        Key = if
            is_list(K) -> list_to_binary(K);
            true -> K
        end,
        Value = lua_value_to_erlang(V),
        maps:put(Key, Value, Acc)
    end, #{}, Table);
lua_table_to_erlang(Value) ->
    Value.

lua_value_to_erlang(V) when is_list(V) ->
    case is_lua_table(V) of
        true -> lua_table_to_erlang(V);
        false -> 
            try list_to_binary(V)
            catch _:_ -> V
            end
    end;
lua_value_to_erlang(V) ->
    V.

is_lua_table([]) -> true;
is_lua_table([{_, _} | _]) -> true;
is_lua_table(_) -> false.

run_tests() ->
    io:format("=== Running AOS Sandbox Tests ===~n~n"),
    test_compute(),
    io:format("~n=== Tests Complete ===~n").