-module(test_luerl_encode).
-export([test/0]).

test() ->
    LuaState0 = luerl:init(),
    {ok, AosContent} = file:read_file("../aos.lua"),
    {_, LuaState1} = luerl:do(binary_to_list(AosContent), LuaState0),
    
    % Create a wrapper function that handles table conversion
    WrapperCode = "
        function call_compute_wrapper(state_list, assignment_list)
            -- Convert lists to proper tables
            local function list_to_table(list)
                local t = {}
                for i = 1, #list do
                    local pair = list[i]
                    if type(pair) == 'table' and #pair == 2 then
                        t[pair[1]] = pair[2]
                    end
                end
                return t
            end
            
            -- Recursively convert nested structures
            local function convert_structure(v)
                if type(v) == 'table' then
                    -- Check if it's a list of pairs
                    local is_pair_list = true
                    for i, item in ipairs(v) do
                        if type(item) ~= 'table' or #item ~= 2 then
                            is_pair_list = false
                            break
                        end
                    end
                    
                    if is_pair_list and #v > 0 then
                        -- Convert pair list to table
                        local t = {}
                        for i = 1, #v do
                            t[v[i][1]] = convert_structure(v[i][2])
                        end
                        return t
                    else
                        -- Regular array, convert elements
                        local t = {}
                        for k, val in pairs(v) do
                            t[k] = convert_structure(val)
                        end
                        return t
                    end
                else
                    return v
                end
            end
            
            local state = convert_structure(state_list)
            local assignment = convert_structure(assignment_list)
            
            -- Debug: Check the converted structures
            print('DEBUG: assignment.body = ' .. tostring(assignment.body))
            if assignment.body then
                print('DEBUG: assignment.body.action = ' .. tostring(assignment.body.action))
                print('DEBUG: assignment.body.data = ' .. tostring(assignment.body.data))
            end
            
            return compute(state, assignment)
        end
    ",
    
    {_, LuaState2} = luerl:do(WrapperCode, LuaState1),
    
    % Now test with our wrapper
    State = #{
        <<"results">> => #{
            <<"outbox">> => [],
            <<"output">> => #{<<"data">> => <<"">>, <<"prompt">> => <<"">>}
        }
    },
    
    Message = #{
        <<"id">> => <<"test">>,
        <<"from">> => <<"test">>,
        <<"owner">> => <<"test">>,
        <<"tags">> => [],
        <<"action">> => <<"eval">>,
        <<"data">> => <<"return 777">>
    },
    
    Assignment = #{
        <<"body">> => Message
    },
    
    % Convert to pair lists
    StateList = map_to_pair_list(State),
    AssignmentList = map_to_pair_list(Assignment),
    
    io:format("StateList: ~p~n", [StateList]),
    io:format("AssignmentList: ~p~n", [AssignmentList]),
    
    % Call the wrapper
    {[Status, Result], _} = luerl:call_function([call_compute_wrapper], [StateList, AssignmentList], LuaState2),
    io:format("Status: ~p~n", [Status]),
    
    % Print the raw result first
    io:format("Raw result: ~p~n", [Result]),
    
    % Try to extract the data
    case extract_output_data(Result) of
        {ok, Data} ->
            io:format("Output data: ~p~n", [Data]);
        error ->
            io:format("Failed to extract output data~n"),
            % Try the lua_table_to_erlang approach
            case catch lua_table_to_erlang(Result) of
                Map when is_map(Map) ->
                    io:format("Converted to map: ~p~n", [Map]),
                    case maps:get(<<"results">>, Map, undefined) of
                        undefined -> ok;
                        Results ->
                            case maps:get(<<"output">>, Results, undefined) of
                                undefined -> ok;
                                Output ->
                                    Data = maps:get(<<"data">>, Output, <<"no data">>),
                                    io:format("Found data via map conversion: ~p~n", [Data]),
                                    io:format("Full output map: ~p~n", [Output])
                            end
                    end;
                Error ->
                    io:format("Conversion error: ~p~n", [Error])
            end
    end,
    
    ok.

lua_table_to_erlang(Table) when is_list(Table) ->
    lists:foldl(fun
        ({K, V}, Acc) when is_integer(K) ->
            Acc;
        ({K, V}, Acc) when is_list(K) ->
            case catch list_to_binary(K) of
                Key when is_binary(Key) ->
                    Value = lua_value_to_erlang(V),
                    maps:put(Key, Value, Acc);
                _ ->
                    Acc
            end;
        ({K, V}, Acc) ->
            Value = lua_value_to_erlang(V),
            maps:put(K, Value, Acc)
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

map_to_pair_list(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) ->
        Key = if
            is_binary(K) -> binary_to_list(K);
            is_atom(K) -> atom_to_list(K);
            true -> K
        end,
        Value = value_to_lua(V),
        [[Key, Value] | Acc]
    end, [], Map).

value_to_lua(V) when is_map(V) ->
    map_to_pair_list(V);
value_to_lua([]) ->
    [];
value_to_lua(V) when is_list(V) ->
    [value_to_lua(Item) || Item <- V];
value_to_lua(V) when is_binary(V) ->
    binary_to_list(V);
value_to_lua(V) ->
    V.

extract_output_data(Result) when is_list(Result) ->
    % Navigate through the structure to find output.data
    case lists:keyfind("results", 1, Result) of
        {"results", Results} ->
            case lists:keyfind("output", 1, Results) of
                {"output", Output} ->
                    case lists:keyfind("data", 1, Output) of
                        {"data", Data} ->
                            {ok, Data};
                        _ -> error
                    end;
                _ -> error
            end;
        _ -> error
    end;
extract_output_data(_) ->
    error.