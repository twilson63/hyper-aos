-module(aos_test_helpers).
-export([
    default_owner/0,
    create_process_message/0,
    create_process_message/1,
    create_process_assignment/0,
    create_process_assignment/1,
    create_eval_message/1,
    create_eval_message/2,
    create_eval_assignment/1,
    create_eval_assignment/2,
    create_base_state/0,
    create_assignment/1,
    build_lua_table_string/1,
    build_lua_value_string/1,
    initialize_aos/0,
    initialize_process/2,
    ensure_colors_initialized/1,
    call_compute/3,
    extract_output_data/1,
    create_unauthorized_eval_message/1,
    create_message_without_commitments/1
]).

%% Default test owner address
default_owner() ->
    <<"TestOwner123456789012345678901234567890123">>.

%% Create a process initialization message
create_process_message() ->
    create_process_message(default_owner()).

create_process_message(Owner) ->
    #{
        <<"id">> => <<"process-init">>,
        <<"type">> => <<"process">>,
        <<"commitments">> => #{
            <<"init-key">> => #{
                <<"type">> => <<"rsa-pss-512">>,
                <<"committer">> => Owner
            }
        }
    }.

%% Create a process assignment
create_process_assignment() ->
    create_process_assignment(create_process_message()).

create_process_assignment(ProcessMsg) ->
    #{
        <<"process-id">> => <<"test-process">>,
        <<"timestamp">> => 1234567890,
        <<"block-height">> => 1000,
        <<"owner">> => <<"test-owner">>,
        <<"body">> => ProcessMsg
    }.

%% Create an eval message
create_eval_message(Data) ->
    create_eval_message(Data, default_owner()).

create_eval_message(Data, Owner) ->
    #{
        <<"id">> => <<"eval-msg">>,
        <<"action">> => <<"eval">>,
        <<"data">> => Data,
        <<"commitments">> => #{
            <<"key-1234">> => #{
                <<"type">> => <<"rsa-pss-512">>,
                <<"committer">> => Owner
            }
        }
    }.

%% Create an eval assignment
create_eval_assignment(Data) ->
    create_eval_assignment(Data, default_owner()).

create_eval_assignment(Data, Owner) ->
    create_assignment(create_eval_message(Data, Owner)).

%% Create a generic assignment
create_assignment(Body) ->
    #{
        <<"process-id">> => <<"test-process">>,
        <<"timestamp">> => 1234567890,
        <<"block-height">> => 1000,
        <<"owner">> => <<"test-owner">>,
        <<"body">> => Body
    }.

%% Create base state for tests
create_base_state() ->
    #{
        <<"results">> => #{
            <<"outbox">> => [],
            <<"output">> => #{
                <<"data">> => <<"">>,
                <<"prompt">> => <<"">>
            }
        }
    }.

%% Build Lua table string from Erlang map
build_lua_table_string(Map) when is_map(Map) ->
    "{" ++ string:join(
        maps:fold(fun(K, V, Acc) ->
            Key = if
                is_binary(K) -> binary_to_list(K);
                is_atom(K) -> atom_to_list(K);
                true -> K
            end,
            %% Wrap key in brackets if it contains special characters
            SafeKey = case lists:any(fun(C) -> not (C >= $a andalso C =< $z) andalso 
                                               not (C >= $A andalso C =< $Z) andalso
                                               not (C >= $0 andalso C =< $9) andalso
                                               C =/= $_ end, Key) of
                true -> io_lib:format("['~s']", [Key]);
                false -> Key
            end,
            ValueStr = build_lua_value_string(V),
            [io_lib:format("~s = ~s", [SafeKey, ValueStr]) | Acc]
        end, [], Map),
        ", "
    ) ++ "}";
build_lua_table_string(List) when is_list(List) ->
    "{" ++ string:join(
        lists:map(fun build_lua_value_string/1, List),
        ", "
    ) ++ "}".

build_lua_value_string(V) when is_map(V) ->
    build_lua_table_string(V);
build_lua_value_string([]) ->
    "{}";
build_lua_value_string(V) when is_list(V) ->
    build_lua_table_string(V);
build_lua_value_string(V) when is_binary(V) ->
    %% Escape single quotes in the string
    Str = binary_to_list(V),
    EscapedStr = lists:flatten(string:replace(Str, "'", "\\'", all)),
    io_lib:format("'~s'", [EscapedStr]);
build_lua_value_string(V) when is_atom(V) ->
    io_lib:format("'~s'", [atom_to_list(V)]);
build_lua_value_string(V) when is_integer(V) ->
    integer_to_list(V);
build_lua_value_string(V) when is_float(V) ->
    float_to_list(V);
build_lua_value_string(true) ->
    "true";
build_lua_value_string(false) ->
    "false";
build_lua_value_string(_) ->
    "nil".

%% Initialize AOS with loaded Lua state
initialize_aos() ->
    LuaState0 = luerl:init(),
    {ok, AosContent} = file:read_file("../aos.lua"),
    AosCode = binary_to_list(AosContent),
    case luerl:do(AosCode, LuaState0) of
        {ok, _, LuaState1} -> 
            LuaState1;
        {error, {lua_error, {compile_error, {syntax_error, Details}}, _}} ->
            error({aos_syntax_error, Details});
        {error, Error, _} ->
            error({aos_load_error, Error})
    end.

%% Initialize a process with an owner
initialize_process(LuaState, State) ->
    ProcessAssignment = create_process_assignment(),
    case call_compute(LuaState, State, ProcessAssignment) of
        {[_Status, _Result], NewLuaState} -> 
            ensure_colors_initialized(NewLuaState);
        {error, _Error, NewLuaState} -> 
            ensure_colors_initialized(NewLuaState);
        Error -> error({initialize_process_failed, Error})
    end.

%% Ensure colors are available in _G.colors for stringify function
ensure_colors_initialized(LuaState) ->
    %% Copy meta.colors to _G.colors if colors are stored in meta
    {ok, _, LuaState2} = luerl:do("if _G.meta and _G.meta.colors then _G.colors = _G.meta.colors end", LuaState),
    LuaState2.

%% Call compute function
call_compute(LuaState, State, Assignment) ->
    StateStr = build_lua_table_string(State),
    AssignmentStr = build_lua_table_string(Assignment),
    
    LuaCode = lists:flatten([
        "local state = ", StateStr, "\n",
        "local assignment = ", AssignmentStr, "\n",
        "local status, result = compute(state, assignment)\n",
        "return status, result\n"
    ]),
    
    case luerl:do(LuaCode, LuaState) of
        {ok, Result, NewLuaState} -> {Result, NewLuaState};
        {error, Error, NewLuaState} -> {error, Error, NewLuaState};
        Error -> Error
    end.

%% Extract output data from result
extract_output_data(Result) ->
    case Result of
        Binary when is_binary(Binary) ->
            Binary;
        {[_Status, StateTable], LuerlStateStruct} when is_tuple(StateTable) ->
            %% Use simple Lua script to extract the data
            LuaCode = "
            if _G.filtered_state and _G.filtered_state.results and 
               _G.filtered_state.results.output and _G.filtered_state.results.output.data then
                return _G.filtered_state.results.output.data
            else
                return 'path_not_found'
            end
            ",
            
            try
                case luerl:set_table_keys([<<"filtered_state">>], StateTable, LuerlStateStruct) of
                    {ok, LuerlState2} ->
                        case luerl:do(LuaCode, LuerlState2) of
                            {ok, [Data], _NewState} when is_binary(Data) -> Data;
                            {ok, [Data], _NewState} -> iolist_to_binary(io_lib:format("~p", [Data]));
                            {ok, Data, _NewState} when is_binary(Data) -> Data;  
                            {ok, Data, _NewState} -> iolist_to_binary(io_lib:format("~p", [Data]));
                            {error, Error, _} -> iolist_to_binary(io_lib:format("lua_error:~p", [Error]));
                            Other -> iolist_to_binary(io_lib:format("unexpected:~p", [Other]))
                        end;
                    SetError -> iolist_to_binary(io_lib:format("set_error:~p", [SetError]))
                end
            catch
                ExceptionClass:ExceptionReason ->
                    iolist_to_binary(io_lib:format("exception:~p:~p", [ExceptionClass, ExceptionReason]))
            end;
        Other ->
            %% Fallback case  
            <<"UNKNOWN_FORMAT">>
    end.

%% Helper function to extract nested values from Luerl table representation
extract_nested_value(Table, Keys) when is_list(Table), is_list(Keys) ->
    extract_nested_value_helper(Table, Keys);
extract_nested_value(_, _) ->
    undefined.

extract_nested_value_helper(Table, []) ->
    Table;
extract_nested_value_helper(Table, [Key | RestKeys]) when is_list(Table) ->
    case find_key_in_luerl_table(Table, Key) of
        undefined -> undefined;
        Value -> extract_nested_value_helper(Value, RestKeys)
    end;
extract_nested_value_helper(_, _) ->
    undefined.

%% Helper function to find a key in Luerl's table representation
%% Luerl represents Lua tables as lists of {Key, Value} tuples
find_key_in_luerl_table([], _Key) ->
    undefined;
find_key_in_luerl_table([{K, V} | _Rest], Key) when K =:= Key ->
    V;
find_key_in_luerl_table([{K, V} | _Rest], Key) when is_binary(K), is_list(Key) ->
    case binary_to_list(K) =:= Key of
        true -> V;
        false -> find_key_in_luerl_table(_Rest, Key)
    end;
find_key_in_luerl_table([{K, V} | _Rest], Key) when is_list(K), is_list(Key) ->
    case K =:= Key of
        true -> V;
        false -> find_key_in_luerl_table(_Rest, Key)
    end;
find_key_in_luerl_table([_H | Rest], Key) ->
    find_key_in_luerl_table(Rest, Key).

%% Helper function to extract nested values from Luerl table references
luerl_extract_nested_value(TableRef, Keys, LuerlState) ->
    luerl_extract_nested_helper(TableRef, Keys, LuerlState).

luerl_extract_nested_helper(TableRef, [], _LuerlState) ->
    TableRef;
luerl_extract_nested_helper({tref, N}, [Key | RestKeys], {tstruct, TableMap}) ->
    case maps:get(N, TableMap, undefined) of
        undefined -> undefined;
        {table, _ArrayPart, TablePart, _Meta} ->
            case find_key_in_luerl_table_part(TablePart, Key) of
                undefined -> undefined;
                Value -> luerl_extract_nested_helper(Value, RestKeys, {tstruct, TableMap})
            end;
        _Other -> undefined
    end;
luerl_extract_nested_helper(_Other, _Keys, _LuerlState) ->
    undefined.

%% Helper to find key in Luerl table part (complex nested structure)
find_key_in_luerl_table_part(empty, _Key) ->
    undefined;
find_key_in_luerl_table_part({empty, K, V, Rest}, Key) when is_binary(K) ->
    case binary_to_list(K) =:= Key of
        true -> V;
        false -> find_key_in_luerl_table_part(Rest, Key)
    end;
find_key_in_luerl_table_part({K, V, Rest}, Key) when is_binary(K) ->
    case binary_to_list(K) =:= Key of
        true -> V;
        false -> find_key_in_luerl_table_part(Rest, Key)
    end;
find_key_in_luerl_table_part({A, B, Rest}, Key) ->
    case find_key_in_luerl_table_part(A, Key) of
        undefined -> 
            case find_key_in_luerl_table_part(B, Key) of
                undefined -> find_key_in_luerl_table_part(Rest, Key);
                Value -> Value
            end;
        Value -> Value
    end;
find_key_in_luerl_table_part(nil, _Key) ->
    undefined;
find_key_in_luerl_table_part(_Other, _Key) ->
    undefined.

%% Create an unauthorized eval message (different committer)
create_unauthorized_eval_message(Data) ->
    #{
        <<"id">> => <<"eval-msg">>,
        <<"action">> => <<"eval">>,
        <<"data">> => Data,
        <<"commitments">> => #{
            <<"key-1234">> => #{
                <<"type">> => <<"rsa-pss-512">>,
                <<"committer">> => <<"UnauthorizedUser123456789012345678901234567">>
            }
        }
    }.

%% Create a message without commitments
create_message_without_commitments(Data) ->
    #{
        <<"id">> => <<"eval-msg">>,
        <<"action">> => <<"eval">>,
        <<"data">> => Data
    }.