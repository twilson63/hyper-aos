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
    {_, LuaState1} = luerl:do(binary_to_list(AosContent), LuaState0),
    LuaState1.

%% Initialize a process with an owner
initialize_process(LuaState, State) ->
    ProcessAssignment = create_process_assignment(),
    {[_, _], NewLuaState} = call_compute(LuaState, State, ProcessAssignment),
    NewLuaState.

%% Call compute function
call_compute(LuaState, State, Assignment) ->
    StateStr = build_lua_table_string(State),
    AssignmentStr = build_lua_table_string(Assignment),
    
    LuaCode = lists:flatten([
        "local state = ", StateStr, "\n",
        "local assignment = ", AssignmentStr, "\n",
        "local status, result = compute(state, assignment)\n",
        "return result.results.output.data, result.results.status\n"
    ]),
    
    luerl:do(LuaCode, LuaState).

%% Extract output data from result
extract_output_data(Result) when is_binary(Result) ->
    Result;
extract_output_data({[Data, _Status], _LuaState}) ->
    iolist_to_binary(Data);
extract_output_data(_) ->
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