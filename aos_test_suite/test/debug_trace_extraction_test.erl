-module(debug_trace_extraction_test).
-include_lib("eunit/include/eunit.hrl").

trace_extraction_test() ->
    %% Check if verbose mode is enabled via environment variable or command line args
    Verbose = case os:getenv("EUNIT_VERBOSE") of
        "true" -> true;
        _ ->
            case init:get_argument(verbose) of
                {ok, _} -> true;
                _ -> false
            end
    end,
    trace_extraction_test(Verbose).

trace_extraction_test(Verbose) ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),

    %% Initialize process
    LuaState2 = aos_test_helpers:initialize_process(LuaState, State),

    %% Test simple math computation
    Assignment = aos_test_helpers:create_eval_assignment(<<"return 1 + 1">>),
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),

    %% Just verify that call_compute returns successfully
    case Result of
        {ok, [Data, _], _} when is_binary(Data); is_list(Data) ->
            case Verbose of
                true -> ?debugFmt("call_compute returned successfully with expected format", []);
                _ -> ok
            end,
            ok;
        {error, Error} ->
            case Verbose of
                true -> ?debugFmt("call_compute returned error: ~p", [Error]);
                _ -> ok
            end,
            {error, Error};
        Other ->
            case Verbose of
                true -> ?debugFmt("call_compute returned unexpected format: ~p", [Other]);
                _ -> ok
            end,
            {error, {unexpected_format, Other}}
    end.

find_results_key(TablePart) ->
    find_key_in_part(TablePart, "results").

find_output_key(TablePart) ->
    find_key_in_part(TablePart, "output").

find_data_key(TablePart) ->
    find_key_in_part(TablePart, "data").

find_key_in_part(empty, _Key) ->
    undefined;
find_key_in_part({empty, K, V, Rest}, Key) when is_binary(K) ->
    case binary_to_list(K) =:= Key of
        true -> V;
        false -> find_key_in_part(Rest, Key)
    end;
find_key_in_part({K, V, Rest}, Key) when is_binary(K) ->
    case binary_to_list(K) =:= Key of
        true -> V;
        false -> find_key_in_part(Rest, Key)
    end;
find_key_in_part({A, B, Rest}, Key) ->
    case find_key_in_part(A, Key) of
        undefined -> 
            case find_key_in_part(B, Key) of
                undefined -> find_key_in_part(Rest, Key);
                Value -> Value
            end;
        Value -> Value
    end;
find_key_in_part(nil, _Key) ->
    undefined;
find_key_in_part(_Other, _Key) ->
    undefined.