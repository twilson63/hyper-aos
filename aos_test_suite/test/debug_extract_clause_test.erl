-module(debug_extract_clause_test).
-include_lib("eunit/include/eunit.hrl").

extract_clause_test() ->
    %% Check if verbose mode is enabled via environment variable or command line args
    Verbose = case os:getenv("EUNIT_VERBOSE") of
        "true" -> true;
        _ ->
            case init:get_argument(verbose) of
                {ok, _} -> true;
                _ -> false
            end
    end,
    extract_clause_test(Verbose).

extract_clause_test(Verbose) ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),

    %% Initialize process
    LuaState2 = aos_test_helpers:initialize_process(LuaState, State),

    %% Test simple math computation
    Assignment = aos_test_helpers:create_eval_assignment(<<"return 1 + 1">>),
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),

    case Verbose of
        true -> ?debugFmt("Full result structure: ~p", [Result]);
        _ -> ok
    end,

    %% Check the pattern matching manually
    case Result of
        {[Status, StateTable], {luerl, _LuerlState}} when is_tuple(StateTable) ->
            case Verbose of
                true -> ?debugFmt("Matched Luerl clause: Status=~p, StateTable=~p", [Status, StateTable]);
                _ -> ok
            end;
        {[Status, StateTable], _LuaState} when is_list(StateTable) ->
            case Verbose of
                true -> ?debugFmt("Matched list clause: Status=~p", [Status]);
                _ -> ok
            end;
        {error, Error, _} ->
            case Verbose of
                true -> ?debugFmt("Matched error clause: ~p", [Error]);
                _ -> ok
            end;
        Other when is_binary(Other) ->
            case Verbose of
                true -> ?debugFmt("Matched binary clause: ~p", [Other]);
                _ -> ok
            end;
        Other ->
            case Verbose of
                true -> ?debugFmt("No pattern matched: ~p", [Other]);
                _ -> ok
            end
    end,

    Output = aos_test_helpers:extract_output_data(Result),
    case Verbose of
        true -> ?debugFmt("Extracted output: ~p", [Output]);
        _ -> ok
    end.