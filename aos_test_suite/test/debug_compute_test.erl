-module(debug_compute_test).
-include_lib("eunit/include/eunit.hrl").

compute_test_disabled() ->
    %% Check if verbose mode is enabled via environment variable or command line args
    Verbose = case os:getenv("EUNIT_VERBOSE") of
        "true" -> true;
        _ ->
            case init:get_argument(verbose) of
                {ok, _} -> true;
                _ -> false
            end
    end,
    compute_test_disabled(Verbose).

compute_test_disabled(Verbose) ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),

    %% Try to initialize process
    try
        LuaState2 = aos_test_helpers:initialize_process(LuaState, State),
        case Verbose of
            true -> ?debugFmt("Process initialized successfully", []);
            _ -> ok
        end,

        %% Try simple compute call
        Assignment = aos_test_helpers:create_eval_assignment(<<"return 1 + 1">>),
        Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
        case Verbose of
            true -> ?debugFmt("Compute result: ~p", [Result]);
            _ -> ok
        end,

        Output = aos_test_helpers:extract_output_data(Result),
        case Verbose of
            true -> ?debugFmt("Extracted output: ~p", [Output]);
            _ -> ok
        end
    catch
        Error:Reason:Stack ->
            case Verbose of
                true -> ?debugFmt("Error during compute test: ~p:~p~n~p", [Error, Reason, Stack]);
                _ -> ok
            end,
            ?assert(false)
    end.
