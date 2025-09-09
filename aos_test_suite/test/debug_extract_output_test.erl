-module(debug_extract_output_test).
-include_lib("eunit/include/eunit.hrl").

extract_output_test() ->
    %% Check if verbose mode is enabled via environment variable or command line args
    Verbose = case os:getenv("EUNIT_VERBOSE") of
        "true" -> true;
        _ ->
            case init:get_argument(verbose) of
                {ok, _} -> true;
                _ -> false
            end
    end,
    extract_output_test(Verbose).

extract_output_test(Verbose) ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),

    %% Initialize process
    LuaState2 = aos_test_helpers:initialize_process(LuaState, State),

    %% Test simple math computation
    Assignment = aos_test_helpers:create_eval_assignment(<<"return 1 + 1">>),
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),

    case Verbose of
        true -> ?debugFmt("Raw result from call_compute: ~p", [Result]);
        _ -> ok
    end,

    case Verbose of
        true -> ?debugFmt("About to call extract_output_data with: ~p", [Result]);
        _ -> ok
    end,
    Output = aos_test_helpers:extract_output_data(Result),
    case Verbose of
        true -> ?debugFmt("Extracted output: ~p", [Output]);
        _ -> ok
    end,

    %% Verify it's the expected result
    ?assertEqual(<<"2">>, Output).

extract_message_test() ->
    %% Check if verbose mode is enabled via environment variable or command line args
    Verbose = case os:getenv("EUNIT_VERBOSE") of
        "true" -> true;
        _ ->
            case init:get_argument(verbose) of
                {ok, _} -> true;
                _ -> false
            end
    end,
    extract_message_test(Verbose).

extract_message_test(Verbose) ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),

    %% Initialize process
    LuaState2 = aos_test_helpers:initialize_process(LuaState, State),

    %% Test basic message (no action)
    Message = #{
        <<"id">> => <<"test-msg">>,
        <<"data">> => <<"Hello">>
    },
    Assignment = aos_test_helpers:create_assignment(Message),
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),

    Output = aos_test_helpers:extract_output_data(Result),
    case Verbose of
        true -> ?debugFmt("Message output: ~p", [Output]);
        _ -> ok
    end,

    %% Should contain "Hello" and "unknown" (for the from address)
    ?assert(binary:match(Output, <<"Hello">>) =/= nomatch),
    ?assert(binary:match(Output, <<"unknown">>) =/= nomatch).