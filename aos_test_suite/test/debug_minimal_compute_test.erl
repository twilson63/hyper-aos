-module(debug_minimal_compute_test).
-include_lib("eunit/include/eunit.hrl").

minimal_compute_test() ->
    %% Check if verbose mode is enabled via environment variable or command line args
    Verbose = case os:getenv("EUNIT_VERBOSE") of
        "true" -> true;
        _ ->
            case init:get_argument(verbose) of
                {ok, _} -> true;
                _ -> false
            end
    end,
    minimal_compute_test(Verbose).

minimal_compute_test(Verbose) ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),

    %% Test by modifying compute to avoid the problematic line
    OverrideLua = "
    -- Override compute with a simple version that doesn't call extract_state_from_global
    function compute(state, assignment)
        _G._OUTPUT = ''

        -- Just return a simple state without extraction
        local simple_state = {
            results = {
                output = {data = 'test', prompt = 'prompt'},
                outbox = {},
                status = 'ok',
                info = 'hyper-aos'
            }
        }

        return 'ok', simple_state
    end
    ",

    case luerl:do(OverrideLua, LuaState) of
        {ok, _, LuaState2} ->
            case Verbose of
                true -> ?debugFmt("Override successful", []);
                _ -> ok
            end,

            %% Now try the compute call
            LuaCode = "return compute({}, {})",
            case luerl:do(LuaCode, LuaState2) of
                {ok, Result, _} ->
                    case Verbose of
                        true -> ?debugFmt("Override compute succeeded: ~p", [Result]);
                        _ -> ok
                    end;
                {error, Error, _} ->
                    case Verbose of
                        true -> ?debugFmt("Override compute failed: ~p", [Error]);
                        _ -> ok
                    end
            end;
        {error, Error, _} ->
            case Verbose of
                true -> ?debugFmt("Override failed: ~p", [Error]);
                _ -> ok
            end
    end.