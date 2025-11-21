-module(debug_extract_state_test).
-include_lib("eunit/include/eunit.hrl").

extract_state_test() ->
    %% Check if verbose mode is enabled via environment variable or command line args
    Verbose = case os:getenv("EUNIT_VERBOSE") of
        "true" -> true;
        _ ->
            case init:get_argument(verbose) of
                {ok, _} -> true;
                _ -> false
            end
    end,
    extract_state_test(Verbose).

extract_state_test(Verbose) ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),

    %% Test extract_state_from_global function directly
    LuaCode = "
    -- Test if extract_state_from_global exists and what type it is
    if extract_state_from_global then
        return 'function exists'
    else
        return 'function missing'
    end
    ",

    case luerl:do(LuaCode, LuaState) of
        {ok, [Result1], LuaState2} ->
            case Verbose of
                true -> ?debugFmt("extract_state_from_global check: ~p", [Result1]);
                _ -> ok
            end;
        {error, Error1, LuaState2} ->
            case Verbose of
                true -> ?debugFmt("Error checking extract_state_from_global: ~p", [Error1]);
                _ -> ok
            end
    end,

    %% Test if we can call it manually
    LuaCode2 = "
    -- First make sure basic functions exist
    local checks = {}
    checks.pairs_type = type(pairs)
    checks.next_type = type(next)
    checks.type_type = type(type)

    -- Try to call extract_state_from_global with empty visited table
    local success, result = pcall(extract_state_from_global, {})
    checks.extract_success = success
    if not success then
        checks.extract_error = result
    else
        checks.extract_result_type = type(result)
    end

    return checks
    ",

    case luerl:do(LuaCode2, LuaState2) of
        {ok, [ChecksTable], _LuaState3} ->
            case Verbose of
                true -> ?debugFmt("Function checks: ~p", [ChecksTable]);
                _ -> ok
            end;
        {error, Error2, _LuaState3} ->
            case Verbose of
                true -> ?debugFmt("Error in function checks: ~p", [Error2]);
                _ -> ok
            end
    end.