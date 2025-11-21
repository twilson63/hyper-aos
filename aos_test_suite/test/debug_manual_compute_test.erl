-module(debug_manual_compute_test).
-include_lib("eunit/include/eunit.hrl").

manual_compute_test() ->
    %% Check if verbose mode is enabled via environment variable or command line args
    Verbose = case os:getenv("EUNIT_VERBOSE") of
        "true" -> true;
        _ ->
            case init:get_argument(verbose) of
                {ok, _} -> true;
                _ -> false
            end
    end,
    manual_compute_test(Verbose).

manual_compute_test(Verbose) ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),

    %% Try very simple compute call
    LuaCode1 = "return compute({}, {})",
    case luerl:do(LuaCode1, LuaState) of
        {ok, Result1, LuaState2} ->
            case Verbose of
                true -> ?debugFmt("Simple compute succeeded: ~p", [Result1]);
                _ -> ok
            end;
        {error, Error1, LuaState2} ->
            case Verbose of
                true -> ?debugFmt("Simple compute failed: ~p", [Error1]);
                _ -> ok
            end
    end,

    %% Try with process initialization message
    LuaCode2 = "
    local assignment = {
        ['process-id'] = 'test-process',
        timestamp = 1234567890,
        ['block-height'] = 1000,
        owner = 'test-owner',
        body = {
            id = 'process-init',
            type = 'process',
            commitments = {
                ['init-key'] = {
                    type = 'rsa-pss-512',
                    committer = 'TestOwner123456789012345678901234567890123'
                }
            }
        }
    }
    return compute({}, assignment)",

    case luerl:do(LuaCode2, LuaState2) of
        {ok, Result2, _LuaState3} ->
            case Verbose of
                true -> ?debugFmt("Process init compute succeeded: ~p", [Result2]);
                _ -> ok
            end;
        {error, Error2, _LuaState3} ->
            case Verbose of
                true -> ?debugFmt("Process init compute failed: ~p", [Error2]);
                _ -> ok
            end
    end.