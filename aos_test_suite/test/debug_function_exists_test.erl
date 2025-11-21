-module(debug_function_exists_test).
-include_lib("eunit/include/eunit.hrl").

function_exists_test() ->
    %% Check if verbose mode is enabled via environment variable or command line args
    Verbose = case os:getenv("EUNIT_VERBOSE") of
        "true" -> true;
        _ ->
            case init:get_argument(verbose) of
                {ok, _} -> true;
                _ -> false
            end
    end,
    function_exists_test(Verbose).

function_exists_test(Verbose) ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),

    %% Check if compute function exists
    case luerl:do("return type(compute)", LuaState) of
        {ok, [Type], _NewState} ->
            case Verbose of
                true -> ?debugFmt("compute type: ~p", [Type]);
                _ -> ok
            end,
            ?assertEqual(<<"function">>, Type);
        {error, Error, _NewState} ->
            case Verbose of
                true -> ?debugFmt("Error checking compute type: ~p", [Error]);
                _ -> ok
            end,
            ?assert(false)
    end.
    
global_functions_test() ->
    %% Check if verbose mode is enabled via environment variable or command line args
    Verbose = case os:getenv("EUNIT_VERBOSE") of
        "true" -> true;
        _ ->
            case init:get_argument(verbose) of
                {ok, _} -> true;
                _ -> false
            end
    end,
    global_functions_test(Verbose).

global_functions_test(Verbose) ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),

    %% List all global functions
    LuaCode = "local funcs = {}
    for k,v in pairs(_G) do
        if type(v) == 'function' then
            table.insert(funcs, k)
        end
    end
    table.sort(funcs)
    return table.concat(funcs, ', ')",

    case luerl:do(LuaCode, LuaState) of
        {ok, [Functions], _NewState} ->
            case Verbose of
                true -> ?debugFmt("Available functions: ~s", [Functions]);
                _ -> ok
            end;
        {error, Error, _NewState} ->
            case Verbose of
                true -> ?debugFmt("Error listing functions: ~p", [Error]);
                _ -> ok
            end
    end.