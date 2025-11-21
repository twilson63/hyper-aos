-module(bint_luerl_benchmark_test).

-export([bint_luerl_benchmark_test/0]).

%% Benchmark runner
benchmark(Name, Fun, Iterations) ->
    StartTime = erlang:monotonic_time(microsecond),
    run_iterations(Fun, Iterations),
    EndTime = erlang:monotonic_time(microsecond),
    TotalTime = EndTime - StartTime,
    AvgTime = TotalTime / Iterations,
    io:format("~-40s: ~8.2f µs/op (~p ops in ~.3fs)~n", 
              [Name, AvgTime, Iterations, TotalTime / 1000000]).

run_iterations(_, 0) -> ok;
run_iterations(Fun, N) ->
    Fun(),
    run_iterations(Fun, N - 1).

%% Main benchmark test - simplified version
bint_luerl_benchmark_test() ->
    %% Initialize LUERL with bint_luerl
    State = luerl:init(),
    BintPath = "../src/bint_luerl.lua",
    {ok, BintSrc} = file:read_file(BintPath),

    %% Simple test to verify bint_luerl loads and basic operations work
    TestCode = <<"
        local bint_loader = function()
            ", BintSrc/binary, "
        end
        local bint_module = bint_loader()
        _G.bint = bint_module(256)

        -- Test basic operations
        local a = bint.new(42)
        local b = bint.new(24)
        local c = a + b
        local d = a * b
        local result = tostring(c) .. ',' .. tostring(d)

        return result
    ">>,

    case luerl:do(TestCode, State) of
        {ok, [Result], _} when is_binary(Result) ->
            %% Verify we got expected result
            Expected = <<"66,1008">>,
            case Result of
                Expected ->
                    io:format("BINT LUERL test passed: ~s~n", [Result]),
                    ok;
                _ ->
                    io:format("BINT LUERL test failed: expected ~s, got ~s~n", [Expected, Result]),
                    {error, {unexpected_result, Result}}
            end;
        {error, Error, _} ->
            io:format("BINT LUERL test error: ~p~n", [Error]),
            {error, Error};
        Other ->
            io:format("BINT LUERL test unexpected result: ~p~n", [Other]),
            {error, {unexpected_format, Other}}
    end.