-module(bint_luerl_benchmark_simple).

-export([run/0]).

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

%% Main benchmark
run() ->
    %% Initialize LUERL with bint_luerl
    State = luerl:init(),
    BintPath = filename:join([code:lib_dir(aos_test_suite), "..", "..", "..", "..", "..", "src", "bint_luerl.lua"]),
    {ok, BintSrc} = file:read_file(BintPath),
    
    InitCode = <<"
        local bint_loader = function()
            ", BintSrc/binary, "
        end
        local bint_module = bint_loader()
        _G.bint = bint_module(256)
        
        -- Create test data
        small_a = bint.new(123456789)
        small_b = bint.new(987654321)
        
        medium_a = bint.new('123456789012345678901234567890')
        medium_b = bint.new('987654321098765432109876543210')
        
        large_a = bint.new('1' .. string.rep('0', 50))
        large_b = bint.new('9' .. string.rep('9', 50))
        
        return 'initialized'
    ">>,
    {_, State1} = luerl:do(InitCode, State),
    
    io:format("~n" ++ string:copies("=", 71) ++ "~n"),
    io:format("BINT_LUERL BENCHMARK (Optimized LUERL Implementation - Simple)~n"),
    io:format(string:copies("=", 71) ++ "~n"),
    
    %% Creation Operations
    io:format("~n-- Creation Operations --~n"),
    benchmark("Create from small integer", fun() ->
        luerl:do(<<"local n = bint.new(42)">>, State1)
    end, 1000),
    
    benchmark("Create from string (30 digits)", fun() ->
        luerl:do(<<"local n = bint.new('123456789012345678901234567890')">>, State1)
    end, 500),
    
    %% Addition Operations
    io:format("~n-- Addition Operations --~n"),
    benchmark("Add small numbers", fun() ->
        luerl:do(<<"local c = small_a + small_b">>, State1)
    end, 1000),
    
    benchmark("Add medium numbers (30 digits)", fun() ->
        luerl:do(<<"local c = medium_a + medium_b">>, State1)
    end, 500),
    
    benchmark("Add large numbers (50 digits)", fun() ->
        luerl:do(<<"local c = large_a + large_b">>, State1)
    end, 200),
    
    %% Subtraction Operations
    io:format("~n-- Subtraction Operations --~n"),
    benchmark("Subtract small numbers", fun() ->
        luerl:do(<<"local c = small_b - small_a">>, State1)
    end, 1000),
    
    benchmark("Subtract medium numbers (30 digits)", fun() ->
        luerl:do(<<"local c = medium_b - medium_a">>, State1)
    end, 500),
    
    %% Multiplication Operations
    io:format("~n-- Multiplication Operations --~n"),
    benchmark("Multiply small numbers", fun() ->
        luerl:do(<<"local c = small_a * small_b">>, State1)
    end, 500),
    
    benchmark("Multiply medium numbers (30 digits)", fun() ->
        luerl:do(<<"local c = medium_a * medium_b">>, State1)
    end, 100),
    
    %% String Conversion
    io:format("~n-- String Conversion --~n"),
    benchmark("Convert small to string", fun() ->
        luerl:do(<<"local s = tostring(small_a)">>, State1)
    end, 1000),
    
    benchmark("Convert medium to string", fun() ->
        luerl:do(<<"local s = tostring(medium_a)">>, State1)
    end, 500),
    
    %% Mixed Operations (Factorial)
    io:format("~n-- Mixed Operations (Factorial) --~n"),
    benchmark("Factorial(20)", fun() ->
        luerl:do(<<"
            local result = bint.one()
            for i = 2, 20 do
                result = result * bint.new(i)
            end
        ">>, State1)
    end, 100),
    
    io:format("~n" ++ string:copies("=", 71) ++ "~n"),
    io:format("Note: Reduced iterations for faster benchmarking~n"),
    io:format("LUERL overhead includes interpreter and state management~n"),
    ok.