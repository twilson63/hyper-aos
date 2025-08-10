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

%% Main benchmark test
bint_luerl_benchmark_test() ->
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
        
        huge_a = bint.new('1' .. string.rep('0', 100))
        huge_b = bint.new('9' .. string.rep('9', 100))
        
        return 'initialized'
    ">>,
    {_, State1} = luerl:do(InitCode, State),
    
    io:format("~n" ++ string:copies("=", 71) ++ "~n"),
    io:format("BINT_LUERL BENCHMARK (Optimized LUERL Implementation)~n"),
    io:format(string:copies("=", 71) ++ "~n"),
    
    %% Creation Operations
    io:format("~n-- Creation Operations --~n"),
    benchmark("Create from small integer", fun() ->
        luerl:do(<<"local n = bint.new(42)">>, State1)
    end, 50000),
    
    benchmark("Create from string (30 digits)", fun() ->
        luerl:do(<<"local n = bint.new('123456789012345678901234567890')">>, State1)
    end, 10000),
    
    benchmark("Create from string (100 digits)", fun() ->
        luerl:do(<<"local n = bint.new('1' .. string.rep('2', 99))">>, State1)
    end, 5000),
    
    %% Addition Operations
    io:format("~n-- Addition Operations --~n"),
    benchmark("Add small numbers", fun() ->
        luerl:do(<<"local c = small_a + small_b">>, State1)
    end, 50000),
    
    benchmark("Add medium numbers (30 digits)", fun() ->
        luerl:do(<<"local c = medium_a + medium_b">>, State1)
    end, 20000),
    
    benchmark("Add large numbers (50 digits)", fun() ->
        luerl:do(<<"local c = large_a + large_b">>, State1)
    end, 10000),
    
    benchmark("Add huge numbers (100 digits)", fun() ->
        luerl:do(<<"local c = huge_a + huge_b">>, State1)
    end, 5000),
    
    %% Subtraction Operations
    io:format("~n-- Subtraction Operations --~n"),
    benchmark("Subtract small numbers", fun() ->
        luerl:do(<<"local c = small_b - small_a">>, State1)
    end, 50000),
    
    benchmark("Subtract medium numbers (30 digits)", fun() ->
        luerl:do(<<"local c = medium_b - medium_a">>, State1)
    end, 20000),
    
    benchmark("Subtract large numbers (50 digits)", fun() ->
        luerl:do(<<"local c = large_b - large_a">>, State1)
    end, 10000),
    
    %% Multiplication Operations
    io:format("~n-- Multiplication Operations --~n"),
    benchmark("Multiply small numbers", fun() ->
        luerl:do(<<"local c = small_a * small_b">>, State1)
    end, 30000),
    
    benchmark("Multiply medium numbers (30 digits)", fun() ->
        luerl:do(<<"local c = medium_a * medium_b">>, State1)
    end, 5000),
    
    benchmark("Multiply large numbers (50 digits)", fun() ->
        luerl:do(<<"local c = large_a * large_b">>, State1)
    end, 1000),
    
    benchmark("Multiply huge numbers (100 digits)", fun() ->
        luerl:do(<<"local c = huge_a * huge_b">>, State1)
    end, 500),
    
    %% Division Operations
    io:format("~n-- Division Operations --~n"),
    benchmark("Divide small numbers", fun() ->
        luerl:do(<<"local c = small_b // small_a">>, State1)
    end, 30000),
    
    benchmark("Divide medium numbers (30 digits)", fun() ->
        luerl:do(<<"local c = medium_b // medium_a">>, State1)
    end, 5000),
    
    benchmark("Divide large numbers (50 digits)", fun() ->
        luerl:do(<<"local c = large_b // large_a">>, State1)
    end, 1000),
    
    %% Comparison Operations
    io:format("~n-- Comparison Operations --~n"),
    benchmark("Compare small numbers", fun() ->
        luerl:do(<<"local result = small_a < small_b">>, State1)
    end, 100000),
    
    benchmark("Compare medium numbers (30 digits)", fun() ->
        luerl:do(<<"local result = medium_a < medium_b">>, State1)
    end, 50000),
    
    benchmark("Compare large numbers (50 digits)", fun() ->
        luerl:do(<<"local result = large_a < large_b">>, State1)
    end, 30000),
    
    %% Bitwise Operations
    io:format("~n-- Bitwise Operations --~n"),
    benchmark("Bitwise AND small numbers", fun() ->
        luerl:do(<<"local c = small_a & small_b">>, State1)
    end, 50000),
    
    benchmark("Bitwise OR medium numbers", fun() ->
        luerl:do(<<"local c = medium_a | medium_b">>, State1)
    end, 20000),
    
    benchmark("Bitwise XOR large numbers", fun() ->
        luerl:do(<<"local c = large_a ~ large_b">>, State1)
    end, 10000),
    
    benchmark("Left shift small number", fun() ->
        luerl:do(<<"local c = small_a << 10">>, State1)
    end, 50000),
    
    benchmark("Right shift medium number", fun() ->
        luerl:do(<<"local c = medium_a >> 10">>, State1)
    end, 20000),
    
    %% Power Operations
    io:format("~n-- Power Operations --~n"),
    benchmark("Power small (2^100)", fun() ->
        luerl:do(<<"local c = bint.ipow(bint.new(2), bint.new(100))">>, State1)
    end, 5000),
    
    benchmark("Power medium (10^50)", fun() ->
        luerl:do(<<"local c = bint.ipow(bint.new(10), bint.new(50))">>, State1)
    end, 2000),
    
    %% String Conversion
    io:format("~n-- String Conversion --~n"),
    benchmark("Convert small to string", fun() ->
        luerl:do(<<"local s = tostring(small_a)">>, State1)
    end, 50000),
    
    benchmark("Convert medium to string", fun() ->
        luerl:do(<<"local s = tostring(medium_a)">>, State1)
    end, 20000),
    
    benchmark("Convert large to string", fun() ->
        luerl:do(<<"local s = tostring(large_a)">>, State1)
    end, 10000),
    
    benchmark("Convert to hex (medium number)", fun() ->
        luerl:do(<<"local s = medium_a:tobase(16)">>, State1)
    end, 20000),
    
    %% Mixed Operations (Factorial)
    io:format("~n-- Mixed Operations (Factorial) --~n"),
    benchmark("Factorial(20)", fun() ->
        luerl:do(<<"
            local result = bint.one()
            for i = 2, 20 do
                result = result * bint.new(i)
            end
        ">>, State1)
    end, 5000),
    
    benchmark("Factorial(50)", fun() ->
        luerl:do(<<"
            local result = bint.one()
            for i = 2, 50 do
                result = result * bint.new(i)
            end
        ">>, State1)
    end, 1000),
    
    io:format("~n" ++ string:copies("=", 71) ++ "~n"),
    io:format("Benchmark completed successfully!~n"),
    ok.