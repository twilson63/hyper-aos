-module(bint_luerl_extreme_limits_test).

-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 60000).

%% Test fixture setup
setup() ->
    State = luerl:init(),
    
    BintPath = case file:read_file("../../src/bint_luerl.lua") of
        {ok, Content} -> {ok, Content};
        {error, _} -> file:read_file("../src/bint_luerl.lua")
    end,
    
    case BintPath of
        {ok, BintSrc} ->
            Code = <<"
                local bint_loader = function()
                    ", BintSrc/binary, "
                end
                local bint_module = bint_loader()
                _G.bint = bint_module(256)
                _G.bint1024 = bint_module(1024)
                _G.bint4096 = bint_module(4096)
                _G.bint_unlimited = bint_module(999999)
                return 'bint loaded'
            ">>,
            State1 = case luerl:do(Code, State) of
                {ok, _, NewState1} -> NewState1;
                {error, Reason1} -> error({failed_to_load_module, bint, Reason1})
            end,
            State1;
        {error, Reason} ->
            error({failed_to_read_bint_file, Reason})
    end.

teardown(_State) ->
    erlang:garbage_collect(),
    ok.

%% Test suite
bint_luerl_extreme_limits_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(State) ->
         [
          {"Test 1000-digit numbers", timeout, ?TIMEOUT, fun() -> test_1000_digit_numbers(State) end},
          {"Test 10000-digit numbers", timeout, ?TIMEOUT, fun() -> test_10000_digit_numbers(State) end},
          {"Test 2^1024 (RSA key size)", timeout, ?TIMEOUT, fun() -> test_2_power_1024(State) end},
          {"Test 2^2048 (RSA-2048)", timeout, ?TIMEOUT, fun() -> test_2_power_2048(State) end},
          {"Test 2^4096 (RSA-4096)", timeout, ?TIMEOUT, fun() -> test_2_power_4096(State) end},
          {"Test factorial of 500", timeout, ?TIMEOUT, fun() -> test_factorial_500(State) end},
          {"Test factorial of 1000", timeout, ?TIMEOUT, fun() -> test_factorial_1000(State) end},
          {"Test Fibonacci 10000", timeout, ?TIMEOUT, fun() -> test_fibonacci_10000(State) end},
          {"Test very large multiplication", timeout, ?TIMEOUT, fun() -> test_large_multiplication(State) end},
          {"Test googol operations (10^100)", timeout, ?TIMEOUT, fun() -> test_googol(State) end},
          {"Test googolplex digits (10^(10^100) - partial)", timeout, ?TIMEOUT, fun() -> test_googolplex_partial(State) end}
         ]
     end}.

%%% ==================================================================
%%% EXTREME LIMIT TESTS
%%% ==================================================================

test_1000_digit_numbers(State) ->
    {ok, [NumDigits], _} = luerl:do(<<"
        -- Create ~1000-digit number using 2^3321 (approximately 1000 digits)
        local result = bint.ipow(bint.new(2), bint.new(3321))
        local result_str = tostring(result)
        return #result_str
    ">>, State),
    io:format("✅ Successfully handled 1000-digit numbers: ~p digits~n", [NumDigits]),
    ?assert(NumDigits >= 1000),
    ok.

test_10000_digit_numbers(State) ->
    {ok, [NumDigits], _} = luerl:do(<<"
        -- Create ~10000-digit number using 2^33219 (approximately 10000 digits)
        local result = bint.ipow(bint.new(2), bint.new(33219))
        local result_str = tostring(result)
        return #result_str
    ">>, State),
    io:format("✅ Successfully handled 10,000-digit numbers: ~p digits~n", [NumDigits]),
    ?assert(NumDigits >= 10000),
    ok.

test_2_power_1024(State) ->
    {ok, [NumDigits], _} = luerl:do(<<"
        -- 2^1024 (RSA-1024 size)
        local result = bint1024.ipow(bint1024.new(2), bint1024.new(1024))
        local result_str = tostring(result)
        return #result_str
    ">>, State),
    io:format("✅ 2^1024 = ~p digits~n", [NumDigits]),
    ?assert(NumDigits > 300),
    ok.

test_2_power_2048(State) ->
    {ok, [NumDigits], _} = luerl:do(<<"
        -- 2^2048 (RSA-2048 size)
        local result = bint.ipow(bint.new(2), bint.new(2048))
        local result_str = tostring(result)
        return #result_str
    ">>, State),
    io:format("✅ 2^2048 = ~p digits~n", [NumDigits]),
    ?assert(NumDigits > 600),
    ok.

test_2_power_4096(State) ->
    {ok, [NumDigits], _} = luerl:do(<<"
        -- 2^4096 (RSA-4096 size)
        local result = bint4096.ipow(bint4096.new(2), bint4096.new(4096))
        local result_str = tostring(result)
        return #result_str
    ">>, State),
    io:format("✅ 2^4096 = ~p digits~n", [NumDigits]),
    ?assert(NumDigits > 1200),
    ok.

test_factorial_500(State) ->
    {ok, [NumDigits], _} = luerl:do(<<"
        local function factorial(n)
            if n <= 1 then
                return bint.one()
            end
            local result = bint.new(n)
            for i = n - 1, 2, -1 do
                result = result * bint.new(i)
            end
            return result
        end
        
        local result = factorial(500)
        local result_str = tostring(result)
        return #result_str
    ">>, State),
    io:format("✅ 500! = ~p digits~n", [NumDigits]),
    ?assert(NumDigits > 1000),
    ok.

test_factorial_1000(State) ->
    {ok, [NumDigits], _} = luerl:do(<<"
        local function factorial(n)
            if n <= 1 then
                return bint.one()
            end
            local result = bint.new(n)
            for i = n - 1, 2, -1 do
                result = result * bint.new(i)
            end
            return result
        end
        
        local result = factorial(1000)
        local result_str = tostring(result)
        return #result_str
    ">>, State),
    io:format("✅ 1000! = ~p digits~n", [NumDigits]),
    ?assert(NumDigits > 2500),
    ok.

test_fibonacci_10000(State) ->
    {ok, [NumDigits], _} = luerl:do(<<"
        local function fibonacci(n)
            if n <= 1 then
                return bint.new(n)
            end
            local a = bint.zero()
            local b = bint.one()
            for i = 2, n do
                local temp = a + b
                a = b
                b = temp
            end
            return b
        end
        
        local result = fibonacci(10000)
        local result_str = tostring(result)
        return #result_str
    ">>, State),
    io:format("✅ Fibonacci(10000) = ~p digits~n", [NumDigits]),
    ?assert(NumDigits > 2000),
    ok.

test_large_multiplication(State) ->
    {ok, [NumDigits], _} = luerl:do(<<"
        -- Multiply two large numbers (10^500 each)
        local a = bint.ipow(bint.new(10), bint.new(500))
        local b = bint.ipow(bint.new(10), bint.new(500))
        local result = a * b
        local result_str = tostring(result)
        return #result_str
    ">>, State),
    io:format("✅ 500-digit × 500-digit = ~p digits~n", [NumDigits]),
    ?assert(NumDigits >= 1000),
    ok.

test_googol(State) ->
    {ok, [NumDigits, true], _} = luerl:do(<<"
        -- Googol = 10^100
        local googol = bint.ipow(bint.new(10), bint.new(100))
        local result_str = tostring(googol)
        
        -- Googol squared
        local googol_squared = googol * googol
        local squared_str = tostring(googol_squared)
        
        return #result_str, #squared_str == 201
    ">>, State),
    io:format("✅ Googol (10^100) = ~p digits, Googol² = 201 digits~n", [NumDigits]),
    ok.

test_googolplex_partial(State) ->
    {ok, [NumDigits], _} = luerl:do(<<"
        -- We can't do 10^(10^100), but we can do 10^10000
        -- This is still an incredibly large number
        local result = bint.ipow(bint.new(10), bint.new(10000))
        local result_str = tostring(result)
        return #result_str
    ">>, State),
    io:format("✅ 10^10000 = ~p digits (approaching googolplex scale)~n", [NumDigits]),
    ?assertEqual(10001, NumDigits),
    ok.

