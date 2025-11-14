-module(bint_luerl_comprehensive_test).

-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 30000).
-define(PERF_TIMEOUT, 60000).

%% Test fixture setup and teardown
setup() ->
    State = luerl:init(),
    
    %% Load optimized bint module
    BintPath = case file:read_file("../../src/bint_luerl.lua") of
        {ok, Content} -> {ok, Content};
        {error, _} -> file:read_file("../src/bint_luerl.lua")
    end,
    
    case BintPath of
        {ok, BintSrc} ->
            %% Execute bint module and make it available
            Code = <<"
                local bint_loader = function()
                    ", BintSrc/binary, "
                end
                local bint_module = bint_loader()
                -- Create bint instances with different bit sizes
                _G.bint = bint_module(256)
                _G.bint128 = bint_module(128)
                _G.bint512 = bint_module(512)
                return 'bint loaded'
            ">>,
            State1 = case luerl:do(Code, State) of
                {ok, _, NewState1} -> 
                    NewState1;
                {error, Reason1} ->
                    error({failed_to_load_module, bint, Reason1})
            end,
            State1;
        {error, Reason} ->
            error({failed_to_read_bint_file, Reason})
    end.

teardown(_State) ->
    erlang:garbage_collect(),
    ok.

%% Main test suite
bint_luerl_comprehensive_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(State) ->
         [
          %% Large Number Math Tests
          {"Very large addition (100+ digits)", timeout, ?TIMEOUT, fun() -> test_very_large_addition(State) end},
          {"Very large multiplication", timeout, ?TIMEOUT, fun() -> test_very_large_multiplication(State) end},
          {"Large number division", timeout, ?TIMEOUT, fun() -> test_large_division(State) end},
          {"Large number modulo", timeout, ?TIMEOUT, fun() -> test_large_modulo(State) end},
          {"Power of large numbers (2^256)", timeout, ?TIMEOUT, fun() -> test_large_power(State) end},
          {"Factorial of 100", timeout, ?TIMEOUT, fun() -> test_factorial_100(State) end},
          {"Fibonacci 1000", timeout, ?TIMEOUT, fun() -> test_fibonacci_1000(State) end},
          {"Large negative numbers", timeout, ?TIMEOUT, fun() -> test_large_negative(State) end},
          
          %% Performance Tests
          {"Rapid small operations (1000 additions)", timeout, ?PERF_TIMEOUT, fun() -> test_rapid_operations(State) end},
          {"Multiplication chain", timeout, ?PERF_TIMEOUT, fun() -> test_multiplication_chain(State) end},
          {"String conversion performance", timeout, ?PERF_TIMEOUT, fun() -> test_string_conversion_perf(State) end},
          {"Bitwise operations performance", timeout, ?PERF_TIMEOUT, fun() -> test_bitwise_perf(State) end},
          {"Shift operations performance", timeout, ?PERF_TIMEOUT, fun() -> test_shift_perf(State) end},
          
          %% Security Tests
          {"Division by zero protection", timeout, ?TIMEOUT, fun() -> test_division_by_zero(State) end},
          {"Modulo by zero protection", timeout, ?TIMEOUT, fun() -> test_modulo_by_zero(State) end},
          {"Invalid string parsing", timeout, ?TIMEOUT, fun() -> test_invalid_string(State) end},
          {"Invalid base rejection (< 2)", timeout, ?TIMEOUT, fun() -> test_invalid_base_low(State) end},
          {"Invalid base rejection (> 36)", timeout, ?TIMEOUT, fun() -> test_invalid_base_high(State) end},
          {"Invalid digit for base", timeout, ?TIMEOUT, fun() -> test_invalid_digit(State) end},
          {"Negative exponent handling", timeout, ?TIMEOUT, fun() -> test_negative_exponent(State) end},
          {"Type safety for invalid inputs", timeout, ?TIMEOUT, fun() -> test_type_safety(State) end},
          {"Empty string handling", timeout, ?TIMEOUT, fun() -> test_empty_string(State) end},
          {"Overflow safety with large shifts", timeout, ?TIMEOUT, fun() -> test_large_shift_safety(State) end}
         ]
     end}.

%%% ==================================================================
%%% LARGE NUMBER MATH TESTS
%%% ==================================================================

test_very_large_addition(State) ->
    {ok, [true], _} = luerl:do(<<"
        local a = bint.new('123456789012345678901234567890123456789012345678901234567890')
        local b = bint.new('987654321098765432109876543210987654321098765432109876543210')
        local expected = bint.new('1111111110111111111011111111101111111110111111111011111111100')
        local result = a + b
        return bint.eq(result, expected)
    ">>, State),
    ok.

test_very_large_multiplication(State) ->
    {ok, [true], _} = luerl:do(<<"
        local a = bint.new('999999999999999999999999999999')
        local b = bint.new('888888888888888888888888888888')
        local result = a * b
        local result_str = tostring(result)
        return #result_str > 50
    ">>, State),
    ok.

test_large_division(State) ->
    {ok, [true], _} = luerl:do(<<"
        local a = bint.new('1000000000000000000000000000000')
        local b = bint.new('1000000000000')
        local result = a // b
        local expected = bint.new('1000000000000000000')
        return bint.eq(result, expected)
    ">>, State),
    ok.

test_large_modulo(State) ->
    {ok, [true], _} = luerl:do(<<"
        local a = bint.new('123456789012345678901234567890')
        local b = bint.new('987654321')
        local result = a % b
        return result < b
    ">>, State),
    ok.

test_large_power(State) ->
    {ok, [true], _} = luerl:do(<<"
        local result = bint.ipow(bint.new(2), bint.new(256))
        local result_str = tostring(result)
        return #result_str >= 70 and #result_str <= 80
    ">>, State),
    ok.

test_factorial_100(State) ->
    {ok, [true], _} = luerl:do(<<"
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
        
        local result = factorial(100)
        local result_str = tostring(result)
        return #result_str >= 155 and #result_str <= 160
    ">>, State),
    ok.

test_fibonacci_1000(State) ->
    {ok, [true], _} = luerl:do(<<"
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
        
        local result = fibonacci(1000)
        local result_str = tostring(result)
        return #result_str > 200
    ">>, State),
    ok.

test_large_negative(State) ->
    {ok, [true], _} = luerl:do(<<"
        local a = bint.new('-999999999999999999999999999999')
        local result = bint.abs(a)
        local expected = bint.new('999999999999999999999999999999')
        return bint.eq(result, expected)
    ">>, State),
    ok.

%%% ==================================================================
%%% PERFORMANCE TESTS
%%% ==================================================================

test_rapid_operations(State) ->
    {ok, [true], _} = luerl:do(<<"
        local sum = bint.zero()
        for i = 1, 1000 do
            sum = sum + bint.new(i)
        end
        local expected = bint.new(500500)
        return bint.eq(sum, expected)
    ">>, State),
    ok.

test_multiplication_chain(State) ->
    {ok, [true], _} = luerl:do(<<"
        local result = bint.one()
        for i = 1, 20 do
            result = result * bint.new(2)
        end
        local expected = bint.new(1048576)
        return bint.eq(result, expected)
    ">>, State),
    ok.

test_string_conversion_perf(State) ->
    {ok, [true], _} = luerl:do(<<"
        local num = bint.ipow(bint.new(10), bint.new(100))
        local str = tostring(num)
        local parsed = bint.new(str)
        return bint.eq(num, parsed)
    ">>, State),
    ok.

test_bitwise_perf(State) ->
    {ok, [true], _} = luerl:do(<<"
        local a = bint.new(0xFFFFFFFFFFFFFFFF)
        local result = a
        for i = 1, 100 do
            result = result & bint.new(0xAAAAAAAAAAAAAAAA)
            result = result | bint.new(0x5555555555555555)
            result = result ~ bint.new(0x1234567890ABCDEF)
        end
        return bint.isbint(result)
    ">>, State),
    ok.

test_shift_perf(State) ->
    {ok, [true], _} = luerl:do(<<"
        local num = bint.new(1)
        for i = 1, 100 do
            num = num << 1
        end
        local expected = bint.ipow(bint.new(2), bint.new(100))
        return bint.eq(num, expected)
    ">>, State),
    ok.

%%% ==================================================================
%%% SECURITY TESTS
%%% ==================================================================

test_division_by_zero(State) ->
    case luerl:do(<<"
        local a = bint.new(100)
        local b = bint.zero()
        local c = a // b
        return c
    ">>, State) of
        {ok, _, _} -> ?assert(false, "Division by zero should have failed");
        {lua_error, _, _} -> ok
    end.

test_modulo_by_zero(State) ->
    case luerl:do(<<"
        local a = bint.new(100)
        local b = bint.zero()
        local c = a % b
        return c
    ">>, State) of
        {ok, _, _} -> ?assert(false, "Modulo by zero should have failed");
        {lua_error, _, _} -> ok
    end.

test_invalid_string(State) ->
    {ok, [true], _} = luerl:do(<<"
        local result = bint.fromstring('not a number')
        return result == nil
    ">>, State),
    ok.

test_invalid_base_low(State) ->
    {ok, [true], _} = luerl:do(<<"
        local result = bint.frombase('123', 1)
        return result == nil
    ">>, State),
    ok.

test_invalid_base_high(State) ->
    {ok, [true], _} = luerl:do(<<"
        local result = bint.frombase('123', 37)
        return result == nil
    ">>, State),
    ok.

test_invalid_digit(State) ->
    {ok, [true], _} = luerl:do(<<"
        local result = bint.frombase('FF', 10)
        return result == nil
    ">>, State),
    ok.

test_negative_exponent(State) ->
    {ok, [true], _} = luerl:do(<<"
        local result = bint.ipow(bint.new(2), bint.new(-10))
        return bint.iszero(result)
    ">>, State),
    ok.

test_type_safety(State) ->
    case luerl:do(<<"
        local result = bint.new({})
        return result
    ">>, State) of
        {ok, _, _} -> ?assert(false, "Invalid type should have failed");
        {lua_error, _, _} -> ok
    end.

test_empty_string(State) ->
    {ok, [true], _} = luerl:do(<<"
        local result = bint.fromstring('')
        return result == nil
    ">>, State),
    ok.

test_large_shift_safety(State) ->
    {ok, [true], _} = luerl:do(<<"
        local num = bint.new(1)
        local result = num << 1000
        return bint.isbint(result)
    ">>, State),
    ok.
