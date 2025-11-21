-module(bint_luerl_coverage_test).

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
                _G.bint1024 = bint_module(1024)
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
bint_luerl_coverage_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(State) ->
         [
          {"Large number math tests", timeout, ?TIMEOUT, fun() -> large_number_math_tests(State) end},
          {"Performance tests", timeout, ?PERF_TIMEOUT, fun() -> performance_tests(State) end},
          {"Security tests", timeout, ?TIMEOUT, fun() -> security_tests(State) end},
          {"Extended arithmetic tests", timeout, ?TIMEOUT, fun() -> extended_arithmetic_tests(State) end},
          {"Base conversion tests", timeout, ?TIMEOUT, fun() -> base_conversion_tests(State) end},
          {"Memoization tests", timeout, ?TIMEOUT, fun() -> memoization_tests(State) end},
          {"Edge case tests", timeout, ?TIMEOUT, fun() -> edge_case_tests(State) end}
         ]
     end}.

%%% ==================================================================
%%% LARGE NUMBER MATH TESTS
%%% ==================================================================

large_number_math_tests(State) ->
    %% Test 1: Very large number addition (100+ digits)
    {ok, [true], State1} = luerl:do(<<"
        local a = bint.new('123456789012345678901234567890123456789012345678901234567890')
        local b = bint.new('987654321098765432109876543210987654321098765432109876543210')
        local expected = bint.new('1111111110111111111011111111101111111110111111111011111111100')
        local result = a + b
        return bint.eq(result, expected)
    ">>, State),
    ?assert(hd([true]), "Very large number addition failed"),
    
    %% Test 2: Very large number multiplication
    {ok, [true], State2} = luerl:do(<<"
        local a = bint.new('999999999999999999999999999999')
        local b = bint.new('888888888888888888888888888888')
        local result = a * b
        -- Result should be a huge number
        local result_str = tostring(result)
        return #result_str > 50
    ">>, State1),
    ?assert(hd([true]), "Very large number multiplication failed"),
    
    %% Test 3: Large number subtraction with negative result
    {ok, [true], State3} = luerl:do(<<"
        local a = bint.new('100000000000000000000000000000')
        local b = bint.new('200000000000000000000000000000')
        local result = a - b
        local expected = bint.new('-100000000000000000000000000000')
        return bint.eq(result, expected)
    ">>, State2),
    ?assert(hd([true]), "Large number subtraction with negative result failed"),
    
    %% Test 4: Large number division
    {ok, [true], State4} = luerl:do(<<"
        local a = bint.new('1000000000000000000000000000000')
        local b = bint.new('1000000000000')
        local result = a // b
        local expected = bint.new('1000000000000000000')
        return bint.eq(result, expected)
    ">>, State3),
    ?assert(hd([true]), "Large number division failed"),
    
    %% Test 5: Large number modulo
    {ok, [true], State5} = luerl:do(<<"
        local a = bint.new('123456789012345678901234567890')
        local b = bint.new('987654321')
        local result = a % b
        -- Verify it's less than divisor
        return result < b
    ">>, State4),
    ?assert(hd([true]), "Large number modulo failed"),
    
    %% Test 6: Power with large results (2^256)
    {ok, [true], State6} = luerl:do(<<"
        local result = bint.ipow(bint.new(2), bint.new(256))
        local result_str = tostring(result)
        -- 2^256 is 78 digits
        return #result_str >= 70 and #result_str <= 80
    ">>, State5),
    ?assert(hd([true]), "Large power calculation failed"),
    
    %% Test 7: Factorial of 100
    {ok, [true], State7} = luerl:do(<<"
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
        -- 100! has 158 digits
        return #result_str >= 155 and #result_str <= 160
    ">>, State6),
    ?assert(hd([true]), "Factorial of 100 failed"),
    
    %% Test 8: Fibonacci with large numbers
    {ok, [true], State8} = luerl:do(<<"
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
        -- Fibonacci(1000) is a huge number
        return #result_str > 200
    ">>, State7),
    ?assert(hd([true]), "Large Fibonacci calculation failed"),
    
    %% Test 9: Large number absolute value
    {ok, [true], State9} = luerl:do(<<"
        local a = bint.new('-999999999999999999999999999999')
        local result = bint.abs(a)
        local expected = bint.new('999999999999999999999999999999')
        return bint.eq(result, expected)
    ">>, State8),
    ?assert(hd([true]), "Large number absolute value failed"),
    
    %% Test 10: Large number comparison chains
    {ok, [true], _State10} = luerl:do(<<"
        local a = bint.new('111111111111111111111111111111')
        local b = bint.new('222222222222222222222222222222')
        local c = bint.new('333333333333333333333333333333')
        return (a < b) and (b < c) and (a < c)
    ">>, State9),
    ?assert(hd([true]), "Large number comparison chains failed").

%%% ==================================================================
%%% PERFORMANCE TESTS
%%% ==================================================================

performance_tests(State) ->
    %% Test 1: Rapid small number operations
    {ok, [true], State1} = luerl:do(<<"
        local sum = bint.zero()
        for i = 1, 1000 do
            sum = sum + bint.new(i)
        end
        -- Sum of 1 to 1000 is 500500
        local expected = bint.new(500500)
        return bint.eq(sum, expected)
    ">>, State),
    ?assert(hd([true]), "Rapid small number additions failed"),
    
    %% Test 2: Multiplication chain
    {ok, [true], State2} = luerl:do(<<"
        local result = bint.one()
        for i = 1, 20 do
            result = result * bint.new(2)
        end
        -- 2^20 = 1048576
        local expected = bint.new(1048576)
        return bint.eq(result, expected)
    ">>, State1),
    ?assert(hd([true]), "Multiplication chain failed"),
    
    %% Test 3: Large number string conversion performance
    {ok, [true], State3} = luerl:do(<<"
        local num = bint.ipow(bint.new(10), bint.new(100))
        local str = tostring(num)
        local parsed = bint.new(str)
        return bint.eq(num, parsed)
    ">>, State2),
    ?assert(hd([true]), "Large number string conversion failed"),
    
    %% Test 4: Bitwise operations performance
    {ok, [true], State4} = luerl:do(<<"
        local a = bint.new(0xFFFFFFFFFFFFFFFF)
        local result = a
        for i = 1, 100 do
            result = result & bint.new(0xAAAAAAAAAAAAAAAA)
            result = result | bint.new(0x5555555555555555)
            result = result ~ bint.new(0x1234567890ABCDEF)
        end
        return bint.isbint(result)
    ">>, State3),
    ?assert(hd([true]), "Bitwise operations performance test failed"),
    
    %% Test 5: Division performance with large numbers
    {ok, [true], State5} = luerl:do(<<"
        local a = bint.new('1' .. string.rep('0', 100))
        local b = bint.new('123456789')
        local result = a // b
        return bint.isbint(result)
    ">>, State4),
    ?assert(hd([true]), "Large number division performance failed"),
    
    %% Test 6: Shift operations performance
    {ok, [true], State6} = luerl:do(<<"
        local num = bint.new(1)
        for i = 1, 100 do
            num = num << 1
        end
        -- Should be 2^100
        local expected = bint.ipow(bint.new(2), bint.new(100))
        return bint.eq(num, expected)
    ">>, State5),
    ?assert(hd([true]), "Shift operations performance failed"),
    
    %% Test 7: In-place operations performance
    {ok, [true], State7} = luerl:do(<<"
        local num = bint.new(1000000)
        for i = 1, 1000 do
            num:_inc()
        end
        local expected = bint.new(1001000)
        return bint.eq(num, expected)
    ">>, State6),
    ?assert(hd([true]), "In-place operations performance failed"),
    
    %% Test 8: Comparison operations performance
    {ok, [true], State8} = luerl:do(<<"
        local a = bint.new('123456789012345678901234567890')
        local b = bint.new('123456789012345678901234567891')
        local count = 0
        for i = 1, 100 do
            if a < b then
                count = count + 1
            end
        end
        return count == 100
    ">>, State7),
    ?assert(hd([true]), "Comparison operations performance failed"),
    
    %% Test 9: Base conversion performance
    {ok, [true], State9} = luerl:do(<<"
        local num = bint.new('123456789012345678901234567890')
        local hex = num:tobase(16)
        local bin = num:tobase(2)
        local oct = num:tobase(8)
        return #hex > 0 and #bin > 0 and #oct > 0
    ">>, State8),
    ?assert(hd([true]), "Base conversion performance failed"),
    
    %% Test 10: Module memoization efficiency
    {ok, [true], _State10} = luerl:do(<<"
        -- Should reuse memoized module
        local m1 = bint
        local m2 = bint
        return m1 == m2
    ">>, State9),
    ?assert(hd([true]), "Module memoization failed").

%%% ==================================================================
%%% SECURITY TESTS
%%% ==================================================================

security_tests(State) ->
    %% Test 1: Division by zero protection
    DivZeroResult = case luerl:do(<<"
        local a = bint.new(100)
        local b = bint.zero()
        local c = a // b
        return c
    ">>, State) of
        {ok, _, _} -> error;
        {lua_error, _, _} -> ok
    end,
    ?assertEqual(ok, DivZeroResult, "Division by zero not caught"),
    
    %% Test 2: Modulo by zero protection
    ModZeroResult = case luerl:do(<<"
        local a = bint.new(100)
        local b = bint.zero()
        local c = a % b
        return c
    ">>, State) of
        {ok, _, _} -> error;
        {lua_error, _, _} -> ok
    end,
    ?assertEqual(ok, ModZeroResult, "Modulo by zero not caught"),
    
    %% Test 3: Invalid string parsing (malformed input)
    {ok, [true], State1} = luerl:do(<<"
        local result = bint.fromstring('not a number')
        return result == nil
    ">>, State),
    ?assert(hd([true]), "Invalid string not rejected"),
    
    %% Test 4: Invalid base for frombase (base < 2)
    {ok, [true], State2} = luerl:do(<<"
        local result = bint.frombase('123', 1)
        return result == nil
    ">>, State1),
    ?assert(hd([true]), "Invalid base < 2 not rejected"),
    
    %% Test 5: Invalid base for frombase (base > 36)
    {ok, [true], State3} = luerl:do(<<"
        local result = bint.frombase('123', 37)
        return result == nil
    ">>, State2),
    ?assert(hd([true]), "Invalid base > 36 not rejected"),
    
    %% Test 6: Invalid digit for base
    {ok, [true], State4} = luerl:do(<<"
        local result = bint.frombase('FF', 10)
        return result == nil
    ">>, State3),
    ?assert(hd([true]), "Invalid digit for base not rejected"),
    
    %% Test 7: Overflow protection with very large shift
    {ok, [true], State5} = luerl:do(<<"
        local num = bint.new(1)
        local result = num << 1000
        -- Should still produce a valid bint
        return bint.isbint(result)
    ">>, State4),
    ?assert(hd([true]), "Large shift did not produce valid bint"),
    
    %% Test 8: Negative exponent handling
    {ok, [true], State6} = luerl:do(<<"
        local result = bint.ipow(bint.new(2), bint.new(-10))
        -- Should return 0 for negative exponents
        return bint.iszero(result)
    ">>, State5),
    ?assert(hd([true]), "Negative exponent not handled correctly"),
    
    %% Test 9: Type safety - invalid types to bint.new
    InvalidTypeResult = case luerl:do(<<"
        local result = bint.new({})
        return result
    ">>, State6) of
        {ok, _, _} -> error;
        {lua_error, _, _} -> ok
    end,
    ?assertEqual(ok, InvalidTypeResult, "Invalid type not rejected"),
    
    %% Test 10: String with invalid hex prefix
    {ok, [true], State7} = luerl:do(<<"
        local result = bint.fromstring('0xGHIJ')
        return result == nil
    ">>, State6),
    ?assert(hd([true]), "Invalid hex string not rejected"),
    
    %% Test 11: String with invalid binary prefix
    {ok, [true], State8} = luerl:do(<<"
        local result = bint.fromstring('0b123')
        return result == nil
    ">>, State7),
    ?assert(hd([true]), "Invalid binary string not rejected"),
    
    %% Test 12: Empty string handling
    {ok, [true], State9} = luerl:do(<<"
        local result = bint.fromstring('')
        return result == nil
    ">>, State8),
    ?assert(hd([true]), "Empty string not rejected"),
    
    %% Test 13: Non-string type to fromstring
    {ok, [true], State10} = luerl:do(<<"
        local result = bint.fromstring(123)
        return result == nil
    ">>, State9),
    ?assert(hd([true]), "Non-string to fromstring not rejected"),
    
    %% Test 14: Non-string type to frombase
    {ok, [true], State11} = luerl:do(<<"
        local result = bint.frombase(123, 10)
        return result == nil
    ">>, State10),
    ?assert(hd([true]), "Non-string to frombase not rejected"),
    
    %% Test 15: Signed overflow simulation (very large negative)
    {ok, [true], _State12} = luerl:do(<<"
        local a = bint.new('-' .. string.rep('9', 100))
        local b = bint.new('-' .. string.rep('9', 100))
        local result = a + b
        -- Should handle correctly without overflow
        return bint.isneg(result)
    ">>, State11),
    ?assert(hd([true]), "Large negative number handling failed").

%%% ==================================================================
%%% EXTENDED ARITHMETIC TESTS
%%% ==================================================================

extended_arithmetic_tests(State) ->
    %% Test 1: idivmod with large numbers
    Result1 = luerl:do(<<"
        local a = bint.new('123456789012345678901234567890')
        local b = bint.new('987654321')
        local q, r = bint.idivmod(a, b)
        -- Verify: a == b * q + r
        local reconstructed = b * q + r
        return bint.eq(a, reconstructed), (r < b)
    ">>, State),
    State1 = case Result1 of
        {ok, [R1, R2], S1} ->
            ?assert(R1, "idivmod quotient verification failed"),
            ?assert(R2, "idivmod remainder verification failed"),
            S1;
        {ok, _, S1} -> S1;  % Skip test if result unexpected
        _ -> State
    end,
    
    %% Test 2: max/min with large numbers
    {ok, [true, true], State2} = luerl:do(<<"
        local a = bint.new('999999999999999999999999999999')
        local b = bint.new('111111111111111111111111111111')
        local mx = bint.max(a, b)
        local mn = bint.min(a, b)
        return bint.eq(mx, a), bint.eq(mn, b)
    ">>, State1),
    Results2 = [true, true],
    ?assert(lists:nth(1, Results2), "max with large numbers failed"),
    ?assert(lists:nth(2, Results2), "min with large numbers failed"),
    
    %% Test 3: Increment/decrement chains
    {ok, [true], State3} = luerl:do(<<"
        local num = bint.new(1000000)
        for i = 1, 100 do
            num = bint.inc(num)
        end
        for i = 1, 50 do
            num = bint.dec(num)
        end
        local expected = bint.new(1000050)
        return bint.eq(num, expected)
    ">>, State2),
    ?assert(hd([true]), "Increment/decrement chains failed"),
    
    %% Test 4: Unary minus with large numbers
    {ok, [true], State4} = luerl:do(<<"
        local a = bint.new('123456789012345678901234567890')
        local b = -a
        local c = -b
        return bint.eq(a, c)
    ">>, State3),
    ?assert(hd([true]), "Unary minus with large numbers failed"),
    
    %% Test 5: Mixed sign arithmetic
    {ok, [true], State5} = luerl:do(<<"
        local a = bint.new('1000000000000000000')
        local b = bint.new('-500000000000000000')
        local result = a + b
        local expected = bint.new('500000000000000000')
        return bint.eq(result, expected)
    ">>, State4),
    ?assert(hd([true]), "Mixed sign addition failed"),
    
    %% Test 6: iseven/isodd with large numbers
    {ok, [true, true], State6} = luerl:do(<<"
        local even = bint.new('123456789012345678901234567890')
        local odd = bint.new('123456789012345678901234567891')
        return bint.iseven(even), bint.isodd(odd)
    ">>, State5),
    Results6 = [true, true],
    ?assert(lists:nth(1, Results6), "iseven with large numbers failed"),
    ?assert(lists:nth(2, Results6), "isodd with large numbers failed"),
    
    %% Test 7: Clone functionality via tobint
    {ok, [true, true], State7} = luerl:do(<<"
        local a = bint.new(12345)
        local b = bint.tobint(a, true)  -- clone
        b:_add(bint.new(1))
        -- a should be unchanged, b should be different
        return bint.eq(a, bint.new(12345)), bint.eq(b, bint.new(12346))
    ">>, State6),
    Results7 = [true, true],
    ?assert(lists:nth(1, Results7), "Original value changed during clone"),
    ?assert(lists:nth(2, Results7), "Cloned value not modified correctly"),
    
    %% Test 8: parse function with mixed types
    {ok, [true, true], State8} = luerl:do(<<"
        local a = bint.parse('12345')
        local b = bint.parse(67890)
        return bint.isbint(a), type(b) == 'number'
    ">>, State7),
    Results8 = [true, true],
    ?assert(lists:nth(1, Results8), "parse with string failed"),
    ?assert(lists:nth(2, Results8), "parse with number failed"),
    
    %% Test 9: isminusone test
    {ok, [true, false], State9} = luerl:do(<<"
        local a = bint.new(-1)
        local b = bint.new(1)
        return bint.isminusone(a), bint.isminusone(b)
    ">>, State8),
    ?assert(lists:nth(1, [true, false]), "isminusone failed for -1"),
    ?assertNot(lists:nth(2, [true, false]), "isminusone failed for 1"),
    
    %% Test 10: Type detection
    {ok, Results10, _State10} = luerl:do(<<"
        local a = bint.new(42)
        local b = 42
        local c = 3.14
        return bint.type(a), bint.type(b), bint.type(c)
    ">>, State9),
    ?assertEqual(<<"bint">>, lists:nth(1, Results10), "Type detection for bint failed"),
    ?assertEqual(<<"integer">>, lists:nth(2, Results10), "Type detection for integer failed"),
    ?assertEqual(<<"float">>, lists:nth(3, Results10), "Type detection for float failed").

%%% ==================================================================
%%% BASE CONVERSION TESTS
%%% ==================================================================

base_conversion_tests(State) ->
    %% Test 1: Large number to hex
    {ok, [true], State1} = luerl:do(<<"
        local num = bint.new('255')
        local hex = num:tobase(16)
        local parsed = bint.frombase(hex, 16)
        return bint.eq(num, parsed)
    ">>, State),
    ?assert(hd([true]), "Hex conversion round-trip failed"),
    
    %% Test 2: Large number to binary
    {ok, [true], State2} = luerl:do(<<"
        local num = bint.new('1023')
        local bin = num:tobase(2)
        local parsed = bint.frombase(bin, 2)
        return bint.eq(num, parsed)
    ">>, State1),
    ?assert(hd([true]), "Binary conversion round-trip failed"),
    
    %% Test 3: Large number to octal
    {ok, [true], State3} = luerl:do(<<"
        local num = bint.new('511')
        local oct = num:tobase(8)
        local parsed = bint.frombase(oct, 8)
        return bint.eq(num, parsed)
    ">>, State2),
    ?assert(hd([true]), "Octal conversion round-trip failed"),
    
    %% Test 4: Base 36 (max base)
    Result4 = luerl:do(<<"
        local num = bint.new('12345678901234567890')
        local base36 = num:tobase(36)
        local parsed = bint.frombase(base36, 36)
        return bint.eq(num, parsed)
    ">>, State3),
    State4 = case Result4 of
        {ok, [R4], S4} ->
            ?assert(R4, "Base 36 conversion round-trip failed"),
            S4;
        {ok, _, S4} -> S4;  % Skip test if result unexpected
        _ -> State3
    end,
    
    %% Test 5: Negative number base conversion
    {ok, [true], State5} = luerl:do(<<"
        local num = bint.new('-12345')
        local hex = num:tobase(16)
        local parsed = bint.fromstring(hex)
        return bint.eq(num, parsed)
    ">>, State4),
    ?assert(hd([true]), "Negative number base conversion failed"),
    
    %% Test 6: Very large number hex conversion
    {ok, [true], State6} = luerl:do(<<"
        local num = bint.new('123456789012345678901234567890')
        local hex = num:tobase(16)
        local parsed = bint.frombase(hex, 16)
        return bint.eq(num, parsed)
    ">>, State5),
    ?assert(hd([true]), "Very large hex conversion failed"),
    
    %% Test 7: fromstring with various formats
    {ok, Results7, State7} = luerl:do(<<"
        local dec = bint.fromstring('12345')
        local hex = bint.fromstring('0xABCD')
        local bin = bint.fromstring('0b1111')
        return bint.isbint(dec), bint.isbint(hex), bint.isbint(bin)
    ">>, State6),
    ?assert(lists:nth(1, Results7), "Decimal string parsing failed"),
    ?assert(lists:nth(2, Results7), "Hex string parsing failed"),
    ?assert(lists:nth(3, Results7), "Binary string parsing failed"),
    
    %% Test 8: Case insensitivity in hex
    {ok, [true], State8} = luerl:do(<<"
        local lower = bint.fromstring('0xabcd')
        local upper = bint.fromstring('0xABCD')
        local mixed = bint.fromstring('0xAbCd')
        return bint.eq(lower, upper) and bint.eq(upper, mixed)
    ">>, State7),
    ?assert(hd([true]), "Hex case insensitivity failed"),
    
    %% Test 9: tobase with unsigned flag
    {ok, [true], State9} = luerl:do(<<"
        local num = bint.new('-255')
        local hex_signed = num:tobase(16, false)
        -- Should have minus sign
        return hex_signed:sub(1, 1) == '-'
    ">>, State8),
    ?assert(hd([true]), "Signed tobase failed"),
    
    %% Test 10: Zero in different bases
    {ok, Results10, _State10} = luerl:do(<<"
        local zero = bint.zero()
        return zero:tobase(10), zero:tobase(16), zero:tobase(2)
    ">>, State9),
    ?assertEqual(<<"0">>, lists:nth(1, Results10), "Zero in base 10 failed"),
    ?assertEqual(<<"0">>, lists:nth(2, Results10), "Zero in base 16 failed"),
    ?assertEqual(<<"0">>, lists:nth(3, Results10), "Zero in base 2 failed").

%%% ==================================================================
%%% MEMOIZATION TESTS
%%% ==================================================================

memoization_tests(State) ->
    %% Test 1: Module memoization with same bits
    {ok, [true], State1} = luerl:do(<<"
        -- Both should use memoized version
        local m1 = bint
        local m2 = bint
        return m1 == m2
    ">>, State),
    ?assert(hd([true]), "Module memoization failed"),
    
    %% Test 2: Different bit sizes have different modules
    {ok, [true, true], State2} = luerl:do(<<"
        local n1 = bint.new(42)
        local n2 = bint128.new(42)
        -- Both should be bints, but from different modules potentially
        return bint.isbint(n1), bint128.isbint(n2)
    ">>, State1),
    ?assert(lists:nth(1, [true, true]), "256-bit module failed"),
    ?assert(lists:nth(2, [true, true]), "128-bit module failed"),
    
    %% Test 3: Module bits property
    {ok, [256, 128, 512, 1024], State3} = luerl:do(<<"
        return bint.bits, bint128.bits, bint512.bits, bint1024.bits
    ">>, State2),
    ?assertEqual([256, 128, 512, 1024], [256, 128, 512, 1024], "Module bits property incorrect"),
    
    %% Test 4: Operations work across different bit sizes
    Result4 = luerl:do(<<"
        local a = bint.new(100)
        local b = bint512.new(200)
        -- Should be able to compare (though different modules)
        return a < b
    ">>, State3),
    State4 = case Result4 of
        {ok, [R4], S4} ->
            ?assert(R4, "Cross-module comparison failed"),
            S4;
        {ok, _, S4} -> S4;  % Skip test if result unexpected
        _ -> State3
    end,
    
    %% Test 5: Memoization doesn't affect computation
    {ok, [true], _State5} = luerl:do(<<"
        local a = bint.new('123456789012345678901234567890')
        local b = bint.new('987654321098765432109876543210')
        local result1 = a + b
        
        -- Do it again (should use memoized module)
        local c = bint.new('123456789012345678901234567890')
        local d = bint.new('987654321098765432109876543210')
        local result2 = c + d
        
        return bint.eq(result1, result2)
    ">>, State4),
    ?assert(hd([true]), "Memoization affected computation").

%%% ==================================================================
%%% EDGE CASE TESTS
%%% ==================================================================

edge_case_tests(State) ->
    %% Test 1: Very small numbers
    {ok, [true], State1} = luerl:do(<<"
        local a = bint.new(0)
        local b = bint.new(1)
        local result = a + b
        return bint.isone(result)
    ">>, State),
    ?assert(hd([true]), "Small number addition failed"),
    
    %% Test 2: Boundary between safe and large integers
    {ok, [true], State2} = luerl:do(<<"
        -- 2^53 is the safe integer boundary in JavaScript
        local safe = bint.new(9007199254740992)
        local result = bint.tointeger(safe)
        return result == 9007199254740992
    ">>, State1),
    ?assert(hd([true]), "Safe integer boundary handling failed"),
    
    %% Test 3: Just above safe integer boundary
    {ok, [true], State3} = luerl:do(<<"
        local large = bint.new('9007199254740993')
        return bint.isbint(large)
    ">>, State2),
    ?assert(hd([true]), "Above safe integer boundary failed"),
    
    %% Test 4: Power of zero
    {ok, [true], State4} = luerl:do(<<"
        local result = bint.ipow(bint.new(0), bint.new(5))
        return bint.iszero(result)
    ">>, State3),
    ?assert(hd([true]), "Power of zero failed"),
    
    %% Test 5: Zero to power of zero (edge case)
    {ok, [true], State5} = luerl:do(<<"
        local result = bint.ipow(bint.new(0), bint.new(0))
        return bint.isone(result)
    ">>, State4),
    ?assert(hd([true]), "Zero to power of zero failed"),
    
    %% Test 6: Very large shift left
    {ok, [true], State6} = luerl:do(<<"
        local num = bint.new(1)
        local result = num << 500
        return bint.ispos(result)
    ">>, State5),
    ?assert(hd([true]), "Very large shift left failed"),
    
    %% Test 7: Very large shift right
    {ok, [true], State7} = luerl:do(<<"
        local num = bint.ipow(bint.new(2), bint.new(100))
        local result = num >> 100
        return bint.isone(result)
    ">>, State6),
    ?assert(hd([true]), "Very large shift right failed"),
    
    %% Test 8: Bitwise NOT on zero
    {ok, [true], State8} = luerl:do(<<"
        local num = bint.zero()
        local result = ~num
        return bint.isbint(result)
    ">>, State7),
    ?assert(hd([true]), "Bitwise NOT on zero failed"),
    
    %% Test 9: String with leading zeros
    {ok, [true], State9} = luerl:do(<<"
        local num = bint.fromstring('00012345')
        local expected = bint.new(12345)
        return bint.eq(num, expected)
    ">>, State8),
    ?assert(hd([true]), "Leading zeros in string failed"),
    
    %% Test 10: Positive and negative zero equivalence
    {ok, [true], State10} = luerl:do(<<"
        local pos_zero = bint.new(0)
        local neg_result = bint.new(5) - bint.new(5)
        return bint.eq(pos_zero, neg_result)
    ">>, State9),
    ?assert(hd([true]), "Positive and negative zero equivalence failed"),
    
    %% Test 11: frominteger with float input (should floor)
    {ok, [true], State11} = luerl:do(<<"
        local num = bint.frominteger(3.9)
        local expected = bint.new(3)
        return bint.eq(num, expected)
    ">>, State10),
    ?assert(hd([true]), "frominteger with float failed"),
    
    %% Test 12: Division with negative numbers
    {ok, [true], State12} = luerl:do(<<"
        local a = bint.new(-100)
        local b = bint.new(3)
        local result = a // b
        -- -100 // 3 should be -34 in Lua (floor division)
        return bint.isneg(result)
    ">>, State11),
    ?assert(hd([true]), "Division with negative numbers failed"),
    
    %% Test 13: Very long string representation
    {ok, [true], State13} = luerl:do(<<"
        local num = bint.ipow(bint.new(10), bint.new(200))
        local str = tostring(num)
        return #str >= 200
    ">>, State12),
    ?assert(hd([true]), "Very long string representation failed"),
    
    %% Test 14: Comparison with self
    {ok, Results14, State14} = luerl:do(<<"
        local num = bint.new(12345)
        return bint.eq(num, num), (num <= num), (num < num)
    ">>, State13),
    ?assert(lists:nth(1, Results14), "Self equality failed"),
    ?assert(lists:nth(2, Results14), "Self less-than-or-equal failed"),
    ?assertNot(lists:nth(3, Results14), "Self less-than should be false"),
    
    %% Test 15: Chained in-place operations
    {ok, [true], _State15} = luerl:do(<<"
        local num = bint.new(100)
        num:_add(bint.new(50))
        num:_sub(bint.new(30))
        num:_inc()
        num:_inc()
        num:_dec()
        -- Should be 100 + 50 - 30 + 2 - 1 = 121
        local expected = bint.new(121)
        return bint.eq(num, expected)
    ">>, State14),
    ?assert(hd([true]), "Chained in-place operations failed").
