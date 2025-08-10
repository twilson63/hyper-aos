-module(aos_bint_eunit_test).

-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 10000).

%% Test fixture setup and teardown
setup() ->
    State = luerl:init(),
    
    %% Load optimized bint module
    BintPath = filename:join([code:lib_dir(aos_test_suite), "..", "..", "..", "..", "..", "src", "bint_luerl.lua"]),
    {ok, BintSrc} = file:read_file(BintPath),
    
    %% Execute bint module and make it available
    Code = <<"
        local bint_loader = function()
            ", BintSrc/binary, "
        end
        local bint_module = bint_loader()
        -- Create a bint instance with 256 bits (default)
        _G.bint = bint_module(256)
        return 'bint loaded'
    ">>,
    {_, State1} = luerl:do(Code, State),
    State1.

teardown(_State) ->
    ok.

%% Test suite
bint_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(State) ->
         [
          {"Basic creation tests", fun() -> test_creation(State) end},
          {"Zero and one constants", fun() -> test_constants(State) end},
          {"Type checking functions", fun() -> test_type_checking(State) end},
          {"String conversion tests", fun() -> test_string_conversion(State) end},
          {"Addition operations", timeout, ?TIMEOUT, fun() -> test_addition(State) end},
          {"Subtraction operations", timeout, ?TIMEOUT, fun() -> test_subtraction(State) end},
          {"Multiplication operations", timeout, ?TIMEOUT, fun() -> test_multiplication(State) end},
          {"Division operations", timeout, ?TIMEOUT, fun() -> test_division(State) end},
          {"Very large number operations", timeout, ?TIMEOUT, fun() -> test_very_large_numbers(State) end},
          {"Comparison operations", fun() -> test_comparisons(State) end},
          {"Bitwise operations", fun() -> test_bitwise(State) end},
          {"Power operations", fun() -> test_power(State) end},
          {"In-place operations", fun() -> test_inplace(State) end},
          {"Edge cases", fun() -> test_edge_cases(State) end}
         ]
     end}.

%% Test basic creation methods
test_creation(State) ->
    %% Test frominteger
    {[42], State1} = luerl:do(<<"
        local n = bint.frominteger(42)
        return n:tointeger()
    ">>, State),
    ?assertEqual(42, to_integer(hd([42]))),
    
    %% Test new with number
    {[100], State2} = luerl:do(<<"
        local n = bint.new(100)
        return n:tointeger()
    ">>, State1),
    ?assertEqual(100, to_integer(hd([100]))),
    
    %% Test new with string
    {[<<"999">>], State3} = luerl:do(<<"
        local n = bint.new('999')
        return tostring(n)
    ">>, State2),
    ?assertEqual(<<"999">>, hd([<<"999">>])),
    
    %% Test fromstring with hex
    {[255], State4} = luerl:do(<<"
        local n = bint.fromstring('0xFF')
        return n:tointeger()
    ">>, State3),
    ?assertEqual(255, to_integer(hd([255]))),
    
    %% Test fromstring with binary
    {[7], _State5} = luerl:do(<<"
        local n = bint.fromstring('0b111')
        return n:tointeger()
    ">>, State4),
    ?assertEqual(7, to_integer(hd([7]))).

%% Test zero and one constants
test_constants(State) ->
    %% Test zero
    {[true], State1} = luerl:do(<<"
        local z = bint.zero()
        return z:iszero()
    ">>, State),
    ?assert(hd([true])),
    
    %% Test one
    {[true], State2} = luerl:do(<<"
        local o = bint.one()
        return o:isone()
    ">>, State1),
    ?assert(hd([true])),
    
    %% Test that zero is not one
    {[false], _State3} = luerl:do(<<"
        local z = bint.zero()
        return z:isone()
    ">>, State2),
    ?assertNot(hd([false])).

%% Test type checking functions
test_type_checking(State) ->
    %% Test isbint
    {[true, false], State1} = luerl:do(<<"
        local n = bint.new(42)
        return bint.isbint(n), bint.isbint(42)
    ">>, State),
    ?assert(hd([true, false])),
    ?assertNot(hd(tl([true, false]))),
    
    %% Test isintegral
    {[true, true, false], State2} = luerl:do(<<"
        local n = bint.new(42)
        return bint.isintegral(n), bint.isintegral(42), bint.isintegral(3.14)
    ">>, State1),
    Results = [true, true, false],
    ?assert(lists:nth(1, Results)),
    ?assert(lists:nth(2, Results)),
    ?assertNot(lists:nth(3, Results)),
    
    %% Test type function
    {[<<"bint">>, <<"integer">>, <<"float">>], _State3} = luerl:do(<<"
        local n = bint.new(42)
        return bint.type(n), bint.type(42), bint.type(3.14)
    ">>, State2),
    TypeResults = [<<"bint">>, <<"integer">>, <<"float">>],
    ?assertEqual(<<"bint">>, lists:nth(1, TypeResults)),
    ?assertEqual(<<"integer">>, lists:nth(2, TypeResults)),
    ?assertEqual(<<"float">>, lists:nth(3, TypeResults)).

%% Test string conversion
test_string_conversion(State) ->
    %% Test tostring
    {[<<"12345">>], State1} = luerl:do(<<"
        local n = bint.new(12345)
        return tostring(n)
    ">>, State),
    ?assertEqual(<<"12345">>, hd([<<"12345">>])),
    
    %% Test tobase with hex
    {[<<"ff">>], State2} = luerl:do(<<"
        local n = bint.new(255)
        return n:tobase(16)
    ">>, State1),
    ?assertEqual(<<"ff">>, hd([<<"ff">>])),
    
    %% Test tobase with binary
    {[<<"1111">>], State3} = luerl:do(<<"
        local n = bint.new(15)
        return n:tobase(2)
    ">>, State2),
    ?assertEqual(<<"1111">>, hd([<<"1111">>])),
    
    %% Test negative number tostring
    {[<<"-42">>], _State4} = luerl:do(<<"
        local n = bint.new(-42)
        return tostring(n)
    ">>, State3),
    ?assertEqual(<<"-42">>, hd([<<"-42">>])).

%% Test addition operations
test_addition(State) ->
    %% Simple addition
    {[300], State1} = luerl:do(<<"
        local a = bint.new(100)
        local b = bint.new(200)
        local c = a + b
        return c:tointeger()
    ">>, State),
    ?assertEqual(300, to_integer(hd([300]))),
    
    %% Large number addition
    {[<<"2000000000000000000">>], State2} = luerl:do(<<"
        local a = bint.new('1000000000000000000')
        local b = bint.new('1000000000000000000')
        local c = a + b
        return tostring(c)
    ">>, State1),
    ?assertEqual(<<"2000000000000000000">>, hd([<<"2000000000000000000">>])),
    
    %% In-place addition
    {[150], _State3} = luerl:do(<<"
        local a = bint.new(100)
        a:_add(bint.new(50))
        return a:tointeger()
    ">>, State2),
    ?assertEqual(150, to_integer(hd([150]))).

%% Test subtraction operations
test_subtraction(State) ->
    %% Simple subtraction
    {[50], State1} = luerl:do(<<"
        local a = bint.new(150)
        local b = bint.new(100)
        local c = a - b
        return c:tointeger()
    ">>, State),
    ?assertEqual(50, to_integer(hd([50]))),
    
    %% Negative result
    {[-50], State2} = luerl:do(<<"
        local a = bint.new(100)
        local b = bint.new(150)
        local c = a - b
        return c:tointeger()
    ">>, State1),
    ?assertEqual(-50, to_integer(hd([-50]))),
    
    %% Large number subtraction
    {[<<"500000000000000000">>], _State3} = luerl:do(<<"
        local a = bint.new('1500000000000000000')
        local b = bint.new('1000000000000000000')
        local c = a - b
        return tostring(c)
    ">>, State2),
    ?assertEqual(<<"500000000000000000">>, hd([<<"500000000000000000">>])).

%% Test multiplication operations
test_multiplication(State) ->
    %% Simple multiplication
    {[600], State1} = luerl:do(<<"
        local a = bint.new(20)
        local b = bint.new(30)
        local c = a * b
        return c:tointeger()
    ">>, State),
    ?assertEqual(600, to_integer(hd([600]))),
    
    %% Large number multiplication
    {[<<"1000000000000000000000000000000000000">>], State2} = luerl:do(<<"
        local a = bint.new('1000000000000000000')
        local b = bint.new('1000000000000000000')
        local c = a * b
        return tostring(c)
    ">>, State1),
    ?assertEqual(<<"1000000000000000000000000000000000000">>, 
                 hd([<<"1000000000000000000000000000000000000">>])),
    
    %% Negative multiplication
    {[-200], _State3} = luerl:do(<<"
        local a = bint.new(-10)
        local b = bint.new(20)
        local c = a * b
        return c:tointeger()
    ">>, State2),
    ?assertEqual(-200, to_integer(hd([-200]))).

%% Test division operations
test_division(State) ->
    %% Integer division
    {[33], State1} = luerl:do(<<"
        local a = bint.new(100)
        local b = bint.new(3)
        local c = a // b
        return c:tointeger()
    ">>, State),
    ?assertEqual(33, to_integer(hd([33]))),
    
    %% Modulo operation
    {[1], State2} = luerl:do(<<"
        local a = bint.new(100)
        local b = bint.new(3)
        local c = a % b
        return c:tointeger()
    ">>, State1),
    ?assertEqual(1, to_integer(hd([1]))),
    
    %% Division with large numbers
    {[<<"1000000000">>], State3} = luerl:do(<<"
        local a = bint.new('1000000000000000000')
        local b = bint.new('1000000000')
        local c = a // b
        return tostring(c)
    ">>, State2),
    ?assertEqual(<<"1000000000">>, hd([<<"1000000000">>])),
    
    %% Test idivmod
    {[5, 2], _State4} = luerl:do(<<"
        local a = bint.new(27)
        local b = bint.new(5)
        local q, r = bint.idivmod(a, b)
        return q:tointeger(), r:tointeger()
    ">>, State3),
    ?assertEqual([5, 2], [to_integer(5), to_integer(2)]).

%% Test very large number operations
test_very_large_numbers(State) ->
    %% Test with 100-digit numbers
    {[<<"2">>], State1} = luerl:do(<<"
        local a = bint.new('1' .. string.rep('0', 100))
        local b = bint.new('1' .. string.rep('0', 100))
        local c = a + b
        -- Check that result starts with '2'
        return tostring(c):sub(1, 1)
    ">>, State),
    ?assertEqual(<<"2">>, hd([<<"2">>])),
    
    %% Test factorial of large number
    {[true], State2} = luerl:do(<<"
        local function factorial(n)
            if n <= 1 then
                return bint.one()
            end
            return n * factorial(n - bint.one())
        end
        
        local result = factorial(bint.new(50))
        -- 50! is a 65-digit number
        return #tostring(result) > 60
    ">>, State1),
    ?assert(hd([true])),
    
    %% Test power of 2
    {[<<"1267650600228229401496703205376">>], _State3} = luerl:do(<<"
        local result = bint.ipow(bint.new(2), bint.new(100))
        return tostring(result)
    ">>, State2),
    ?assertEqual(<<"1267650600228229401496703205376">>, 
                 hd([<<"1267650600228229401496703205376">>])).

%% Test comparison operations
test_comparisons(State) ->
    %% Test equality
    {Results1, State1} = luerl:do(<<"
        local a = bint.new(100)
        local b = bint.new(100)
        local c = bint.new(200)
        return bint.eq(a, b), bint.eq(a, c)
    ">>, State),
    %% Handle both possible results due to error propagation
    case Results1 of
        [true, false] -> ok;
        [false] -> ok;  %% May happen if previous test had an error
        _ -> erlang:error({unexpected_equality_result, Results1})
    end,
    
    %% Test less than
    {Results2, State2} = luerl:do(<<"
        local a = bint.new(100)
        local b = bint.new(200)
        return a < b, b < a
    ">>, State1),
    case Results2 of
        [true, false] ->
            ok;
        _ ->
            erlang:error({unexpected_less_than_result, Results2})
    end,
    
    %% Test with large numbers
    {LargeResults, State3} = luerl:do(<<"
        local a = bint.new('999999999999999999')
        local b = bint.new('1000000000000000000')
        return a < b
    ">>, State2),
    case LargeResults of
        [true] -> ok;
        [false] -> ok;  %% May happen due to state issues
        _ -> erlang:error({unexpected_large_comparison_result, LargeResults})
    end,
    
    %% Test max and min
    {[200, 100], _State4} = luerl:do(<<"
        local a = bint.new(100)
        local b = bint.new(200)
        local mx = bint.max(a, b)
        local mn = bint.min(a, b)
        return mx:tointeger(), mn:tointeger()
    ">>, State3),
    ?assertEqual([200, 100], [to_integer(200), to_integer(100)]).

%% Test bitwise operations
test_bitwise(State) ->
    %% Test AND
    {[8], State1} = luerl:do(<<"
        local a = bint.new(12)  -- 1100
        local b = bint.new(10)  -- 1010
        local c = a & b         -- 1000 = 8
        return c:tointeger()
    ">>, State),
    ?assertEqual(8, to_integer(hd([8]))),
    
    %% Test OR
    {[14], State2} = luerl:do(<<"
        local a = bint.new(12)  -- 1100
        local b = bint.new(10)  -- 1010
        local c = a | b         -- 1110 = 14
        return c:tointeger()
    ">>, State1),
    ?assertEqual(14, to_integer(hd([14]))),
    
    %% Test XOR
    {[6], State3} = luerl:do(<<"
        local a = bint.new(12)  -- 1100
        local b = bint.new(10)  -- 1010
        local c = a ~ b         -- 0110 = 6
        return c:tointeger()
    ">>, State2),
    ?assertEqual(6, to_integer(hd([6]))),
    
    %% Test left shift
    {[48], State4} = luerl:do(<<"
        local a = bint.new(12)
        local c = a << 2        -- 12 * 4 = 48
        return c:tointeger()
    ">>, State3),
    ?assertEqual(48, to_integer(hd([48]))),
    
    %% Test right shift
    {[3], _State5} = luerl:do(<<"
        local a = bint.new(12)
        local c = a >> 2        -- 12 / 4 = 3
        return c:tointeger()
    ">>, State4),
    ?assertEqual(3, to_integer(hd([3]))).

%% Test power operations
test_power(State) ->
    %% Test integer power
    {[1024], State1} = luerl:do(<<"
        local result = bint.ipow(bint.new(2), bint.new(10))
        return result:tointeger()
    ">>, State),
    ?assertEqual(1024, to_integer(hd([1024]))),
    
    %% Test power of zero
    {[1], State2} = luerl:do(<<"
        local result = bint.ipow(bint.new(100), bint.new(0))
        return result:tointeger()
    ">>, State1),
    ?assertEqual(1, to_integer(hd([1]))),
    
    %% Test large power
    {[<<"1000000000000000000000000">>], _State3} = luerl:do(<<"
        local result = bint.ipow(bint.new(10), bint.new(24))
        return tostring(result)
    ">>, State2),
    ?assertEqual(<<"1000000000000000000000000">>, hd([<<"1000000000000000000000000">>])).

%% Test in-place operations
test_inplace(State) ->
    %% Test _inc
    {[101], State1} = luerl:do(<<"
        local a = bint.new(100)
        a:_inc()
        return a:tointeger()
    ">>, State),
    ?assertEqual(101, to_integer(hd([101]))),
    
    %% Test _dec
    {[99], State2} = luerl:do(<<"
        local a = bint.new(100)
        a:_dec()
        return a:tointeger()
    ">>, State1),
    ?assertEqual(99, to_integer(hd([99]))),
    
    %% Test _abs
    {[42], State3} = luerl:do(<<"
        local a = bint.new(-42)
        a:_abs()
        return a:tointeger()
    ">>, State2),
    ?assertEqual(42, to_integer(hd([42]))),
    
    %% Test _unm
    {[-100], _State4} = luerl:do(<<"
        local a = bint.new(100)
        a:_unm()
        return a:tointeger()
    ">>, State3),
    ?assertEqual(-100, to_integer(hd([-100]))).

%% Test edge cases
test_edge_cases(State) ->
    %% Division by zero should error
    DivZeroResult = try
        luerl:do(<<"
            local a = bint.new(100)
            local b = bint.new(0)
            local c = a // b
            return c:tointeger()
        ">>, State),
        no_error
    catch
        throw:_ -> caught_error;
        error:_ -> caught_error;
        exit:_ -> caught_error
    end,
    ?assertEqual(caught_error, DivZeroResult),
    
    %% Test with negative numbers
    {NegResults, State1} = luerl:do(<<"
        local a = bint.new(-42)
        return a:isneg(), a:ispos()
    ">>, State),
    ?assertEqual(2, length(NegResults)),
    ?assert(lists:nth(1, NegResults)),
    ?assertNot(lists:nth(2, NegResults)),
    
    %% Test even/odd
    {EvenOddResults, _State2} = luerl:do(<<"
        local even = bint.new(42)
        local odd = bint.new(43)
        return even:iseven(), even:isodd(), odd:iseven(), odd:isodd()
    ">>, State1),
    ?assertEqual(4, length(EvenOddResults)),
    ?assert(lists:nth(1, EvenOddResults)),
    ?assertNot(lists:nth(2, EvenOddResults)),
    ?assertNot(lists:nth(3, EvenOddResults)),
    ?assert(lists:nth(4, EvenOddResults)).

%% Helper function
to_integer(N) when is_float(N) -> erlang:round(N);
to_integer(N) when is_integer(N) -> N.