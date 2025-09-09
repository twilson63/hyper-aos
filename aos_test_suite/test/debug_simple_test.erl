-module(debug_simple_test).
-include_lib("eunit/include/eunit.hrl").

debug_test() ->
    State = luerl:init(),
    
    %% Load bint module 
    {ok, BintSrc} = file:read_file("../src/bint_luerl.lua"),
    Code = <<"
        local bint_loader = function()
            ", BintSrc/binary, "
        end
        local bint_module = bint_loader()
        _G.bint = bint_module(256)
        
        -- Debug large number - test addition like the actual test
        local test_str = '1' .. string.rep('0', 100)
        local a = bint.new(test_str)
        local b = bint.new(test_str)
        local large_num = a + b
        
        -- Debug division by zero  
        local div_zero_error = false
        local success, err = pcall(function()
            local a = bint.new(100)
            local b = bint.new(0)
            local c = a // b
            return c
        end)
        
        if not success then
            div_zero_error = true
        end
        
        return tostring(large_num), div_zero_error
    ">>,
    
    {ok, [LargeResult, DivZeroError], _NewState} = luerl:do(Code, State),
    
    io:format("Large number result: ~p~n", [LargeResult]),
    io:format("Division by zero threw error: ~p~n", [DivZeroError]),
    
    %% Test that large number starts with "2" (1e100 + 1e100 = 2e100)
    ExpectedStart = <<"2">>,
    ActualStart = binary:part(LargeResult, 0, 1),
    io:format("Expected start: ~p, Actual start: ~p~n", [ExpectedStart, ActualStart]),
    
    %% Check if the result is a proper large number (should be close to 100 digits)
    io:format("Result length: ~p~n", [byte_size(LargeResult)]),
    ?assert(byte_size(LargeResult) >= 90),
    ?assertEqual(ExpectedStart, ActualStart),
    ?assertEqual(true, DivZeroError).