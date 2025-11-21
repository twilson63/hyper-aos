-module(debug_multiplication_test).
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
        
        local a = bint.new('1000000000000000000')
        local b = bint.new('1000000000000000000')
        
        -- Debug the input values
        local a_str = tostring(a)
        local b_str = tostring(b)
        
        -- Debug the multiplication
        local c = a * b
        local c_str = tostring(c)
        
        return a_str, b_str, c_str
    ">>,
    
    {ok, [AStr, BStr, CStr], _NewState} = luerl:do(Code, State),
    
    io:format("A: ~s~n", [AStr]),
    io:format("B: ~s~n", [BStr]),
    io:format("C (A*B): ~s~n", [CStr]),
    io:format("Length of A: ~p~n", [byte_size(AStr)]),
    io:format("Length of B: ~p~n", [byte_size(BStr)]),
    io:format("Length of C: ~p~n", [byte_size(CStr)]),
    
    %% Expected result: 1e18 * 1e18 = 1e36 = 1 followed by 36 zeros
    Expected = <<"1000000000000000000000000000000000000">>,
    io:format("Expected: ~s~n", [Expected]),
    io:format("Expected length: ~p~n", [byte_size(Expected)]),
    
    %% Compare with expected
    ?assertEqual(Expected, CStr).