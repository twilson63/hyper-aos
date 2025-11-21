-module(test_luerl_direct).

-include_lib("eunit/include/eunit.hrl").

luerl_direct_test() ->
    io:format("=== Direct LUERL Test ===~n"),
    
    %% Initialize LUERL exactly like the test does
    State = luerl:init(),
    
    %% Load bint module
    BintPath = "../src/bint_luerl.lua",
    {ok, BintSrc} = file:read_file(BintPath),
    
    %% Execute exactly as the test does
    Code = <<"
        local bint_loader = function()
            ", BintSrc/binary, "
        end
        local bint_module = bint_loader()
        -- Create a bint instance with 256 bits (default)
        _G.bint = bint_module(256)
        return 'bint loaded'
    ">>,
    
    {ok, [LoadResult], State1} = luerl:do(Code, State),
    io:format("Module load result: ~p~n", [LoadResult]),
    
    %% Test the large number case
    TestCode = <<"
        local a = bint.new('1' .. string.rep('0', 100))
        local b = bint.new('1' .. string.rep('0', 100))
        local c = a + b
        return tostring(c):sub(1, 1)
    ">>,
    
    io:format("Running large number test...~n"),
    case luerl:do(TestCode, State1) of
        {ok, [Result], _State2} ->
            io:format("SUCCESS: Result = ~p~n", [Result]),
            io:format("Expected: <<\"2\">>~n"),
            io:format("Match: ~p~n", [Result =:= <<"2">>]);
        {error, Reason} ->
            io:format("ERROR: ~p~n", [Reason]);
        Other ->
            io:format("UNEXPECTED: ~p~n", [Other])
    end,
    
    %% Also test simpler cases
    io:format("~nTesting simpler cases:~n"),
    
    SimpleCases = [
        <<"return tostring(bint.new('123'))">>,
        <<"return tostring(bint.new('1000000000'))">>,  %% 10^9
        <<"return tostring(bint.new('1' .. string.rep('0', 20)))">>,  %% 21 digits
        <<"local a = bint.new('100'); return tostring(a + a)">>
    ],
    
    lists:foreach(fun(TestCase) ->
        case luerl:do(TestCase, State1) of
            {ok, [SimpleResult], _} ->
                io:format("  ~s -> ~p~n", [TestCase, SimpleResult]);
            {error, SimpleError} ->
                io:format("  ~s -> ERROR: ~p~n", [TestCase, SimpleError])
        end
    end, SimpleCases),
    
    ok.