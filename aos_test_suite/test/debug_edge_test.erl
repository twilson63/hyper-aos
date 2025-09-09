-module(debug_edge_test).
-include_lib("eunit/include/eunit.hrl").

debug_edge_test() ->
    State = luerl:init(),
    
    %% Load bint module 
    {ok, BintSrc} = file:read_file("../src/bint_luerl.lua"),
    Code = <<"
        local bint_loader = function()
            ", BintSrc/binary, "
        end
        local bint_module = bint_loader()
        _G.bint = bint_module(256)
        
        return 'bint loaded'
    ">>,
    
    {ok, [_LoadResult], State1} = luerl:do(Code, State),
    
    %% Test division by zero 
    io:format("Testing division by zero...~n"),
    DivZeroResult = case luerl:do(<<"
        local a = bint.new(100)
        local b = bint.new(0)
        local c = a // b
        return c:tointeger()
    ">>, State1) of
        {ok, ResultValues, _} -> 
            io:format("Unexpected success with values: ~p~n", [ResultValues]),
            no_error;
        {lua_error, {assert_error, <<"attempt to divide by zero">>}, _} ->
            io:format("Expected division by zero error occurred~n"),
            caught_error;
        {lua_error, Reason, _} -> 
            io:format("Other lua error occurred: ~p~n", [Reason]),
            caught_error;
        Other -> 
            io:format("Unexpected result: ~p~n", [Other]),
            other_error
    end,
    
    ?assertEqual(caught_error, DivZeroResult).