-module(debug_parse_test).
-include_lib("eunit/include/eunit.hrl").

debug_test() ->
    State = luerl:init(),
    
    %% Test tonumber behavior in LUERL for large numbers
    Code = <<"
        local test_str = '1000000000000000000'
        local as_number = tonumber(test_str)
        
        -- Check what type of number we get
        local result_str = tostring(as_number)
        
        -- Test if the string round-trips correctly
        local round_trip = tostring(tonumber(result_str))
        
        return test_str, result_str, round_trip, as_number
    ">>,
    
    {ok, [TestStr, ResultStr, RoundTrip, AsNumber], _NewState} = luerl:do(Code, State),
    
    io:format("Original string: ~s~n", [TestStr]),
    io:format("tonumber result as string: ~s~n", [ResultStr]),
    io:format("Round trip: ~s~n", [RoundTrip]),
    io:format("As number (raw): ~p~n", [AsNumber]),
    
    %% LUERL converts large numbers to scientific notation
    %% Check that the conversion works correctly
    ?assertEqual(<<"1.0e18">>, ResultStr).