-module(debug_factorial_test).
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
        
        -- Test a simpler factorial first - use proper recursive function definition
        local factorial
        factorial = function(n)
            print('factorial called with n =', tostring(n), 'type =', type(n))
            if n <= 1 then
                print('Base case, returning bint.one()')
                return bint.one()
            end
            print('Recursive case, computing n - bint.one()')
            local n_minus_one = n - bint.one()
            print('n_minus_one =', tostring(n_minus_one), 'type =', type(n_minus_one))
            print('About to call factorial recursively')
            local sub_result = factorial(n_minus_one)
            print('Got sub_result =', tostring(sub_result), 'type =', type(sub_result))
            print('About to multiply n * sub_result')
            local result = n * sub_result
            print('Result =', tostring(result), 'type =', type(result))
            return result
        end
        
        -- Test with a small number first
        print('Testing factorial(5)')
        local result5 = factorial(bint.new(5))
        print('factorial(5) =', tostring(result5))
        
        return tostring(result5)
    ">>,
    
    case luerl:do(Code, State) of
        {ok, [Result], _NewState} ->
            io:format("Success! factorial(5) = ~s~n", [Result]),
            ?assertEqual(<<"120">>, Result);
        {lua_error, Reason, _} ->
            io:format("Lua error: ~p~n", [Reason]),
            ?assert(false);
        Other ->
            io:format("Other result: ~p~n", [Other]),
            ?assert(false)
    end.