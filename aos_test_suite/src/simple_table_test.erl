-module(simple_table_test).
-export([test/0]).

test() ->
    LuaState0 = luerl:init(),
    {ok, AosContent} = file:read_file("../aos.lua"),
    {_, LuaState1} = luerl:do(binary_to_list(AosContent), LuaState0),
    
    % Create the exact structure we need in Lua directly
    SetupCode = "
        -- Create state
        test_state = {
            results = {
                outbox = {},
                output = {data = '', prompt = ''}
            }
        }
        
        -- Create assignment with eval action
        test_assignment = {
            body = {
                id = 'test',
                from = 'test', 
                owner = 'test',
                tags = {},
                action = 'eval',
                data = 'return 42'
            }
        }
        
        -- Call compute
        local status, result = compute(test_state, test_assignment)
        
        -- Return the output data
        return result.results.output.data
    ",
    
    {[OutputData], _} = luerl:do(SetupCode, LuaState1),
    io:format("Output data: ~p~n", [OutputData]),
    
    % Now test with security
    SecurityCode = "
        -- Create state with RSA-PSS-512 commitment
        secure_state = {
            process = {
                commitments = {
                    key123 = {type = 'RSA-PSS-512'}
                }
            },
            results = {
                outbox = {},
                output = {data = '', prompt = ''}
            }
        }
        
        -- Test 1: No matching commitment
        no_match_assignment = {
            body = {
                id = 'test',
                from = 'test',
                owner = 'test', 
                tags = {},
                action = 'eval',
                data = 'return 99'
            }
        }
        
        local status1, result1 = compute(secure_state, no_match_assignment)
        print('Test 1 - No match: ' .. result1.results.output.data)
        
        -- Test 2: With matching commitment  
        match_assignment = {
            body = {
                id = 'test',
                from = 'test',
                owner = 'test',
                tags = {},
                action = 'eval',
                data = 'return 100',
                commitments = {
                    key123 = {type = 'RSA-PSS-512'}
                }
            }
        }
        
        local status2, result2 = compute(secure_state, match_assignment)
        print('Test 2 - Match: ' .. result2.results.output.data)
        
        return 'done'
    ",
    
    {[Result], _} = luerl:do(SecurityCode, LuaState1),
    io:format("Security test result: ~p~n", [Result]),
    
    ok.