-module(test_hmac_rejection).
-export([test/0]).

test() ->
    LuaState0 = luerl:init(),
    {ok, AosContent} = file:read_file("../aos.lua"),
    {_, LuaState1} = luerl:do(binary_to_list(AosContent), LuaState0),
    
    % Test that hmac-sha256 commitments are not accepted for eval
    TestCode = "
        -- Test 1: State with owner set and message with hmac-sha256 commitment should NOT allow eval
        local state1 = {
            owner = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0',
            process = {
                commitments = {
                    ['key1'] = {type = 'hmac-sha256', committer = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0'}
                }
            },
            results = {
                outbox = {},
                output = {data = '', prompt = ''}
            }
        }
        
        local assignment1 = {
            body = {
                id = 'test',
                from = 'test',
                owner = 'test',
                tags = {},
                action = 'eval',
                data = 'return \"should fail\"',
                commitments = {
                    ['key1'] = {type = 'hmac-sha256', committer = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0'}
                }
            }
        }
        
        local status1, result1 = compute(state1, assignment1)
        print('Test 1 - hmac-sha256 in state: ' .. result1.results.output.data)
        
        -- Test 2: State with RSA-PSS-512 commitment, message with hmac-sha256 should fail
        local state2 = {
            owner = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0',
            process = {
                commitments = {
                    ['key1'] = {type = 'RSA-PSS-512', committer = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0'}
                }
            },
            results = {
                outbox = {},
                output = {data = '', prompt = ''}
            }
        }
        
        local assignment2 = {
            body = {
                id = 'test',
                from = 'test',
                owner = 'test',
                tags = {},
                action = 'eval',
                data = 'return \"should fail\"',
                commitments = {
                    ['key1'] = {type = 'hmac-sha256', committer = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0'}
                }
            }
        }
        
        local status2, result2 = compute(state2, assignment2)
        print('Test 2 - RSA-PSS-512 state, hmac-sha256 message: ' .. result2.results.output.data)
        
        -- Test 3: Mixed commitments - RSA-PSS-512 with matching committer should work
        local state3 = {
            owner = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0',
            process = {
                commitments = {
                    ['key1'] = {type = 'hmac-sha256', committer = 'SomeOtherUser123456789012345678901234567890'},
                    ['key2'] = {type = 'RSA-PSS-512', committer = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0'}
                }
            },
            results = {
                outbox = {},
                output = {data = '', prompt = ''}
            }
        }
        
        local assignment3 = {
            body = {
                id = 'test',
                from = 'test',
                owner = 'test',
                tags = {},
                action = 'eval',
                data = 'return \"success\"',
                commitments = {
                    ['key2'] = {type = 'RSA-PSS-512', committer = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0'}
                }
            }
        }
        
        local status3, result3 = compute(state3, assignment3)
        print('Test 3 - Mixed state, RSA-PSS-512 message on key2: ' .. result3.results.output.data)
        
        return 'All tests completed'
    ",
    
    {[Result], _} = luerl:do(TestCode, LuaState1),
    io:format("~nResult: ~p~n", [Result]),
    
    ok.