-module(debug_outbox).
-export([test/0]).

test() ->
    LuaState0 = luerl:init(),
    {ok, AosContent} = file:read_file("../aos.lua"),
    {_, LuaState1} = luerl:do(binary_to_list(AosContent), LuaState0),
    
    % Create a test that should produce outbox messages
    TestCode = "
        local state = {
            process = {
                commitments = {
                    ['test-key'] = {type = 'RSA-PSS-512'}
                }
            },
            results = {
                outbox = {},
                output = {data = '', prompt = ''}
            }
        }
        
        local assignment = {
            body = {
                id = 'test',
                from = 'test',
                owner = 'test',
                tags = {},
                action = 'eval',
                data = 'send({target=\"ID\", data=\"hello\"})',
                commitments = {
                    ['test-key'] = {type = 'RSA-PSS-512'}
                }
            }
        }
        
        local status, result = compute(state, assignment)
        
        -- Debug: Check what's in the outbox
        print('Outbox type: ' .. type(result.results.outbox))
        print('Outbox length: ' .. #result.results.outbox)
        
        if #result.results.outbox > 0 then
            for i, msg in ipairs(result.results.outbox) do
                print('Message ' .. i .. ':')
                for k, v in pairs(msg) do
                    print('  ' .. tostring(k) .. ' = ' .. tostring(v))
                end
            end
        end
        
        -- Convert outbox to a format we can parse
        local outbox_data = {}
        for i, msg in ipairs(result.results.outbox) do
            -- Return as indexed array with string keys
            outbox_data[i] = {
                target = tostring(msg.target or ''),
                data = tostring(msg.data or '')
            }
        end
        
        -- Return number of messages for debugging
        return #result.results.outbox, outbox_data
    ",
    
    {[Count, Outbox], LuaState2} = luerl:do(TestCode, LuaState1),
    io:format("~nOutbox count: ~p~n", [Count]),
    io:format("Returned outbox: ~p~n", [Outbox]),
    
    % Try to extract the table
    case Outbox of
        {tref, Ref} ->
            % Get the table from Lua state
            case luerl:get_table([Ref], LuaState2) of
                {Table, _} ->
                    io:format("Extracted table: ~p~n", [Table]);
                Error ->
                    io:format("Error extracting table: ~p~n", [Error])
            end;
        _ ->
            ok
    end,
    
    ok.