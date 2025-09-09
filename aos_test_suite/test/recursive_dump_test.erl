-module(recursive_dump_test).
-include_lib("eunit/include/eunit.hrl").

%% Test the exact recursive scenario in dump.lua
recursive_dumptbl_test() ->
    LuaState = luerl:init(),
    
    % Load a simplified version of the problematic dumptbl function
    Code = "
        -- Simplified version of the problematic part of dumptbl
        local function simple_dumptbl(tbl, depth)
            if depth > 3 then return nil end  -- Prevent infinite recursion for testing
            
            local res = {}
            local fieldIndent = '    '
            
            for k, v in pairs(tbl) do
                local key = tostring(k)
                local vt = type(v)
                local kv
                
                if vt == 'table' then
                    -- This is the line that's failing - recursive call
                    local nested_result = simple_dumptbl(v, depth + 1)
                    kv = string.format('%s%s = %s', fieldIndent, key, nested_result)
                else
                    kv = string.format('%s%s = %s', fieldIndent, key, tostring(v))
                end
                
                table.insert(res, kv)
            end
            
            return table.concat(res, ', ')
        end
        
        local test_table = {
            user = {
                name = 'Alice',
                address = {
                    city = 'NYC'
                }
            }
        }
        
        return simple_dumptbl(test_table, 1)
    ",
    
    Result = luerl:do(Code, LuaState),
    io:format("Recursive dumptbl test result: ~p~n", [Result]).

with_nil_guard_test() ->
    LuaState = luerl:init(),
    
    % Test with a nil guard like we should have in dump.lua
    Code = "
        local function safe_dumptbl(tbl, depth)
            if depth > 3 then return 'nil' end  -- Return string instead of nil
            
            local res = {}
            local fieldIndent = '    '
            
            for k, v in pairs(tbl) do
                local key = tostring(k)
                local vt = type(v)
                local kv
                
                if vt == 'table' then
                    local nested_result = safe_dumptbl(v, depth + 1)
                    -- Guard against nil result
                    nested_result = nested_result or 'nil'
                    kv = string.format('%s%s = %s', fieldIndent, key, nested_result)
                else
                    kv = string.format('%s%s = %s', fieldIndent, key, tostring(v))
                end
                
                table.insert(res, kv)
            end
            
            return table.concat(res, ', ')
        end
        
        local test_table = {
            user = {
                name = 'Alice',
                address = {
                    city = 'NYC'
                }
            }
        }
        
        return safe_dumptbl(test_table, 1)
    ",
    
    Result = luerl:do(Code, LuaState),
    io:format("Safe recursive dumptbl result: ~p~n", [Result]).

% Test the exact conditions that might cause dumptbl to return nil in dump.lua
dumptbl_nil_conditions_test() ->
    LuaState = luerl:init(),
    
    % Load dump.lua and try to find conditions where dumptbl returns nil
    {ok, DumpBin} = file:read_file("../src/dump.lua"),
    DumpSrc = binary_to_list(DumpBin),
    
    % Load and test specific conditions
    LoadAndTestCode = DumpSrc ++ "
        
        -- Test conditions that might cause dumptbl to return nil
        local test1 = dumptbl({a = 1}, 1, '', '    ', {
            LF = '\n',
            circular = {},
            filter = function(val) return val end,
            udata = nil
        })
        
        local test2 = dumptbl({user = {name = 'Alice'}}, 1, '', '    ', {
            LF = '\n', 
            circular = {},
            filter = function(val) return val end,
            udata = nil
        })
        
        return test1, test2
    ",
    
    Result = luerl:do(LoadAndTestCode, LuaState),
    io:format("Direct dumptbl test result: ~p~n", [Result]).