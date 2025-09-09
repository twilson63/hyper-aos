-module(minimal_dump_test).
-include_lib("eunit/include/eunit.hrl").

%% Test minimal dump functionality with LUERL
strformat_directly_test() ->
    LuaState = luerl:init(),
    
    % Test the exact line that's failing in dump.lua (line 73)
    Code = "
        local strformat = string.format
        local val = 'hello'
        return strformat('%q', val)
    ",
    
    Result = luerl:do(Code, LuaState),
    io:format("Direct strformat test result: ~p~n", [Result]).

tostring_in_dump_test() ->
    LuaState = luerl:init(),
    
    % Test tostring function used in dump.lua
    Code = "
        local tbl = {a = 1}
        local ref = tostring(tbl)
        return ref
    ",
    
    Result = luerl:do(Code, LuaState),
    io:format("tostring test result: ~p~n", [Result]).

dump_minimal_test() ->
    LuaState = luerl:init(),
    
    % Test a very minimal version of the dump logic that's failing
    Code = "
        local function minimal_dump(val)
            local t = type(val)
            if t == 'string' then
                return string.format('%q', val)
            elseif t == 'number' or t == 'boolean' then
                return tostring(val)
            end
            return string.format('%q', tostring(val))
        end
        
        local test_val = 'hello'
        return minimal_dump(test_val)
    ",
    
    Result = luerl:do(Code, LuaState),
    io:format("Minimal dump test result: ~p~n", [Result]).

dump_table_minimal_test() ->
    LuaState = luerl:init(),
    
    % Test minimal table dumping
    Code = "
        local function dump_simple(tbl)
            local res = '{'
            for k, v in pairs(tbl) do
                res = res .. tostring(k) .. '=' .. string.format('%q', tostring(v)) .. ','
            end
            res = res .. '}'
            return res
        end
        
        local test_tbl = {a = 1, b = 'hello'}
        return dump_simple(test_tbl)
    ",
    
    Result = luerl:do(Code, LuaState),
    io:format("Minimal table dump result: ~p~n", [Result]).