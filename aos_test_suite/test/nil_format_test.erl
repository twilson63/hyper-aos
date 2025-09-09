-module(nil_format_test).
-include_lib("eunit/include/eunit.hrl").

%% Test how LUERL handles nil values in string.format
nil_in_string_format_test() ->
    LuaState = luerl:init(),
    
    % Test string.format with nil value - this likely causes the undefined_function,nil error
    Code1 = "return string.format('%s', nil)",
    Result1 = luerl:do(Code1, LuaState),
    io:format("string.format with nil result: ~p~n", [Result1]),
    
    % Test with multiple arguments where one is nil
    Code2 = "return string.format('%s%s = %s', '    ', 'key', nil)",
    Result2 = luerl:do(Code2, LuaState),
    io:format("string.format with multiple args including nil: ~p~n", [Result2]),
    
    % Test what happens when we convert nil to string first
    Code3 = "return string.format('%s', tostring(nil))",
    Result3 = luerl:do(Code3, LuaState),
    io:format("string.format with tostring(nil): ~p~n", [Result3]).

function_returning_nil_test() ->
    LuaState = luerl:init(),
    
    % Test a function that might return nil like dumptbl could
    Code = "
        local function might_return_nil(flag)
            if flag then
                return 'valid_string'
            end
            return nil  -- This could happen in dumptbl
        end
        
        local result = might_return_nil(false)
        return string.format('%s%s = %s', '    ', 'key', result)
    ",
    
    Result = luerl:do(Code, LuaState),
    io:format("Function returning nil in format: ~p~n", [Result]).

safe_format_workaround_test() ->
    LuaState = luerl:init(),
    
    % Test a safe format that handles nil values
    Code = "
        local function safe_format(fmt, ...)
            local args = {...}
            for i, arg in ipairs(args) do
                if arg == nil then
                    args[i] = 'nil'
                end
            end
            return string.format(fmt, unpack(args))
        end
        
        return safe_format('%s%s = %s', '    ', 'key', nil)
    ",
    
    Result = luerl:do(Code, LuaState),
    io:format("Safe format workaround result: ~p~n", [Result]).

% Test unpack with nil values which might also be an issue
unpack_with_nil_test() ->
    LuaState = luerl:init(),
    
    Code = "
        local args = {'hello', 'world', nil, 'test'}
        return string.format('%s %s %s %s', unpack(args))
    ",
    
    Result = luerl:do(Code, LuaState),
    io:format("unpack with nil result: ~p~n", [Result]).