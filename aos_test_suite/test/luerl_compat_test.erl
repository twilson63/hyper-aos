-module(luerl_compat_test).
-include_lib("eunit/include/eunit.hrl").

%% Test LUERL string.format compatibility
string_format_basic_test() ->
    State = luerl:init(),
    
    % Test basic string.format
    Code = "return string.format('%s', 'hello')",
    Result = luerl:do(Code, State),
    io:format("Basic string.format result: ~p~n", [Result]).

string_format_quote_test() ->
    State = luerl:init(),
    
    % Test %q format specifier used heavily in dump.lua
    Code = "return string.format('%q', 'hello')",
    Result = luerl:do(Code, State),
    io:format("Quote string.format result: ~p~n", [Result]).

string_format_spaces_test() ->
    State = luerl:init(),
    
    % Test space formatting like in dump.lua line 187: strformat('%' .. tostring(indent) .. 's', '')
    Code = "return string.format('%4s', '')",
    Result = luerl:do(Code, State),
    io:format("Space string.format result: ~p~n", [Result]).

string_format_concatenation_test() ->
    State = luerl:init(),
    
    % Test format string concatenation like in dump.lua
    Code = "local indent = 4; return string.format('%' .. tostring(indent) .. 's', '')",
    Result = luerl:do(Code, State),
    io:format("Concatenated format result: ~p~n", [Result]).

test_all_string_functions() ->
    State = luerl:init(),
    
    % Test what string functions are available
    Code = "
        local funcs = {}
        for k, v in pairs(string) do
            table.insert(funcs, k)
        end
        return funcs
    ",
    Result = luerl:do(Code, State),
    io:format("Available string functions: ~p~n", [Result]).

% Test math.floor and math.huge used in dump.lua
math_functions_test() ->
    State = luerl:init(),
    
    % Test math.floor
    Code1 = "return math.floor(3.7)",
    Result1 = luerl:do(Code1, State),
    io:format("math.floor result: ~p~n", [Result1]),
    
    % Test math.huge
    Code2 = "return math.huge",
    Result2 = luerl:do(Code2, State),
    io:format("math.huge result: ~p~n", [Result2]).