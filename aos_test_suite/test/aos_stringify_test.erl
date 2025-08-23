-module(aos_stringify_test).
-include_lib("eunit/include/eunit.hrl").

%% Test stringify with simple array
stringify_simple_array_test() ->
    LuaState1 = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process
    ProcessMsg = aos_test_helpers:create_process_message(),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState1, State, ProcessAssignment),
    
    %% Ensure colors are available
    LuaState2a = aos_test_helpers:ensure_colors_initialized(LuaState2),
    
    %% Test stringify on simple array
    EvalMsg = aos_test_helpers:create_eval_message(
        <<"return stringify({1, 2, 'hello', 'world'})">>,
        aos_test_helpers:default_owner()
    ),
    Assignment = aos_test_helpers:create_assignment(EvalMsg),
    {_Result, LuaState3} = aos_test_helpers:call_compute(LuaState2a, State, Assignment),
    Output = aos_test_helpers:extract_output_data({_Result, LuaState3}),
    
    %% Check it contains colored elements
    ?assert(binary:match(Output, <<"\e[34m1\e[0m">>) =/= nomatch), % blue number
    ?assert(binary:match(Output, <<"\e[32m\"hello\"\e[0m">>) =/= nomatch). % green string

%% Test stringify with nested table
stringify_nested_table_test() ->
    LuaState1 = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process
    ProcessMsg = aos_test_helpers:create_process_message(),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState1, State, ProcessAssignment),
    
    %% Ensure colors are available
    LuaState2a = aos_test_helpers:ensure_colors_initialized(LuaState2),
    
    %% Test stringify on nested table
    EvalMsg = aos_test_helpers:create_eval_message(
        <<"return stringify({name = 'test', value = 42, nested = {a = 1, b = 2}})">>,
        aos_test_helpers:default_owner()
    ),
    Assignment = aos_test_helpers:create_assignment(EvalMsg),
    {_Result, LuaState3} = aos_test_helpers:call_compute(LuaState2a, State, Assignment),
    Output = aos_test_helpers:extract_output_data({_Result, LuaState3}),
    
    %% Check colored keys and values
    ?assert(binary:match(Output, <<"\e[31mname\e[0m">>) =/= nomatch), % red key
    ?assert(binary:match(Output, <<"\e[32m\"test\"\e[0m">>) =/= nomatch), % green string
    ?assert(binary:match(Output, <<"\e[34m42\e[0m">>) =/= nomatch). % blue number

%% Test print function with table
print_table_test() ->
    LuaState1 = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process
    ProcessMsg = aos_test_helpers:create_process_message(),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState1, State, ProcessAssignment),
    
    %% Ensure colors are available
    LuaState2a = aos_test_helpers:ensure_colors_initialized(LuaState2),
    
    %% Test print with table
    EvalMsg = aos_test_helpers:create_eval_message(
        <<"print({test = 'value', num = 123}); return 'done'">>,
        aos_test_helpers:default_owner()
    ),
    Assignment = aos_test_helpers:create_assignment(EvalMsg),
    {_Result, LuaState3} = aos_test_helpers:call_compute(LuaState2a, State, Assignment),
    
    %% Check _OUTPUT contains colored table - use Lua directly to get _OUTPUT
    {ok, [OutputValue], _} = luerl:do("return _G._OUTPUT", LuaState3),
    OutputBinary = iolist_to_binary(OutputValue),
    
    ?assert(binary:match(OutputBinary, <<"\e[31mtest\e[0m">>) =/= nomatch),
    ?assert(binary:match(OutputBinary, <<"\e[32m\"value\"\e[0m">>) =/= nomatch).

%% Test circular reference handling
circular_reference_test() ->
    LuaState1 = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process
    ProcessMsg = aos_test_helpers:create_process_message(),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState1, State, ProcessAssignment),
    
    %% Ensure colors are available
    LuaState2a = aos_test_helpers:ensure_colors_initialized(LuaState2),
    
    %% Create circular reference
    EvalMsg = aos_test_helpers:create_eval_message(
        <<"local t = {a = 1}; t.self = t; return stringify(t)">>,
        aos_test_helpers:default_owner()
    ),
    Assignment = aos_test_helpers:create_assignment(EvalMsg),
    {_Result, LuaState3} = aos_test_helpers:call_compute(LuaState2a, State, Assignment),
    Output = aos_test_helpers:extract_output_data({_Result, LuaState3}),
    
    %% Check circular reference is handled
    ?assert(binary:match(Output, <<"<circular reference>">>) =/= nomatch).

%% Test multiple arguments to print
print_multiple_args_test() ->
    LuaState1 = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process
    ProcessMsg = aos_test_helpers:create_process_message(),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState1, State, ProcessAssignment),
    
    %% Ensure colors are available
    LuaState2a = aos_test_helpers:ensure_colors_initialized(LuaState2),
    
    %% Test print with multiple arguments
    EvalMsg = aos_test_helpers:create_eval_message(
        <<"print('Hello', {x = 10}, 'World'); return 'done'">>,
        aos_test_helpers:default_owner()
    ),
    Assignment = aos_test_helpers:create_assignment(EvalMsg),
    {_Result, LuaState3} = aos_test_helpers:call_compute(LuaState2a, State, Assignment),
    
    %% Check _OUTPUT using Lua directly to get _OUTPUT
    {ok, [OutputValue], _} = luerl:do("return _G._OUTPUT", LuaState3),
    OutputBinary = iolist_to_binary(OutputValue),
    
    %% Should have tab-separated values
    ?assert(binary:match(OutputBinary, <<"Hello\t">>) =/= nomatch),
    ?assert(binary:match(OutputBinary, <<"\tWorld">>) =/= nomatch),
    ?assert(binary:match(OutputBinary, <<"\e[31mx\e[0m">>) =/= nomatch).