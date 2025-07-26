-module(aos_math_eunit_test).
-include_lib("eunit/include/eunit.hrl").

%% Test addition
addition_test() ->
    Result = test_math_expression(<<"1 + 1">>),
    ?assertEqual(<<"2">>, Result).

%% Test subtraction
subtraction_test() ->
    Result = test_math_expression(<<"5 - 3">>),
    ?assertEqual(<<"2">>, Result).

%% Test multiplication
multiplication_test() ->
    Result = test_math_expression(<<"4 * 3">>),
    ?assertEqual(<<"12">>, Result).

%% Test division
division_test() ->
    Result = test_math_expression(<<"10 / 2">>),
    ?assertEqual(<<"5.0">>, Result).

%% Test exponentiation
exponentiation_test() ->
    Result = test_math_expression(<<"2 ^ 3">>),
    ?assertEqual(<<"8.0">>, Result).

%% Test modulo
modulo_test() ->
    Result = test_math_expression(<<"100 % 7">>),
    ?assertEqual(<<"2">>, Result).

%% Test complex expression
complex_expression_test() ->
    Result = test_math_expression(<<"(2 + 3) * 4">>),
    ?assertEqual(<<"20">>, Result).

%% Test floating point
floating_point_test() ->
    Result = test_math_expression(<<"1.5 + 2.5">>),
    ?assertEqual(<<"4.0">>, Result).

%% Helper function to test a math expression
test_math_expression(Expression) ->
    %% Initialize AOS
    LuaState = aos_test_helpers:initialize_aos(),
    
    %% Create base state and initialize process
    State = aos_test_helpers:create_base_state(),
    LuaState2 = aos_test_helpers:initialize_process(LuaState, State),
    
    %% Create assignment with math expression
    Assignment = aos_test_helpers:create_eval_assignment(<<"return ", Expression/binary>>),
    
    %% Call compute and extract result
    Result = aos_test_helpers:call_compute(LuaState2, State, Assignment),
    aos_test_helpers:extract_output_data(Result).