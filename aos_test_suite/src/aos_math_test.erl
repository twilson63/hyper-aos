-module(aos_math_test).
-include_lib("eunit/include/eunit.hrl").

%% Individual test functions - no generators
addition_test() ->
    Result = test_math_expression(<<"1 + 1">>),
    ?assertEqual(<<"2">>, Result).

subtraction_test() ->
    Result = test_math_expression(<<"5 - 3">>),
    ?assertEqual(<<"2">>, Result).

multiplication_test() ->
    Result = test_math_expression(<<"4 * 3">>),
    ?assertEqual(<<"12">>, Result).

division_test() ->
    Result = test_math_expression(<<"10 / 2">>),
    ?assertEqual(<<"5.0">>, Result).

exponentiation_test() ->
    Result = test_math_expression(<<"2 ^ 3">>),
    ?assertEqual(<<"8.0">>, Result).

complex_expression_test() ->
    Result = test_math_expression(<<"(2 + 3) * 4">>),
    ?assertEqual(<<"20">>, Result).

modulo_test() ->
    Result = test_math_expression(<<"100 % 7">>),
    ?assertEqual(<<"2">>, Result).

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
