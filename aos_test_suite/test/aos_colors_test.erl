-module(aos_colors_test).
-include_lib("eunit/include/eunit.hrl").

%% Test that colors are initialized in meta table
colors_initialization_test() ->
    %% Initialize AOS
    LuaState1 = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process
    ProcessMsg = aos_test_helpers:create_process_message(),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState1, State, ProcessAssignment),
    
    %% Ensure colors are available
    LuaState2a = aos_test_helpers:ensure_colors_initialized(LuaState2),
    
    %% Check that colors table exists in _G.colors
    {ok, [ResetColor], _} = luerl:do("return _G.colors.reset", LuaState2a),
    {ok, [CyanColor], _} = luerl:do("return _G.colors.cyan", LuaState2a),
    {ok, [BrightGreenColor], _} = luerl:do("return _G.colors.bright_green", LuaState2a),
    {ok, [BoldColor], _} = luerl:do("return _G.colors.bold", LuaState2a),
    
    %% Verify some key colors exist
    ?assertEqual(<<"\e[0m">>, ResetColor),
    ?assertEqual(<<"\e[36m">>, CyanColor),
    ?assertEqual(<<"\e[92m">>, BrightGreenColor),
    ?assertEqual(<<"\e[1m">>, BoldColor).

%% Test colored prompt output
colored_prompt_test() ->
    %% Initialize AOS
    LuaState1 = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process
    ProcessMsg = aos_test_helpers:create_process_message(),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState1, State, ProcessAssignment),
    
    %% Ensure colors are available
    LuaState2a = aos_test_helpers:ensure_colors_initialized(LuaState2),
    
    %% Call prompt function
    {ok, [PromptResult], _} = luerl:do("return prompt()", LuaState2a),
    
    %% Convert to binary
    PromptBinary = iolist_to_binary(PromptResult),
    
    %% Check that prompt contains color codes
    ?assert(binary:match(PromptBinary, <<"\e[36m">>) =/= nomatch), % cyan
    ?assert(binary:match(PromptBinary, <<"\e[92m">>) =/= nomatch), % bright_green
    ?assert(binary:match(PromptBinary, <<"\e[0m">>) =/= nomatch),  % reset
    
    %% Check that prompt contains expected text
    ?assert(binary:match(PromptBinary, <<"hyper">>) =/= nomatch),
    ?assert(binary:match(PromptBinary, <<"aos">>) =/= nomatch),
    ?assert(binary:match(PromptBinary, <<"dev">>) =/= nomatch).

%% Test prompt with messages in inbox
prompt_with_inbox_test() ->
    %% Initialize AOS
    LuaState1 = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process
    ProcessMsg = aos_test_helpers:create_process_message(),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState1, State, ProcessAssignment),
    
    %% Ensure colors are available
    LuaState2a = aos_test_helpers:ensure_colors_initialized(LuaState2),
    
    %% Add some messages to Inbox
    EvalMsg1 = aos_test_helpers:create_eval_message(<<"return 'test'">>, aos_test_helpers:default_owner()),
    Assignment1 = aos_test_helpers:create_assignment(EvalMsg1),
    {_, LuaState3} = aos_test_helpers:call_compute(LuaState2a, State, Assignment1),
    
    %% Call prompt function
    {ok, [PromptResult], _} = luerl:do("return prompt()", LuaState3),
    PromptBinary = iolist_to_binary(PromptResult),
    
    %% Check that prompt shows inbox count with color
    ?assert(binary:match(PromptBinary, <<"\e[95m">>) =/= nomatch), % bright_magenta for count
    ?assert(binary:match(PromptBinary, <<"[">>) =/= nomatch),
    ?assert(binary:match(PromptBinary, <<"]">>) =/= nomatch).