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
    
    %% Check that colors table exists
    {ColorsTable, _} = luerl:get_table([<<"meta">>, <<"colors">>], LuaState2),
    
    %% Verify some key colors exist
    ?assertMatch({<<"reset">>, <<"\e[0m">>}, lists:keyfind(<<"reset">>, 1, ColorsTable)),
    ?assertMatch({<<"cyan">>, <<"\e[36m">>}, lists:keyfind(<<"cyan">>, 1, ColorsTable)),
    ?assertMatch({<<"bright_green">>, <<"\e[92m">>}, lists:keyfind(<<"bright_green">>, 1, ColorsTable)),
    ?assertMatch({<<"bold">>, <<"\e[1m">>}, lists:keyfind(<<"bold">>, 1, ColorsTable)).

%% Test colored prompt output
colored_prompt_test() ->
    %% Initialize AOS
    LuaState1 = aos_test_helpers:initialize_aos(),
    State = aos_test_helpers:create_base_state(),
    
    %% Initialize process
    ProcessMsg = aos_test_helpers:create_process_message(),
    ProcessAssignment = aos_test_helpers:create_assignment(ProcessMsg),
    {_, LuaState2} = aos_test_helpers:call_compute(LuaState1, State, ProcessAssignment),
    
    %% Call prompt function
    {[PromptResult], _} = luerl:call_function([<<"prompt">>], [], LuaState2),
    
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
    
    %% Add some messages to Inbox
    EvalMsg1 = aos_test_helpers:create_eval_message(<<"return 'test'">>, aos_test_helpers:default_owner()),
    Assignment1 = aos_test_helpers:create_assignment(EvalMsg1),
    {_, LuaState3} = aos_test_helpers:call_compute(LuaState2, State, Assignment1),
    
    %% Call prompt function
    {[PromptResult], _} = luerl:call_function([<<"prompt">>], [], LuaState3),
    PromptBinary = iolist_to_binary(PromptResult),
    
    %% Check that prompt shows inbox count with color
    ?assert(binary:match(PromptBinary, <<"\e[95m">>) =/= nomatch), % bright_magenta for count
    ?assert(binary:match(PromptBinary, <<"[">>) =/= nomatch),
    ?assert(binary:match(PromptBinary, <<"]">>) =/= nomatch).