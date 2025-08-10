%%%-------------------------------------------------------------------
%%% @doc EUnit tests for AOS Handlers-Utils module
%%% @end
%%%-------------------------------------------------------------------
-module(aos_handlers_utils_eunit_test).
-include_lib("eunit/include/eunit.hrl").

%% Test fixture setup
setup() ->
    L0 = luerl:init(),
    
    % Get the path to source files
    BasePath = filename:join([code:lib_dir(aos_test_suite), "..", "..", "..", "..", ".."]),
    UtilsPath = filename:join([BasePath, "src", "utils.lua"]),
    HandlersUtilsPath = filename:join([BasePath, "src", "handlers-utils.lua"]),
    
    % Read both modules
    {ok, UtilsBinary} = file:read_file(UtilsPath),
    UtilsCode = binary_to_list(UtilsBinary),
    
    {ok, HandlersUtilsBinary} = file:read_file(HandlersUtilsPath),
    HandlersUtilsCode = binary_to_list(HandlersUtilsBinary),
    
    % Load utils module first
    WrappedUtils = "do\n" ++
                   "  local module = function()\n" ++
                   UtilsCode ++ "\n" ++
                   "  end\n" ++
                   "  _G.package.loaded['.utils'] = module()\n" ++
                   "end",
    
    {_, L1} = luerl:do(WrappedUtils, L0),
    
    % Then load handlers-utils module
    WrappedHandlersUtils = "do\n" ++
                          "  local module = function()\n" ++
                          HandlersUtilsCode ++ "\n" ++
                          "  end\n" ++
                          "  _G.package.loaded['.handlers-utils'] = module()\n" ++
                          "end",
    
    {_, L2} = luerl:do(WrappedHandlersUtils, L1),
    
    % Create a mock Handlers global with utils reference for hasMatchingTagOf
    MockHandlers = "Handlers = { utils = require('.handlers-utils') }",
    {_, L3} = luerl:do(MockHandlers, L2),
    
    L3.

teardown(_) ->
    ok.

%% Test suite
handlers_utils_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(L) ->
         [
          % Version test
          test_version(L),
          
          % hasMatchingTag tests
          test_has_matching_tag_success(L),
          test_has_matching_tag_failure(L),
          test_has_matching_tag_missing_tag(L),
          
          % hasMatchingTagOf tests
          test_has_matching_tag_of_success(L),
          test_has_matching_tag_of_multiple_values(L),
          test_has_matching_tag_of_no_match(L),
          
          % hasMatchingData tests
          test_has_matching_data_success(L),
          test_has_matching_data_failure(L),
          test_has_matching_data_nil(L),
          
          % reply tests
          test_reply_string(L),
          test_reply_table(L),
          test_reply_with_tags(L),
          
          % continue tests
          test_continue_match(L),
          test_continue_no_match(L),
          test_continue_with_function_pattern(L)
         ]
     end}.

%% Individual test functions

test_version(L) ->
    {"handlers-utils has correct version",
     fun() ->
         {[Version], _} = luerl:do("return require('.handlers-utils')._version", L),
         ?assertEqual(<<"0.0.2">>, Version)
     end}.

%% hasMatchingTag tests

test_has_matching_tag_success(L) ->
    {"hasMatchingTag returns true when tag matches",
     fun() ->
         Code = "local hu = require('.handlers-utils')\n" ++
                "local msg = { Tags = { Action = 'Eval', Type = 'Message' } }\n" ++
                "local checker = hu.hasMatchingTag('Action', 'Eval')\n" ++
                "return checker(msg)",
         {[Result], _} = luerl:do(Code, L),
         ?assert(Result)
     end}.

test_has_matching_tag_failure(L) ->
    {"hasMatchingTag returns false when tag doesn't match",
     fun() ->
         Code = "local hu = require('.handlers-utils')\n" ++
                "local msg = { Tags = { Action = 'Eval', Type = 'Message' } }\n" ++
                "local checker = hu.hasMatchingTag('Action', 'Info')\n" ++
                "return checker(msg)",
         {[Result], _} = luerl:do(Code, L),
         ?assertNot(Result)
     end}.

test_has_matching_tag_missing_tag(L) ->
    {"hasMatchingTag returns false when tag is missing",
     fun() ->
         Code = "local hu = require('.handlers-utils')\n" ++
                "local msg = { Tags = { Type = 'Message' } }\n" ++
                "local checker = hu.hasMatchingTag('Action', 'Eval')\n" ++
                "return checker(msg)",
         {[Result], _} = luerl:do(Code, L),
         ?assertNot(Result)
     end}.

%% hasMatchingTagOf tests

test_has_matching_tag_of_success(L) ->
    {"hasMatchingTagOf returns match when one value matches",
     fun() ->
         Code = "local hu = require('.handlers-utils')\n" ++
                "local msg = { Tags = { Action = 'Eval' } }\n" ++
                "local checker = hu.hasMatchingTagOf('Action', {'Info', 'Eval', 'Run'})\n" ++
                "return checker(msg)",
         {[Result], _} = luerl:do(Code, L),
         % The function should return a truthy value (not 0, false, or "skip")
         ?assertNotEqual(0, Result),
         ?assertNotEqual(false, Result)
     end}.

test_has_matching_tag_of_multiple_values(L) ->
    {"hasMatchingTagOf checks multiple values",
     fun() ->
         Code = "local hu = require('.handlers-utils')\n" ++
                "local msg = { Tags = { Type = 'Query' } }\n" ++
                "local checker = hu.hasMatchingTagOf('Type', {'Message', 'Query', 'Response'})\n" ++
                "return checker(msg)",
         {[Result], _} = luerl:do(Code, L),
         ?assertNotEqual(0, Result)
     end}.

test_has_matching_tag_of_no_match(L) ->
    {"hasMatchingTagOf returns 0 when no values match",
     fun() ->
         Code = "local hu = require('.handlers-utils')\n" ++
                "local msg = { Tags = { Action = 'Delete' } }\n" ++
                "local checker = hu.hasMatchingTagOf('Action', {'Info', 'Eval', 'Run'})\n" ++
                "return checker(msg)",
         {[Result], _} = luerl:do(Code, L),
         ?assertEqual(0, Result)
     end}.

%% hasMatchingData tests

test_has_matching_data_success(L) ->
    {"hasMatchingData returns true when data matches",
     fun() ->
         Code = "local hu = require('.handlers-utils')\n" ++
                "local msg = { Data = 'Hello World' }\n" ++
                "local checker = hu.hasMatchingData('Hello World')\n" ++
                "return checker(msg)",
         {[Result], _} = luerl:do(Code, L),
         ?assert(Result)
     end}.

test_has_matching_data_failure(L) ->
    {"hasMatchingData returns false when data doesn't match",
     fun() ->
         Code = "local hu = require('.handlers-utils')\n" ++
                "local msg = { Data = 'Hello World' }\n" ++
                "local checker = hu.hasMatchingData('Goodbye')\n" ++
                "return checker(msg)",
         {[Result], _} = luerl:do(Code, L),
         ?assertNot(Result)
     end}.

test_has_matching_data_nil(L) ->
    {"hasMatchingData returns false when data is nil",
     fun() ->
         Code = "local hu = require('.handlers-utils')\n" ++
                "local msg = { }\n" ++
                "local checker = hu.hasMatchingData('Hello')\n" ++
                "return checker(msg)",
         {[Result], _} = luerl:do(Code, L),
         ?assertNot(Result)
     end}.

%% reply tests

test_reply_string(L) ->
    {"reply with string creates proper response",
     fun() ->
         Code = "local hu = require('.handlers-utils')\n" ++
                "local replyData = nil\n" ++
                "local msg = {\n" ++
                "  reply = function(data) replyData = data end\n" ++
                "}\n" ++
                "local replyFn = hu.reply('Test Response')\n" ++
                "replyFn(msg)\n" ++
                "return replyData.Data",
         {[Result], _} = luerl:do(Code, L),
         ?assertEqual(<<"Test Response">>, Result)
     end}.

test_reply_table(L) ->
    {"reply with table passes table directly",
     fun() ->
         Code = "local hu = require('.handlers-utils')\n" ++
                "local replyData = nil\n" ++
                "local msg = {\n" ++
                "  reply = function(data) replyData = data end\n" ++
                "}\n" ++
                "local response = { Data = 'Custom', Tags = { Status = 'OK' } }\n" ++
                "local replyFn = hu.reply(response)\n" ++
                "replyFn(msg)\n" ++
                "return replyData.Data, replyData.Tags and replyData.Tags.Status or nil",
         {Results, _} = luerl:do(Code, L),
         [Data, Status] = Results,
         ?assertEqual(<<"Custom">>, Data),
         ?assertEqual(<<"OK">>, Status)
     end}.

test_reply_with_tags(L) ->
    {"reply with complex table structure",
     fun() ->
         Code = "local hu = require('.handlers-utils')\n" ++
                "local replyData = nil\n" ++
                "local msg = {\n" ++
                "  reply = function(data) replyData = data end\n" ++
                "}\n" ++
                "local response = {\n" ++
                "  Data = 'Result',\n" ++
                "  Tags = {\n" ++
                "    Action = 'Response',\n" ++
                "    Status = '200'\n" ++
                "  }\n" ++
                "}\n" ++
                "local replyFn = hu.reply(response)\n" ++
                "replyFn(msg)\n" ++
                "return replyData.Tags.Action, replyData.Tags.Status",
         {Results, _} = luerl:do(Code, L),
         [Action, Status] = Results,
         ?assertEqual(<<"Response">>, Action),
         ?assertEqual(<<"200">>, Status)
     end}.

%% continue tests

test_continue_match(L) ->
    {"continue returns 1 when pattern matches",
     fun() ->
         Code = "local hu = require('.handlers-utils')\n" ++
                "local msg = { Action = 'Eval', Data = 'test' }\n" ++
                "local pattern = { Action = 'Eval' }\n" ++
                "local continueFn = hu.continue(pattern)\n" ++
                "return continueFn(msg)",
         {[Result], _} = luerl:do(Code, L),
         ?assertEqual(1, Result)
     end}.

test_continue_no_match(L) ->
    {"continue returns original result when pattern doesn't match",
     fun() ->
         Code = "local hu = require('.handlers-utils')\n" ++
                "local msg = { Action = 'Info', Data = 'test' }\n" ++
                "local pattern = { Action = 'Eval' }\n" ++
                "local continueFn = hu.continue(pattern)\n" ++
                "local result = continueFn(msg)\n" ++
                "return result == 0 or result == false or result == nil",
         {[Result], _} = luerl:do(Code, L),
         ?assert(Result)
     end}.

test_continue_with_function_pattern(L) ->
    {"continue works with function patterns",
     fun() ->
         Code = "local hu = require('.handlers-utils')\n" ++
                "local msg = { Value = 10 }\n" ++
                "local pattern = function(m) return m.Value > 5 end\n" ++
                "local continueFn = hu.continue(pattern)\n" ++
                "return continueFn(msg)",
         {[Result], _} = luerl:do(Code, L),
         ?assertEqual(1, Result)
     end}.

%% Edge case tests

test_error_handling() ->
    {"Error handling tests",
     {setup,
      fun setup/0,
      fun teardown/1,
      fun(L) ->
          [
           % Test hasMatchingTag with invalid arguments
           {"hasMatchingTag with non-string name should error",
            fun() ->
                Code = "local hu = require('.handlers-utils')\n" ++
                       "local status = pcall(function()\n" ++
                       "  hu.hasMatchingTag(123, 'value')\n" ++
                       "end)\n" ++
                       "return status",
                {[Status], _} = luerl:do(Code, L),
                ?assertNot(Status)
            end},
           
           % Test hasMatchingTagOf with non-table values
           {"hasMatchingTagOf with non-table values should error",
            fun() ->
                Code = "local hu = require('.handlers-utils')\n" ++
                       "local status = pcall(function()\n" ++
                       "  hu.hasMatchingTagOf('Action', 'not-a-table')\n" ++
                       "end)\n" ++
                       "return status",
                {[Status], _} = luerl:do(Code, L),
                ?assertNot(Status)
            end},
           
           % Test hasMatchingData with non-string value
           {"hasMatchingData with non-string value should error",
            fun() ->
                Code = "local hu = require('.handlers-utils')\n" ++
                       "local status = pcall(function()\n" ++
                       "  hu.hasMatchingData(123)\n" ++
                       "end)\n" ++
                       "return status",
                {[Status], _} = luerl:do(Code, L),
                ?assertNot(Status)
            end},
           
           % Test reply with invalid input
           {"reply with invalid input should error",
            fun() ->
                Code = "local hu = require('.handlers-utils')\n" ++
                       "local status = pcall(function()\n" ++
                       "  hu.reply(123)\n" ++
                       "end)\n" ++
                       "return status",
                {[Status], _} = luerl:do(Code, L),
                ?assertNot(Status)
            end}
          ]
      end}}.

%% Integration tests

test_integration() ->
    {"Integration tests with utils",
     {setup,
      fun setup/0,
      fun teardown/1,
      fun(L) ->
          [
           % Test continue with utils.matchesSpec
           {"continue integrates with utils.matchesSpec",
            fun() ->
                Code = "local hu = require('.handlers-utils')\n" ++
                       "local utils = require('.utils')\n" ++
                       "local msg = {\n" ++
                       "  Action = 'Eval',\n" ++
                       "  Tags = { Type = 'Message' },\n" ++
                       "  Data = 'test'\n" ++
                       "}\n" ++
                       "-- Complex pattern using utils features\n" ++
                       "local pattern = {\n" ++
                       "  Action = function(v) return v == 'Eval' or v == 'Run' end,\n" ++
                       "  Tags = { Type = '_' }  -- Wildcard\n" ++
                       "}\n" ++
                       "local continueFn = hu.continue(pattern)\n" ++
                       "return continueFn(msg)",
                {[Result], _} = luerl:do(Code, L),
                ?assertEqual(1, Result)
            end},
           
           % Test message processing pipeline
           {"message processing pipeline",
            fun() ->
                Code = "local hu = require('.handlers-utils')\n" ++
                       "local results = {}\n" ++
                       "local msg = {\n" ++
                       "  Tags = { Action = 'Process', Type = 'Data' },\n" ++
                       "  Data = 'input',\n" ++
                       "  reply = function(data)\n" ++
                       "    table.insert(results, data)\n" ++
                       "  end\n" ++
                       "}\n" ++
                       "-- Check if has right tag\n" ++
                       "local hasAction = hu.hasMatchingTag('Action', 'Process')\n" ++
                       "if hasAction(msg) then\n" ++
                       "  -- Process and reply\n" ++
                       "  local replyFn = hu.reply({ Data = 'Processed', Tags = { Status = 'OK' } })\n" ++
                       "  replyFn(msg)\n" ++
                       "end\n" ++
                       "return #results, results[1] and results[1].Data or nil",
                {Results, _} = luerl:do(Code, L),
                [Count, Data] = Results,
                ?assertEqual(1, Count),
                ?assertEqual(<<"Processed">>, Data)
            end}
          ]
      end}}.