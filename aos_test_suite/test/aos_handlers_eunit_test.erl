%%%-------------------------------------------------------------------
%%% @doc EUnit tests for AOS Handlers module
%%% @end
%%%-------------------------------------------------------------------
-module(aos_handlers_eunit_test).
-include_lib("eunit/include/eunit.hrl").

%% Test fixture setup
setup() ->
    L0 = luerl:init(),
    
    % Get the path to source files
    BasePath = filename:join([code:lib_dir(aos_test_suite), "..", "..", "..", "..", ".."]),
    UtilsPath = filename:join([BasePath, "src", "utils.lua"]),
    HandlersUtilsPath = filename:join([BasePath, "src", "handlers-utils.lua"]),
    HandlersPath = filename:join([BasePath, "src", "handlers.lua"]),
    
    % Read all required modules
    {ok, UtilsBinary} = file:read_file(UtilsPath),
    UtilsCode = binary_to_list(UtilsBinary),
    
    {ok, HandlersUtilsBinary} = file:read_file(HandlersUtilsPath),
    HandlersUtilsCode = binary_to_list(HandlersUtilsBinary),
    
    {ok, HandlersBinary} = file:read_file(HandlersPath),
    HandlersCode = binary_to_list(HandlersBinary),
    
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
    
    % Finally load handlers module
    WrappedHandlers = "do\n" ++
                      "  local module = function()\n" ++
                      HandlersCode ++ "\n" ++
                      "  end\n" ++
                      "  _G.package.loaded['.handlers'] = module()\n" ++
                      "  _G.Handlers = module()  -- Also set as global\n" ++
                      "end",
    
    {_, L3} = luerl:do(WrappedHandlers, L2),
    
    L3.

teardown(_) ->
    ok.

%% Test suite
handlers_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(L) ->
         [
          % Version test
          test_version(L),
          
          % Basic handler operations
          test_add_handler(L),
          test_append_handler(L),
          test_prepend_handler(L),
          test_remove_handler(L),
          test_update_existing_handler(L),
          
          % Pattern matching tests
          test_string_pattern(L),
          test_table_pattern(L),
          test_function_pattern(L),
          
          % Handler evaluation
          test_evaluate_simple(L),
          test_evaluate_continue(L),
          test_evaluate_break(L),
          test_evaluate_skip(L),
          
          % Once handlers
          test_once_handler(L),
          test_once_handler_named(L),
          
          % MaxRuns functionality
          test_maxruns(L),
          test_maxruns_infinite(L),
          
          % Before/After positioning
          test_before_handler(L),
          test_after_handler(L),
          
          % Generate resolver
          test_generate_resolver_function(L),
          test_generate_resolver_table(L)
          
          % Default handler
          %% test_default_handler(L)  % Commented out - using Inbox insertion instead
         ]
     end}.

%% Individual test functions

test_version(L) ->
    {"handlers has correct version",
     fun() ->
         {[Version], _} = luerl:do("return require('.handlers')._version", L),
         ?assertEqual(<<"0.0.5">>, Version)
     end}.

%% Basic handler operations tests

test_add_handler(L) ->
    {"add handler to list",
     fun() ->
         Code = "local h = require('.handlers')\n" ++
                "h.list = {}  -- Clear list\n" ++
                "local count = h.add('test', { Action = 'Eval' }, function(msg) return 'handled' end)\n" ++
                "return count, #h.list, h.list[1].name",
         {Results, _} = luerl:do(Code, L),
         [Count, ListSize, Name] = Results,
         ?assertEqual(1, Count),
         ?assertEqual(1, ListSize),
         ?assertEqual(<<"test">>, Name)
     end}.

test_append_handler(L) ->
    {"append handler to end of list",
     fun() ->
         Code = "local h = require('.handlers')\n" ++
                "h.list = {}  -- Clear list\n" ++
                "h.add('first', 'Action1', function(msg) return end)\n" ++
                "h.append('second', 'Action2', function(msg) return end)\n" ++
                "return #h.list, h.list[1].name, h.list[2].name",
         {Results, _} = luerl:do(Code, L),
         [Count, First, Second] = Results,
         ?assertEqual(2, Count),
         ?assertEqual(<<"first">>, First),
         ?assertEqual(<<"second">>, Second)
     end}.

test_prepend_handler(L) ->
    {"prepend handler to beginning of list",
     fun() ->
         Code = "local h = require('.handlers')\n" ++
                "h.list = {}  -- Clear list\n" ++
                "h.add('first', 'Action1', function(msg) return end)\n" ++
                "h.prepend('second', 'Action2', function(msg) return end)\n" ++
                "return #h.list, h.list[1].name, h.list[2].name",
         {Results, _} = luerl:do(Code, L),
         [Count, First, Second] = Results,
         ?assertEqual(2, Count),
         ?assertEqual(<<"second">>, First),
         ?assertEqual(<<"first">>, Second)
     end}.

test_remove_handler(L) ->
    {"remove handler from list",
     fun() ->
         Code = "local h = require('.handlers')\n" ++
                "h.list = {}  -- Clear list\n" ++
                "h.add('test1', 'Action1', function(msg) return end)\n" ++
                "h.add('test2', 'Action2', function(msg) return end)\n" ++
                "h.add('test3', 'Action3', function(msg) return end)\n" ++
                "local before = #h.list\n" ++
                "h.remove('test2')\n" ++
                "local after = #h.list\n" ++
                "local remaining = {}\n" ++
                "for i, handler in ipairs(h.list) do\n" ++
                "  table.insert(remaining, handler.name)\n" ++
                "end\n" ++
                "return before, after, remaining[1], remaining[2]",
         {Results, _} = luerl:do(Code, L),
         [Before, After, Name1, Name2] = Results,
         ?assertEqual(3, Before),
         ?assertEqual(2, After),
         ?assertEqual(<<"test1">>, Name1),
         ?assertEqual(<<"test3">>, Name2)
     end}.

test_update_existing_handler(L) ->
    {"updating existing handler by name",
     fun() ->
         Code = "local h = require('.handlers')\n" ++
                "h.list = {}  -- Clear list\n" ++
                "local called = 0\n" ++
                "h.add('test', 'Action1', function(msg) called = 1 end)\n" ++
                "h.add('test', 'Action2', function(msg) called = 2 end)\n" ++
                "return #h.list, h.list[1].pattern",
         {Results, _} = luerl:do(Code, L),
         [Count, Pattern] = Results,
         ?assertEqual(1, Count),  % Should still have only one handler
         ?assertEqual(<<"Action2">>, Pattern)  % Pattern should be updated
     end}.

%% Pattern matching tests

test_string_pattern(L) ->
    {"string pattern matching",
     fun() ->
         Code = "local h = require('.handlers')\n" ++
                "local utils = require('.utils')\n" ++
                "h.list = {}  -- Clear list\n" ++
                "local handled = false\n" ++
                "h.add('test', 'Eval', function(msg) handled = true end)\n" ++
                "local msg = { action = 'Eval' }\n" ++
                "local env = {}\n" ++
                "-- Add default handler\n" ++
                "h.add('_default', function() return true end, function() end)\n" ++
                "h.evaluate(msg, env)\n" ++
                "return handled",
         {[Result], _} = luerl:do(Code, L),
         ?assert(Result)
     end}.

test_table_pattern(L) ->
    {"table pattern matching",
     fun() ->
         Code = "local h = require('.handlers')\n" ++
                "h.list = {}  -- Clear list\n" ++
                "local handled = false\n" ++
                "h.add('test', { Action = 'Eval', Type = 'Message' }, function(msg) handled = true end)\n" ++
                "local msg = { Action = 'Eval', Type = 'Message' }\n" ++
                "local env = {}\n" ++
                "-- Add default handler\n" ++
                "h.add('_default', function() return true end, function() end)\n" ++
                "h.evaluate(msg, env)\n" ++
                "return handled",
         {[Result], _} = luerl:do(Code, L),
         ?assert(Result)
     end}.

test_function_pattern(L) ->
    {"function pattern matching",
     fun() ->
         Code = "local h = require('.handlers')\n" ++
                "h.list = {}  -- Clear list\n" ++
                "local handled = false\n" ++
                "local pattern = function(msg) return msg.Value and msg.Value > 5 end\n" ++
                "h.add('test', pattern, function(msg) handled = true end)\n" ++
                "local msg = { Value = 10 }\n" ++
                "local env = {}\n" ++
                "-- Add default handler\n" ++
                "h.add('_default', function() return true end, function() end)\n" ++
                "h.evaluate(msg, env)\n" ++
                "return handled",
         {[Result], _} = luerl:do(Code, L),
         ?assert(Result)
     end}.

%% Handler evaluation tests

test_evaluate_simple(L) ->
    {"evaluate calls matching handler",
     fun() ->
         Code = "local h = require('.handlers')\n" ++
                "h.list = {}  -- Clear list\n" ++
                "local result = nil\n" ++
                "h.add('test', { Action = 'Test' }, function(msg) result = msg.Data end)\n" ++
                "h.add('_default', function() return true end, function() result = 'default' end)\n" ++
                "local msg = { Action = 'Test', Data = 'success' }\n" ++
                "local env = {}\n" ++
                "h.evaluate(msg, env)\n" ++
                "return result",
         {[Result], _} = luerl:do(Code, L),
         ?assertEqual(<<"success">>, Result)
     end}.

test_evaluate_continue(L) ->
    {"evaluate continues with return value 1",
     fun() ->
         Code = "local h = require('.handlers')\n" ++
                "h.list = {}  -- Clear list\n" ++
                "local calls = {}\n" ++
                "-- First handler returns 1 (continue)\n" ++
                "h.add('first', function(msg) return 1 end, function(msg) table.insert(calls, 'first') end)\n" ++
                "-- Second handler returns -1 (break)\n" ++
                "h.add('second', function(msg) return -1 end, function(msg) table.insert(calls, 'second') end)\n" ++
                "h.add('_default', function() return true end, function() table.insert(calls, 'default') end)\n" ++
                "local msg = { Action = 'Test' }\n" ++
                "local env = {}\n" ++
                "h.evaluate(msg, env)\n" ++
                "return #calls, calls[1], calls[2]",
         {Results, _} = luerl:do(Code, L),
         [Count, First, Second] = Results,
         ?assertEqual(2, Count),
         ?assertEqual(<<"first">>, First),
         ?assertEqual(<<"second">>, Second)
     end}.

test_evaluate_break(L) ->
    {"evaluate breaks with return value -1",
     fun() ->
         Code = "local h = require('.handlers')\n" ++
                "h.list = {}  -- Clear list\n" ++
                "local calls = {}\n" ++
                "-- First handler returns -1 (break)\n" ++
                "h.add('first', function(msg) return -1 end, function(msg) table.insert(calls, 'first') end)\n" ++
                "-- Second handler should not be called\n" ++
                "h.add('second', function(msg) return -1 end, function(msg) table.insert(calls, 'second') end)\n" ++
                "h.add('_default', function() return true end, function() table.insert(calls, 'default') end)\n" ++
                "local msg = { Action = 'Test' }\n" ++
                "local env = {}\n" ++
                "h.evaluate(msg, env)\n" ++
                "return #calls, calls[1]",
         {Results, _} = luerl:do(Code, L),
         [Count, First] = Results,
         ?assertEqual(1, Count),
         ?assertEqual(<<"first">>, First)
     end}.

test_evaluate_skip(L) ->
    {"evaluate skips with return value 0",
     fun() ->
         Code = "local h = require('.handlers')\n" ++
                "h.list = {}  -- Clear list\n" ++
                "local calls = {}\n" ++
                "-- First handler returns 0 (skip)\n" ++
                "h.add('first', function(msg) return 0 end, function(msg) table.insert(calls, 'first') end)\n" ++
                "-- Second handler returns -1 (break)\n" ++
                "h.add('second', function(msg) return -1 end, function(msg) table.insert(calls, 'second') end)\n" ++
                "h.add('_default', function() return true end, function() table.insert(calls, 'default') end)\n" ++
                "local msg = { Action = 'Test' }\n" ++
                "local env = {}\n" ++
                "h.evaluate(msg, env)\n" ++
                "return #calls, calls[1]",
         {Results, _} = luerl:do(Code, L),
         [Count, First] = Results,
         ?assertEqual(1, Count),
         ?assertEqual(<<"second">>, First)
     end}.

%% Once handler tests

test_once_handler(L) ->
    {"once handler is removed after execution",
     fun() ->
         Code = "local h = require('.handlers')\n" ++
                "h.list = {}  -- Clear list\n" ++
                "h.onceNonce = 0  -- Reset nonce\n" ++
                "local calls = 0\n" ++
                "h.once(function(msg) return -1 end, function(msg) calls = calls + 1 end)\n" ++
                "h.add('_default', function() return true end, function() end)\n" ++
                "local msg = { Action = 'Test' }\n" ++
                "local env = {}\n" ++
                "local before = #h.list\n" ++
                "h.evaluate(msg, env)\n" ++
                "local after = #h.list\n" ++
                "h.evaluate(msg, env)  -- Should not increment calls\n" ++
                "return before, after, calls",
         {Results, _} = luerl:do(Code, L),
         [Before, After, Calls] = Results,
         ?assertEqual(2, Before),  % once handler + default
         ?assertEqual(1, After),   % only default remains
         ?assertEqual(1, Calls)
     end}.

test_once_handler_named(L) ->
    {"named once handler",
     fun() ->
         Code = "local h = require('.handlers')\n" ++
                "h.list = {}  -- Clear list\n" ++
                "local calls = 0\n" ++
                "h.once('myonce', function(msg) return -1 end, function(msg) calls = calls + 1 end)\n" ++
                "return h.list[1].name, h.list[1].maxRuns",
         {Results, _} = luerl:do(Code, L),
         [Name, MaxRuns] = Results,
         ?assertEqual(<<"myonce">>, Name),
         ?assertEqual(1, MaxRuns)
     end}.

%% MaxRuns tests

test_maxruns(L) ->
    {"handler with maxRuns is removed after limit",
     fun() ->
         Code = "local h = require('.handlers')\n" ++
                "h.list = {}  -- Clear list\n" ++
                "local calls = 0\n" ++
                "h.add('test', function(msg) return -1 end, function(msg) calls = calls + 1 end, 2)\n" ++
                "h.add('_default', function() return true end, function() end)\n" ++
                "local msg = { Action = 'Test' }\n" ++
                "local env = {}\n" ++
                "h.evaluate(msg, env)  -- First call\n" ++
                "local after1 = #h.list\n" ++
                "h.evaluate(msg, env)  -- Second call\n" ++
                "local after2 = #h.list\n" ++
                "h.evaluate(msg, env)  -- Third call (should not increment)\n" ++
                "return after1, after2, calls",
         {Results, _} = luerl:do(Code, L),
         [After1, After2, Calls] = Results,
         ?assertEqual(2, After1),  % test + default
         ?assertEqual(1, After2),  % only default
         ?assertEqual(2, Calls)
     end}.

test_maxruns_infinite(L) ->
    {"handler with 'inf' maxRuns never removed",
     fun() ->
         Code = "local h = require('.handlers')\n" ++
                "h.list = {}  -- Clear list\n" ++
                "local calls = 0\n" ++
                "h.add('test', function(msg) return -1 end, function(msg) calls = calls + 1 end, 'inf')\n" ++
                "h.add('_default', function() return true end, function() end)\n" ++
                "local msg = { Action = 'Test' }\n" ++
                "local env = {}\n" ++
                "for i = 1, 5 do\n" ++
                "  h.evaluate(msg, env)\n" ++
                "end\n" ++
                "return #h.list, calls",
         {Results, _} = luerl:do(Code, L),
         [Count, Calls] = Results,
         ?assertEqual(2, Count),  % Still has both handlers
         ?assertEqual(5, Calls)
     end}.

%% Before/After positioning tests

test_before_handler(L) ->
    {"before inserts handler before specified handler",
     fun() ->
         Code = "local h = require('.handlers')\n" ++
                "h.list = {}  -- Clear list\n" ++
                "h.add('first', 'Action1', function() end)\n" ++
                "h.add('third', 'Action3', function() end)\n" ++
                "h.before('third').add('second', 'Action2', function() end)\n" ++
                "local names = {}\n" ++
                "for i, handler in ipairs(h.list) do\n" ++
                "  table.insert(names, handler.name)\n" ++
                "end\n" ++
                "return names[1], names[2], names[3]",
         {Results, _} = luerl:do(Code, L),
         [First, Second, Third] = Results,
         ?assertEqual(<<"first">>, First),
         ?assertEqual(<<"second">>, Second),
         ?assertEqual(<<"third">>, Third)
     end}.

test_after_handler(L) ->
    {"after inserts handler after specified handler",
     fun() ->
         Code = "local h = require('.handlers')\n" ++
                "h.list = {}  -- Clear list\n" ++
                "h.add('first', 'Action1', function() end)\n" ++
                "h.add('third', 'Action3', function() end)\n" ++
                "h.after('first').add('second', 'Action2', function() end)\n" ++
                "local names = {}\n" ++
                "for i, handler in ipairs(h.list) do\n" ++
                "  table.insert(names, handler.name)\n" ++
                "end\n" ++
                "return names[1], names[2], names[3]",
         {Results, _} = luerl:do(Code, L),
         [First, Second, Third] = Results,
         ?assertEqual(<<"first">>, First),
         ?assertEqual(<<"second">>, Second),
         ?assertEqual(<<"third">>, Third)
     end}.

%% Generate resolver tests

test_generate_resolver_function(L) ->
    {"generateResolver with function",
     fun() ->
         Code = "local h = require('.handlers')\n" ++
                "local result = nil\n" ++
                "local fn = function(msg) result = msg.Data end\n" ++
                "local resolver = h.generateResolver(fn)\n" ++
                "resolver({ Data = 'test' })\n" ++
                "return result",
         {[Result], _} = luerl:do(Code, L),
         ?assertEqual(<<"test">>, Result)
     end}.

test_generate_resolver_table(L) ->
    {"generateResolver with table of patterns",
     fun() ->
         Code = "local h = require('.handlers')\n" ++
                "local result = nil\n" ++
                "local resolverSpec = {\n" ++
                "  [{ Action = 'Info' }] = function(msg) result = 'info' end,\n" ++
                "  [{ Action = 'Eval' }] = function(msg) result = 'eval' end\n" ++
                "}\n" ++
                "local resolver = h.generateResolver(resolverSpec)\n" ++
                "resolver({ Action = 'Eval' })\n" ++
                "return result",
         {[Result], _} = luerl:do(Code, L),
         ?assertEqual(<<"eval">>, Result)
     end}.

%% Default handler test - Commented out, using Inbox insertion instead
%%
%% test_default_handler(L) ->
%%     {"default handler is called when no match",
%%      fun() ->
%%          Code = "local h = require('.handlers')\n" ++
%%                 "h.list = {}  -- Clear list\n" ++
%%                 "local result = nil\n" ++
%%                 "h.add('test', { Action = 'NoMatch' }, function(msg) result = 'test' end)\n" ++
%%                 "h.add('_default', function() return true end, function(msg) result = 'default' end)\n" ++
%%                 "local msg = { Action = 'Different' }\n" ++
%%                 "local env = {}\n" ++
%%                 "h.evaluate(msg, env)\n" ++
%%                 "return result",
%%          {[Result], _} = luerl:do(Code, L),
%%          ?assertEqual(<<"default">>, Result)
%%      end}.

%% Edge case and error handling tests

test_error_handling() ->
    {"Error handling tests",
     {setup,
      fun setup/0,
      fun teardown/1,
      fun(L) ->
          [
           % Test add with invalid name
           {"add with non-string name should error",
            fun() ->
                Code = "local h = require('.handlers')\n" ++
                       "local status = pcall(function()\n" ++
                       "  h.add(123, 'pattern', function() end)\n" ++
                       "end)\n" ++
                       "return status",
                {[Status], _} = luerl:do(Code, L),
                ?assertNot(Status)
            end},
           
           % Test remove with non-string name
           {"remove with non-string name should error",
            fun() ->
                Code = "local h = require('.handlers')\n" ++
                       "local status = pcall(function()\n" ++
                       "  h.remove(123)\n" ++
                       "end)\n" ++
                       "return status",
                {[Status], _} = luerl:do(Code, L),
                ?assertNot(Status)
            end},
           
           % Test evaluate with invalid msg
           {"evaluate with non-table msg should error",
            fun() ->
                Code = "local h = require('.handlers')\n" ++
                       "local status = pcall(function()\n" ++
                       "  h.evaluate('not-a-table', {})\n" ++
                       "end)\n" ++
                       "return status",
                {[Status], _} = luerl:do(Code, L),
                ?assertNot(Status)
            end},
           
           % Test evaluate with invalid env
           {"evaluate with non-table env should error",
            fun() ->
                Code = "local h = require('.handlers')\n" ++
                       "local status = pcall(function()\n" ++
                       "  h.evaluate({}, 'not-a-table')\n" ++
                       "end)\n" ++
                       "return status",
                {[Status], _} = luerl:do(Code, L),
                ?assertNot(Status)
            end}
          ]
      end}}.

%% Complex integration tests

test_complex_scenarios() ->
    {"Complex handler scenarios",
     {setup,
      fun setup/0,
      fun teardown/1,
      fun(L) ->
          [
           % Test handler chain with different return values
           {"handler chain with mixed returns",
            fun() ->
                Code = "local h = require('.handlers')\n" ++
                       "h.list = {}  -- Clear list\n" ++
                       "local log = {}\n" ++
                       "-- Skip handler\n" ++
                       "h.add('skip', function(msg) return msg.Skip and 0 or 1 end,\n" ++
                       "      function(msg) table.insert(log, 'skip') end)\n" ++
                       "-- Continue handler\n" ++
                       "h.add('continue', function(msg) return msg.Continue and 1 or 0 end,\n" ++
                       "      function(msg) table.insert(log, 'continue') end)\n" ++
                       "-- Break handler\n" ++
                       "h.add('break', function(msg) return msg.Break and -1 or 0 end,\n" ++
                       "      function(msg) table.insert(log, 'break') end)\n" ++
                       "h.add('_default', function() return true end, function() table.insert(log, 'default') end)\n" ++
                       "-- Test message that continues then breaks\n" ++
                       "local msg = { Continue = true, Break = true }\n" ++
                       "h.evaluate(msg, {})\n" ++
                       "return #log, log[1], log[2]",
                {Results, _} = luerl:do(Code, L),
                [Count, First, Second] = Results,
                ?assertEqual(2, Count),
                ?assertEqual(<<"continue">>, First),
                ?assertEqual(<<"break">>, Second)
            end},
           
           % Test with handlers-utils integration
           {"integration with handlers-utils",
            fun() ->
                Code = "local h = require('.handlers')\n" ++
                       "local hu = require('.handlers-utils')\n" ++
                       "h.list = {}  -- Clear list\n" ++
                       "local result = nil\n" ++
                       "-- Use hasMatchingTag from handlers-utils\n" ++
                       "h.add('test', hu.hasMatchingTag('Action', 'Process'),\n" ++
                       "      hu.reply('Processed'))\n" ++
                       "h.add('_default', function() return true end, function() end)\n" ++
                       "local msg = {\n" ++
                       "  Tags = { Action = 'Process' },\n" ++
                       "  reply = function(data) result = data.Data end\n" ++
                       "}\n" ++
                       "h.evaluate(msg, {})\n" ++
                       "return result",
                {[Result], _} = luerl:do(Code, L),
                ?assertEqual(<<"Processed">>, Result)
            end},
           
           % Test multiple handlers with same pattern
           {"multiple handlers with same pattern using continue",
            fun() ->
                Code = "local h = require('.handlers')\n" ++
                       "local hu = require('.handlers-utils')\n" ++
                       "h.list = {}  -- Clear list\n" ++
                       "local results = {}\n" ++
                       "-- First handler continues\n" ++
                       "h.add('first', hu.continue({ Action = 'Multi' }),\n" ++
                       "      function(msg) table.insert(results, 'first') end)\n" ++
                       "-- Second handler continues\n" ++
                       "h.add('second', hu.continue({ Action = 'Multi' }),\n" ++
                       "      function(msg) table.insert(results, 'second') end)\n" ++
                       "-- Third handler breaks\n" ++
                       "h.add('third', { Action = 'Multi' },\n" ++
                       "      function(msg) table.insert(results, 'third') end)\n" ++
                       "h.add('_default', function() return true end, function() end)\n" ++
                       "local msg = { Action = 'Multi' }\n" ++
                       "h.evaluate(msg, {})\n" ++
                       "return #results, results[1], results[2], results[3]",
                {Results, _} = luerl:do(Code, L),
                [Count, R1, R2, R3] = Results,
                ?assertEqual(3, Count),
                ?assertEqual(<<"first">>, R1),
                ?assertEqual(<<"second">>, R2),
                ?assertEqual(<<"third">>, R3)
            end}
          ]
      end}}.