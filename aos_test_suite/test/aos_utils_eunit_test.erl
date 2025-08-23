%%%-------------------------------------------------------------------
%%% @doc EUnit tests for AOS Utils module
%%% @end
%%%-------------------------------------------------------------------
-module(aos_utils_eunit_test).
-include_lib("eunit/include/eunit.hrl").

%% Test fixture setup
setup() ->
    L0 = luerl:init(),
    
    % Read and load the utils module - use absolute path from project root
    % When tests run via rebar3, the working directory is _build/default/lib/aos_test_suite
    UtilsPath = filename:join([code:lib_dir(aos_test_suite), "..", "..", "..", "..", "..", "src", "utils.lua"]),
    {ok, UtilsBinary} = file:read_file(UtilsPath),
    UtilsCode = binary_to_list(UtilsBinary),
    
    % Wrap utils.lua in module pattern to register it
    WrappedUtils = "do\n" ++
                   "  local module = function()\n" ++
                   UtilsCode ++ "\n" ++
                   "  end\n" ++
                   "  _G.package.loaded['.utils'] = module()\n" ++
                   "end",
    
    L1 = case luerl:do(WrappedUtils, L0) of
        {ok, _, NewState1} -> 
            NewState1;
        {error, Reason1} ->
            error({failed_to_load_module, utils, Reason1})
    end,
    L1.

teardown(_) ->
    ok.

%% Test suite
utils_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(L) ->
         [
          % Version test
          test_version(L),
          
          % matchesPattern tests
          test_matches_pattern_wildcard(L),
          test_matches_pattern_exact_string(L),
          test_matches_pattern_regex(L),
          test_matches_pattern_function(L),
          test_matches_pattern_table(L),
          test_matches_pattern_nil(L),
          
          % matchesSpec tests
          test_matches_spec_table(L),
          test_matches_spec_function(L),
          test_matches_spec_string_action(L),
          
          % curry tests
          test_curry_basic(L),
          test_curry_partial(L),
          
          % Array operation tests
          test_concat(L),
          test_reduce(L),
          test_map(L),
          test_filter(L),
          test_find(L),
          test_reverse(L),
          test_includes(L),
          
          % Property tests
          test_prop_eq(L),
          test_prop(L),
          
          % compose test
          test_compose(L),
          
          % keys and values tests
          test_keys(L),
          test_values(L),
          
          % Tab test
          test_tab(L)
         ]
     end}.

%% Individual test functions

test_version(L) ->
    {"utils has correct version",
     fun() ->
         {ok, [Version], _} = luerl:do("return require('.utils')._version", L),
         ?assertEqual(<<"0.0.5">>, Version)
     end}.

%% matchesPattern tests

test_matches_pattern_wildcard(L) ->
    {"matchesPattern with wildcard '_'",
     fun() ->
         Code = "local utils = require('.utils')\n" ++
                "return utils.matchesPattern('_', 'anything', {})",
         {ok, [Result], _} = luerl:do(Code, L),
         ?assert(Result)
     end}.

test_matches_pattern_exact_string(L) ->
    {"matchesPattern with exact string match",
     fun() ->
         Code1 = "local utils = require('.utils')\n" ++
                 "return utils.matchesPattern('hello', 'hello', {})",
         {ok, [Result1], _} = luerl:do(Code1, L),
         ?assert(Result1),
         
         Code2 = "local utils = require('.utils')\n" ++
                 "return utils.matchesPattern('hello', 'world', {})",
         {ok, [Result2], _} = luerl:do(Code2, L),
         ?assertNot(Result2)
     end}.

test_matches_pattern_regex(L) ->
    {"matchesPattern with regex pattern",
     fun() ->
         Code1 = "local utils = require('.utils')\n" ++
                 "return utils.matchesPattern('^test.*', 'testing123', {})",
         {ok, [Result1], _} = luerl:do(Code1, L),
         ?assert(Result1),
         
         Code2 = "local utils = require('.utils')\n" ++
                 "return utils.matchesPattern('%d+', 'abc123def', {})",
         {ok, [Result2], _} = luerl:do(Code2, L),
         ?assert(Result2)
     end}.

test_matches_pattern_function(L) ->
    {"matchesPattern with function",
     fun() ->
         Code = "local utils = require('.utils')\n" ++
                "local pattern = function(value, msg) return value == 'test' end\n" ++
                "return utils.matchesPattern(pattern, 'test', {})",
         {ok, [Result], _} = luerl:do(Code, L),
         ?assert(Result)
     end}.

test_matches_pattern_table(L) ->
    {"matchesPattern with table of patterns",
     fun() ->
         Code = "local utils = require('.utils')\n" ++
                "local patterns = {'hello', 'world', 'test'}\n" ++
                "return utils.matchesPattern(patterns, 'world', {})",
         {ok, [Result], _} = luerl:do(Code, L),
         ?assert(Result)
     end}.

test_matches_pattern_nil(L) ->
    {"matchesPattern with nil pattern",
     fun() ->
         Code = "local utils = require('.utils')\n" ++
                "return utils.matchesPattern(nil, 'anything', {})",
         {ok, [Result], _} = luerl:do(Code, L),
         ?assertNot(Result)
     end}.

%% matchesSpec tests

test_matches_spec_table(L) ->
    {"matchesSpec with table spec",
     fun() ->
         Code = "local utils = require('.utils')\n" ++
                "local msg = {action = 'eval', type = 'compute'}\n" ++
                "local spec = {action = 'eval', type = '_'}\n" ++
                "return utils.matchesSpec(msg, spec)",
         {ok, [Result], _} = luerl:do(Code, L),
         ?assert(Result)
     end}.

test_matches_spec_function(L) ->
    {"matchesSpec with function spec",
     fun() ->
         Code = "local utils = require('.utils')\n" ++
                "local msg = {value = 10}\n" ++
                "local spec = function(m) return m.value > 5 end\n" ++
                "return utils.matchesSpec(msg, spec)",
         {ok, [Result], _} = luerl:do(Code, L),
         ?assert(Result)
     end}.

test_matches_spec_string_action(L) ->
    {"matchesSpec with string spec for action",
     fun() ->
         Code = "local utils = require('.utils')\n" ++
                "local msg = {action = 'eval'}\n" ++
                "return utils.matchesSpec(msg, 'eval')",
         {ok, [Result], _} = luerl:do(Code, L),
         ?assert(Result)
     end}.

%% curry tests

test_curry_basic(L) ->
    {"curry basic functionality",
     fun() ->
         Code = "local utils = require('.utils')\n" ++
                "local add = function(a, b) return a + b end\n" ++
                "local curriedAdd = utils.curry(add, 2)\n" ++
                "return curriedAdd(5)(3)",
         {ok, [Result], _} = luerl:do(Code, L),
         ?assertEqual(8, Result)
     end}.

test_curry_partial(L) ->
    {"curry partial application",
     fun() ->
         Code = "local utils = require('.utils')\n" ++
                "local add3 = function(a, b, c) return a + b + c end\n" ++
                "local curried = utils.curry(add3, 3)\n" ++
                "local add5 = curried(5)\n" ++
                "return add5(2)(3)",
         {ok, [Result], _} = luerl:do(Code, L),
         ?assertEqual(10, Result)
     end}.

%% Array operation tests

test_concat(L) ->
    {"concat arrays",
     fun() ->
         Code = "local utils = require('.utils')\n" ++
                "local a = {1, 2, 3}\n" ++
                "local b = {4, 5, 6}\n" ++
                "local result = utils.concat(a)(b)\n" ++
                "return result[1], result[3], result[6], #result",
         {ok, Results, _} = luerl:do(Code, L),
         ?assertEqual([1, 3, 6, 6], Results)
     end}.

test_reduce(L) ->
    {"reduce array with sum",
     fun() ->
         Code = "local utils = require('.utils')\n" ++
                "local arr = {1, 2, 3, 4}\n" ++
                "local sum = function(acc, v) return acc + v end\n" ++
                "return utils.reduce(sum)(0)(arr)",
         {ok, [Result], _} = luerl:do(Code, L),
         ?assertEqual(10, Result)
     end}.

test_map(L) ->
    {"map over array",
     fun() ->
         Code = "local utils = require('.utils')\n" ++
                "local arr = {1, 2, 3}\n" ++
                "local double = function(x) return x * 2 end\n" ++
                "local result = utils.map(double)(arr)\n" ++
                "return result[1], result[2], result[3]",
         {ok, Results, _} = luerl:do(Code, L),
         ?assertEqual([2, 4, 6], Results)
     end}.

test_filter(L) ->
    {"filter array",
     fun() ->
         Code = "local utils = require('.utils')\n" ++
                "local arr = {1, 2, 3, 4, 5}\n" ++
                "local isEven = function(x) return x % 2 == 0 end\n" ++
                "local result = utils.filter(isEven)(arr)\n" ++
                "return result[1], result[2], #result",
         {ok, Results, _} = luerl:do(Code, L),
         ?assertEqual([2, 4, 2], Results)
     end}.

test_find(L) ->
    {"find element in array",
     fun() ->
         Code = "local utils = require('.utils')\n" ++
                "local arr = {1, 2, 3, 4, 5}\n" ++
                "local isGT3 = function(x) return x > 3 end\n" ++
                "return utils.find(isGT3)(arr)",
         {ok, [Result], _} = luerl:do(Code, L),
         ?assertEqual(4, Result)
     end}.

test_reverse(L) ->
    {"reverse array",
     fun() ->
         Code = "local utils = require('.utils')\n" ++
                "local arr = {1, 2, 3, 4}\n" ++
                "local result = utils.reverse(arr)\n" ++
                "return result[1], result[2], result[3], result[4]",
         {ok, Results, _} = luerl:do(Code, L),
         ?assertEqual([4, 3, 2, 1], Results)
     end}.

test_includes(L) ->
    {"includes value in array",
     fun() ->
         Code1 = "local utils = require('.utils')\n" ++
                 "local arr = {1, 2, 3, 4}\n" ++
                 "return utils.includes(3)(arr)",
         {ok, [Result1], _} = luerl:do(Code1, L),
         ?assert(Result1),
         
         Code2 = "local utils = require('.utils')\n" ++
                 "local arr = {1, 2, 3, 4}\n" ++
                 "return utils.includes(5)(arr)",
         {ok, [Result2], _} = luerl:do(Code2, L),
         ?assertNot(Result2)
     end}.

%% Property tests

test_prop_eq(L) ->
    {"propEq checks property equality",
     fun() ->
         Code1 = "local utils = require('.utils')\n" ++
                 "local obj = {name = 'Lua', version = '5.4'}\n" ++
                 "return utils.propEq('name')('Lua')(obj)",
         {ok, [Result1], _} = luerl:do(Code1, L),
         ?assert(Result1),
         
         Code2 = "local utils = require('.utils')\n" ++
                 "local obj = {name = 'Lua', version = '5.4'}\n" ++
                 "return utils.propEq('name')('Python')(obj)",
         {ok, [Result2], _} = luerl:do(Code2, L),
         ?assertNot(Result2)
     end}.

test_prop(L) ->
    {"prop retrieves property value",
     fun() ->
         Code = "local utils = require('.utils')\n" ++
                "local obj = {name = 'Lua', version = '5.4'}\n" ++
                "return utils.prop('name')(obj)",
         {ok, [Result], _} = luerl:do(Code, L),
         ?assertEqual(<<"Lua">>, Result)
     end}.

%% compose test

test_compose(L) ->
    {"compose functions",
     fun() ->
         Code = "local utils = require('.utils')\n" ++
                "local add1 = function(x) return x + 1 end\n" ++
                "local mul2 = function(x) return x * 2 end\n" ++
                "local composed = utils.compose(add1)(mul2)\n" ++
                "return composed(3)",  % Should be (3 * 2) + 1 = 7
         {ok, [Result], _} = luerl:do(Code, L),
         ?assertEqual(7, Result)
     end}.

%% keys and values tests

test_keys(L) ->
    {"keys returns table keys",
     fun() ->
         Code = "local utils = require('.utils')\n" ++
                "local obj = {name = 'Lua', age = 30}\n" ++
                "local ks = utils.keys(obj)\n" ++
                "table.sort(ks)\n" ++  % Sort for consistent ordering
                "return ks[1], ks[2], #ks",
         {ok, Results, _} = luerl:do(Code, L),
         [K1, K2, Len] = Results,
         ?assertEqual(2, Len),
         Keys = lists:sort([K1, K2]),
         ?assertEqual([<<"age">>, <<"name">>], Keys)
     end}.

test_values(L) ->
    {"values returns table values",
     fun() ->
         Code = "local utils = require('.utils')\n" ++
                "local obj = {name = 'Lua', age = 30}\n" ++
                "local vals = utils.values(obj)\n" ++
                "table.sort(vals, function(a,b) " ++
                "  if type(a) == 'string' and type(b) == 'number' then return true end\n" ++
                "  if type(a) == 'number' and type(b) == 'string' then return false end\n" ++
                "  return tostring(a) < tostring(b)\n" ++
                "end)\n" ++
                "return vals[1], vals[2], #vals",
         {ok, Results, _} = luerl:do(Code, L),
         ?assertEqual(3, length(Results)),
         [V1, V2, Len] = Results,
         ?assertEqual(2, Len),
         % Values should be 'Lua' and 30 in some order
         ?assert((V1 =:= <<"Lua">> andalso V2 =:= 30) orelse 
                 (V1 =:= 30 andalso V2 =:= <<"Lua">>))
     end}.

%% Tab test

test_tab(L) ->
    {"Tab converts tags to table",
     fun() ->
         Code = "local utils = require('.utils')\n" ++
                "local msg = {\n" ++
                "  Tags = {\n" ++
                "    {name = 'Action', value = 'Eval'},\n" ++
                "    {name = 'Type', value = 'Message'},\n" ++
                "    {name = 'From', value = 'Process123'}\n" ++
                "  }\n" ++
                "}\n" ++
                "local result = utils.Tab(msg)\n" ++
                "return result.Action, result.Type, result.From",
         {ok, Results, _} = luerl:do(Code, L),
         ?assertEqual([<<"Eval">>, <<"Message">>, <<"Process123">>], Results)
     end}.

%% Additional edge case tests

test_error_handling() ->
    {"Error handling tests",
     {setup,
      fun setup/0,
      fun teardown/1,
      fun(L) ->
          [
           % Test concat with non-arrays should error
           {"concat with non-array should fail",
            fun() ->
                Code = "local utils = require('.utils')\n" ++
                       "local status, err = pcall(function()\n" ++
                       "  return utils.concat({a = 1})({b = 2})\n" ++
                       "end)\n" ++
                       "return status, err",
                {ok, [Status, _Error], _} = luerl:do(Code, L),
                ?assertNot(Status)
            end},
           
           % Test reduce with non-function should error
           {"reduce with non-function should fail",
            fun() ->
                Code = "local utils = require('.utils')\n" ++
                       "local status, err = pcall(function()\n" ++
                       "  return utils.reduce('not-a-function')(0)({1,2,3})\n" ++
                       "end)\n" ++
                       "return status",
                {ok, [Status], _} = luerl:do(Code, L),
                ?assertNot(Status)
            end},
           
           % Test map with non-array should error
           {"map with non-array should fail",
            fun() ->
                Code = "local utils = require('.utils')\n" ++
                       "local status = pcall(function()\n" ++
                       "  return utils.map(function(x) return x end)({a = 1})\n" ++
                       "end)\n" ++
                       "return status",
                {ok, [Status], _} = luerl:do(Code, L),
                ?assertNot(Status)
            end}
          ]
      end}}.

%% Complex integration tests

test_complex_operations() ->
    {"Complex operation tests",
     {setup,
      fun setup/0,
      fun teardown/1,
      fun(L) ->
          [
           % Chain multiple operations
           {"chain map, filter, reduce",
            fun() ->
                Code = "local utils = require('.utils')\n" ++
                       "local arr = {1, 2, 3, 4, 5}\n" ++
                       "local doubled = utils.map(function(x) return x * 2 end)(arr)\n" ++
                       "local evens = utils.filter(function(x) return x > 5 end)(doubled)\n" ++
                       "local sum = utils.reduce(function(a,b) return a + b end)(0)(evens)\n" ++
                       "return sum",
                {ok, [Result], _} = luerl:do(Code, L),
                ?assertEqual(18, Result)  % (3*2 + 4*2 + 5*2) = 6+8+10 = 24, filtered >5 = 6+8+10 = 24
            end},
           
           % Complex matchesSpec
           {"complex matchesSpec with nested body",
            fun() ->
                Code = "local utils = require('.utils')\n" ++
                       "local msg = {\n" ++
                       "  action = 'transfer',\n" ++
                       "  body = {\n" ++
                       "    recipient = 'addr123',\n" ++
                       "    amount = '1000'\n" ++
                       "  }\n" ++
                       "}\n" ++
                       "local spec = {\n" ++
                       "  action = 'transfer',\n" ++
                       "  recipient = 'addr123'\n" ++
                       "}\n" ++
                       "return utils.matchesSpec(msg, spec)",
                {ok, [Result], _} = luerl:do(Code, L),
                ?assert(Result)
            end}
          ]
      end}}.