%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Hyper-AOS Team
%%% @copyright (C) 2024, Hyper-AOS
%%% @doc
%%% Simple integration test for utils module with AO message processing.
%%% Tests basic utils functionality without complex state management.
%%% @end
%%% Created : 06 Aug 2024 by Hyper-AOS Team
%%%-------------------------------------------------------------------

-module(simple_utils_integration_test).
-author("Hyper-AOS Team").

-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% Test Descriptions
%%%-------------------------------------------------------------------

%% Test that utils module loads correctly
utils_basic_loading_test() ->
  ?_test(
    begin
      {ok, UtilsCode} = file:read_file("utils.lua"),
      {ok, State} = luerl:eval(UtilsCode, luerl:init()),
      
      % Check that utils is available
      {ok, [UtilsExists]} = luerl:eval("return _G.utils ~= nil", State),
      ?assertEqual(true, UtilsExists),
      
      % Check version
      {ok, [Version]} = luerl:eval("return _G.utils._version", State),
      ?assertEqual(<<"1.0.0">>, Version)
    end).

%% Test basic utils functions
utils_basic_functions_test() ->
  ?_test(
    begin
      {ok, UtilsCode} = file:read_file("utils.lua"),
      {ok, State} = luerl:eval(UtilsCode, luerl:init()),
      
      % Test map function
      Code1 = "
        local numbers = {1, 2, 3}
        local doubled = utils.map(function(x) return x * 2 end, numbers)
        return doubled[1], doubled[2], doubled[3]
      ",
      {ok, [Two, Four, Six]} = luerl:eval(Code1, State),
      ?assertEqual(2.0, Two),
      ?assertEqual(4.0, Four),
      ?assertEqual(6.0, Six),
      
      % Test filter function
      Code2 = "
        local numbers = {1, 2, 3, 4, 5}
        local evens = utils.filter(function(x) return x % 2 == 0 end, numbers)
        return #evens, evens[1], evens[2]
      ",
      {ok, [Count, FirstEven, SecondEven]} = luerl:eval(Code2, State),
      ?assertEqual(2.0, Count),
      ?assertEqual(2.0, FirstEven),
      ?assertEqual(4.0, SecondEven)
    end).

%% Test pattern matching functionality
utils_pattern_matching_test() ->
  ?_test(
    begin
      {ok, UtilsCode} = file:read_file("utils.lua"),
      {ok, State} = luerl:eval(UtilsCode, luerl:init()),
      
      % Test matchesPattern function
      Code = "
        -- Test string pattern
        local pattern1 = 'test'
        local result1 = utils.matchesPattern(pattern1, 'test', nil)
        
        -- Test wildcard pattern
        local pattern2 = '_'
        local result2 = utils.matchesPattern(pattern2, 'anything', nil)
        
        -- Test function pattern
        local pattern3 = function(value, msg) return value > 5 end
        local result3 = utils.matchesPattern(pattern3, 10, nil)
        local result4 = utils.matchesPattern(pattern3, 3, nil)
        
        return result1, result2, result3, result4
      ",
      {ok, [StringMatch, WildcardMatch, FunctionMatchTrue, FunctionMatchFalse]} = luerl:eval(Code, State),
      ?assertEqual(true, StringMatch),
      ?assertEqual(true, WildcardMatch),
      ?assertEqual(true, FunctionMatchTrue),
      ?assertEqual(false, FunctionMatchFalse)
    end).

%% Test curry function
utils_curry_test() ->
  ?_test(
    begin
      {ok, UtilsCode} = file:read_file("utils.lua"),
      {ok, State} = luerl:eval(UtilsCode, luerl:init()),
      
      % Test curry functionality
      Code = "
        local add = utils.curry(function(a, b, c) return a + b + c end, 3)
        local addFive = add(5)
        local addFiveAndTen = addFive(10)
        local result = addFiveAndTen(3)
        return result
      ",
      {ok, [Result]} = luerl:eval(Code, State),
      ?assertEqual(18.0, Result)  % 5 + 10 + 3 = 18
    end).

%% Test message-like spec matching (simulated AO message pattern)
utils_message_spec_test() ->
  ?_test(
    begin
      {ok, UtilsCode} = file:read_file("utils.lua"),
      {ok, State} = luerl:eval(UtilsCode, luerl:init()),
      
      % Test spec matching with message-like objects
      Code = "
        local message = {
          Action = 'eval',
          from = 'test_address_43chars_example_sender_addr',
          data = 'print(\"hello world\")'
        }
        
        -- Test string spec (matches Action)
        local spec1 = 'eval'
        local match1 = utils.matchesSpec(message, spec1)
        
        -- Test table spec (all patterns must match)
        local spec2 = {
          Action = 'eval',
          from = 'test_address_43chars_example_sender_addr'
        }
        local match2 = utils.matchesSpec(message, spec2)
        
        -- Test table spec that shouldn't match
        local spec3 = {
          Action = 'send',
          from = 'test_address_43chars_example_sender_addr'
        }
        local match3 = utils.matchesSpec(message, spec3)
        
        return match1, match2, match3
      ",
      {ok, [StringSpecMatch, TableSpecMatch, TableSpecNoMatch]} = luerl:eval(Code, State),
      ?assertEqual(true, StringSpecMatch),
      ?assertEqual(true, TableSpecMatch),
      ?assertEqual(false, TableSpecNoMatch)
    end).

%% Test array utility functions
utils_array_functions_test() ->
  ?_test(
    begin
      {ok, UtilsCode} = file:read_file("utils.lua"),
      {ok, State} = luerl:eval(UtilsCode, luerl:init()),
      
      % Test array utilities
      Code = "
        local arr1 = {1, 2, 3}
        local arr2 = {4, 5, 6}
        
        -- Test concat
        local concatenated = utils.concat(arr1, arr2)
        local concat_len = #concatenated
        
        -- Test reverse
        local reversed = utils.reverse(arr1)
        
        -- Test includes
        local has_2 = utils.includes(2, arr1)
        local has_7 = utils.includes(7, arr1)
        
        -- Test find
        local found = utils.find(function(x) return x > 2 end, arr1)
        
        return concat_len, reversed[1], reversed[3], has_2, has_7, found
      ",
      {ok, [ConcatLen, RevFirst, RevLast, Has2, Has7, Found]} = luerl:eval(Code, State),
      ?assertEqual(6.0, ConcatLen),     % {1,2,3,4,5,6} length
      ?assertEqual(3.0, RevFirst),      % {3,2,1}[1] = 3
      ?assertEqual(1.0, RevLast),       % {3,2,1}[3] = 1
      ?assertEqual(true, Has2),         % arr1 includes 2
      ?assertEqual(false, Has7),        % arr1 doesn't include 7
      ?assertEqual(3.0, Found)          % first element > 2 is 3
    end).

%%%-------------------------------------------------------------------
%%% Test Suite
%%%-------------------------------------------------------------------

utils_integration_test_() ->
  {foreach,
   fun() -> ok end,
   fun(_) -> ok end,
   [
    {"Utils basic loading", utils_basic_loading_test()},
    {"Utils basic functions", utils_basic_functions_test()},
    {"Utils pattern matching", utils_pattern_matching_test()},
    {"Utils curry function", utils_curry_test()},
    {"Utils message spec matching", utils_message_spec_test()},
    {"Utils array functions", utils_array_functions_test()}
   ]}.