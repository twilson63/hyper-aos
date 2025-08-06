%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Hyper-AOS Team
%%% @copyright (C) 2024, Hyper-AOS
%%% @doc
%%% Integration tests for utils module with AO message processing.
%%% Tests the seamless integration of utils.lua functions with aos.lua
%%% message handling, filtering, and transformation capabilities.
%%% @end
%%% Created : 06 Aug 2024 by Hyper-AOS Team
%%%-------------------------------------------------------------------

-module(aos_utils_integration_test).
-author("Hyper-AOS Team").

-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% Test Descriptions
%%%-------------------------------------------------------------------

%% Test that utils module is properly loaded and integrated with aos.lua
utils_loading_test() ->
  ?_test(
    begin
      State = setup_test_environment(),
      
      % Check that utils is available in global namespace
      {ok, [UtilsExists]} = luerl:eval("return _G.utils ~= nil", State),
      ?assertEqual(true, UtilsExists),
      
      % Check utils version
      {ok, [Version]} = luerl:eval("return _G.utils._version", State),
      ?assertEqual(<<"1.0.0">>, Version),
      
      % Check some key utils functions exist
      {ok, [MapExists]} = luerl:eval("return type(_G.utils.map) == 'function'", State),
      {ok, [FilterExists]} = luerl:eval("return type(_G.utils.filter) == 'function'", State),
      {ok, [FindExists]} = luerl:eval("return type(_G.utils.find) == 'function'", State),
      
      ?assertEqual(true, MapExists),
      ?assertEqual(true, FilterExists),
      ?assertEqual(true, FindExists)
    end).

%% Test utils functions integration with message processing
message_filtering_test() ->
  ?_test(
    begin
      State = setup_test_environment(),
      
      % Create test messages with different authorities
      TestMsg1 = #{
        <<"action">> => <<"test">>,
        <<"from">> => <<"authority1_address_43chars_test_auth_addr1">>,
        <<"data">> => <<"Test message from authority 1">>
      },
      TestMsg2 = #{
        <<"action">> => <<"test">>,
        <<"from">> => <<"authority2_address_43chars_test_auth_addr2">>,
        <<"data">> => <<"Test message from authority 2">>
      },
      TestMsg3 = #{
        <<"action">> => <<"test">>,
        <<"from">> => <<"unknown_address_43chars_test_unknown_addr">>,
        <<"data">> => <<"Test message from unknown sender">>
      },
      
      % Add messages to inbox
      Assignment1 = #{<<"body">> => TestMsg1},
      Assignment2 = #{<<"body">> => TestMsg2},
      Assignment3 = #{<<"body">> => TestMsg3},
      
      {ok, State2} = luerl:call_function([compute], [#{}, Assignment1], State),
      {ok, State3} = luerl:call_function([compute], [#{}, Assignment2], State2),
      {ok, State4} = luerl:call_function([compute], [#{}, Assignment3], State3),
      
      % Test utils filtering functionality
      FilterCode = "
        local filtered = utils.filter(utils.propEq('from', 'authority1_address_43chars_test_auth_addr1'), Inbox)
        return #filtered
      ",
      {ok, [FilterCount]} = luerl:eval(FilterCode, State4),
      ?assertEqual(1.0, FilterCount)
    end).

%% Test utils pattern matching with AO message specs
pattern_matching_test() ->
  ?_test(
    begin
      State = setup_test_environment(),
      
      % Create test message with specific pattern
      TestMsg = #{
        <<"action">> => <<"eval">>,
        <<"from">> => <<"owner_address_43chars_test_owner_address1">>,
        <<"data">> => <<"print('hello utils')">>
      },
      
      % Test pattern matching
      Code = "
        local msg = {
          action = 'eval',
          from = 'owner_address_43chars_test_owner_address1',
          data = \"print('hello utils')\"
        }
        local spec = {action = 'eval', from = 'owner_address_43chars_test_owner_address1'}
        return utils.matchesSpec(msg, spec)
      ",
      {ok, [Matches]} = luerl:eval(Code, State),
      ?assertEqual(true, Matches)
    end).

%% Test curried function usage in message processing
curry_functionality_test() ->
  ?_test(
    begin
      State = setup_test_environment(),
      
      % Test curry function with message transformation
      Code = "
        local addPrefix = utils.curry(function(prefix, text)
          return prefix .. ': ' .. text
        end, 2)
        
        local addTimestamp = addPrefix('TIMESTAMPED')
        local messages = {'hello', 'world', 'utils'}
        local timestamped = utils.map(addTimestamp, messages)
        
        return timestamped[1], timestamped[2], timestamped[3]
      ",
      {ok, [Result1, Result2, Result3]} = luerl:eval(Code, State),
      ?assertEqual(<<"TIMESTAMPED: hello">>, Result1),
      ?assertEqual(<<"TIMESTAMPED: world">>, Result2),
      ?assertEqual(<<"TIMESTAMPED: utils">>, Result3)
    end).

%% Test functional composition in message processing pipeline
composition_test() ->
  ?_test(
    begin
      State = setup_test_environment(),
      
      % Test function composition for message processing
      Code = "
        local extractAction = utils.prop('action')
        local toUpperCase = function(str) return string.upper(str) end
        local addPrefix = function(str) return 'ACTION_' .. str end
        
        local processAction = utils.compose(addPrefix, toUpperCase, extractAction)
        
        local testMsg = {action = 'eval', from = 'test', data = 'test'}
        return processAction(testMsg)
      ",
      {ok, [Result]} = luerl:eval(Code, State),
      ?assertEqual(<<"ACTION_EVAL">>, Result)
    end).

%% Test demo-utils action integration
demo_utils_action_test() ->
  ?_test(
    begin
      State = setup_test_environment(),
      
      % Create a demo-utils message
      DemoMsg = #{
        <<"action">> => <<"demo-utils">>,
        <<"from">> => <<"test_sender_43chars_demo_utils_test_addr">>,
        <<"data">> => <<"Testing utils integration">>
      },
      
      Assignment = #{<<"body">> => DemoMsg},
      {ok, _NewState} = luerl:call_function([compute], [#{}, Assignment], State),
      
      % The demo should execute without errors and show utils functionality
      ?assert(true) % If we get here, the demo executed successfully
    end).

%% Test array manipulation functions
array_manipulation_test() ->
  ?_test(
    begin
      State = setup_test_environment(),
      
      % Test array functions
      Code = "
        local arr1 = {1, 2, 3}
        local arr2 = {4, 5, 6}
        
        -- Test concatenation
        local concatenated = utils.concat(arr1, arr2)
        local concat_length = #concatenated
        
        -- Test reversal
        local reversed = utils.reverse(arr1)
        
        -- Test includes
        local includes_2 = utils.includes(2, arr1)
        local includes_7 = utils.includes(7, arr1)
        
        return concat_length, reversed[1], reversed[3], includes_2, includes_7
      ",
      {ok, [ConcatLength, FirstReversed, LastReversed, Includes2, Includes7]} = luerl:eval(Code, State),
      ?assertEqual(6.0, ConcatLength),
      ?assertEqual(3.0, FirstReversed),
      ?assertEqual(1.0, LastReversed),
      ?assertEqual(true, Includes2),
      ?assertEqual(false, Includes7)
    end).

%% Test utils integration with authority validation
authority_validation_test() ->
  ?_test(
    begin
      State = setup_test_environment_with_authorities(),
      
      % Create messages from different sources
      AuthorizedMsg = #{
        <<"action">> => <<"test">>,
        <<"from">> => <<"authority1_address_43chars_test_auth_addr1">>,
        <<"from-process">> => <<"authority1_address_43chars_test_auth_addr1">>,
        <<"data">> => <<"Authorized message">>
      },
      
      UnauthorizedMsg = #{
        <<"action">> => <<"test">>,
        <<"from">> => <<"unknown_address_43chars_test_unknown_addr">>,
        <<"data">> => <<"Unauthorized message">>
      },
      
      % Test filtering by authority status
      Code = "
        local authorized = {
          from = 'authority1_address_43chars_test_auth_addr1',
          ['from-process'] = 'authority1_address_43chars_test_auth_addr1'
        }
        local unauthorized = {
          from = 'unknown_address_43chars_test_unknown_addr'
        }
        
        local messages = {authorized, unauthorized}
        
        -- Filter messages that have matching from and from-process
        local trusted_messages = utils.filter(function(msg)
          return msg.from == msg['from-process']
        end, messages)
        
        return #trusted_messages, trusted_messages[1].from
      ",
      {ok, [TrustedCount, TrustedFrom]} = luerl:eval(Code, State),
      ?assertEqual(1.0, TrustedCount),
      ?assertEqual(<<"authority1_address_43chars_test_auth_addr1">>, TrustedFrom)
    end).

%%%-------------------------------------------------------------------
%%% Helper Functions
%%%-------------------------------------------------------------------

%% Load aos.lua code with utils integration
load_aos_with_utils() ->
  {ok, AosCode} = file:read_file("../aos.lua"),
  AosCode.

%% Setup basic test environment with utils pre-loaded
setup_test_environment() ->
  % First load utils
  {ok, UtilsCode} = file:read_file("../utils.lua"),
  {ok, State1} = luerl:eval(UtilsCode, luerl:init()),
  
  % Then load aos
  {ok, AosCode} = file:read_file("../aos.lua"),
  {ok, State2} = luerl:eval(AosCode, State1),
  State2.

%% Setup test environment with authorities
setup_test_environment_with_authorities() ->
  State = setup_test_environment(),
  
  % Initialize process with authorities
  ProcessMsg = #{
    <<"type">> => <<"process">>,
    <<"authority">> => <<"authority1_address_43chars_test_auth_addr1,authority2_address_43chars_test_auth_addr2">>,
    <<"commitments">> => #{
      <<"process_id_43chars_test_process_identifier">> => #{
        <<"type">> => <<"RSA-PSS-512">>,
        <<"committer">> => <<"owner_address_43chars_test_owner_address1">>,
        <<"commit">> => <<"test_commit_hash">>
      }
    }
  },
  
  Assignment = #{<<"body">> => ProcessMsg},
  {ok, FinalState} = luerl:call_function([compute], [#{}, Assignment], State),
  FinalState.

%%%-------------------------------------------------------------------
%%% Test Suite
%%%-------------------------------------------------------------------

utils_integration_test_() ->
  {foreach,
   fun() -> ok end,
   fun(_) -> ok end,
   [
    {"Utils module loading", utils_loading_test()},
    {"Message filtering with utils", message_filtering_test()},
    {"Pattern matching with specs", pattern_matching_test()},
    {"Curry functionality", curry_functionality_test()},
    {"Function composition", composition_test()},
    {"Demo utils action", demo_utils_action_test()},
    {"Array manipulation", array_manipulation_test()},
    {"Authority validation", authority_validation_test()}
   ]}.