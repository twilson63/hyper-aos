-module(aos_metatable_test).
-include_lib("eunit/include/eunit.hrl").

%% Test fixture setup and teardown
setup() ->
    %% Initialize AOS
    LuaState0 = aos_test_helpers:initialize_aos(),
    
    %% Initialize process (this will call compute and set up metatable)
    State = aos_test_helpers:create_base_state(),
    LuaState1 = aos_test_helpers:initialize_process(LuaState0, State),
    
    %% Override ao.resolve with mock for testing
    MockAoCode = "ao.resolve = function(path) return 'Resolved: ' .. path end",
    {_, LuaState2} = luerl:do(MockAoCode, LuaState1),
    
    {LuaState2, State}.

cleanup(_) ->
    ok.

%% Test suite using test generators
metatable_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({LuaState, State}) ->
         [
          test_process_id_detection({LuaState, State}),
          test_ao_resolve_mock({LuaState, State}),
          test_terminal_property_access({LuaState, State}),
          test_nested_property_chain({LuaState, State}),
          test_proxy_error_handling({LuaState, State}),
          test_proxy_force_resolution({LuaState, State}),
          test_multiple_process_ids({LuaState, State}),
          test_integration_with_compute({LuaState, State}),
          test_proxy_cache_behavior({LuaState, State}),
          test_invalid_process_id({LuaState, State})
         ]
     end}.

%% Test process ID detection via proxy creation
test_process_id_detection({LuaState, _State}) ->
    {"Process ID detection via proxy creation",
     fun() ->
         %% Test valid process ID (43 chars, base64url) creates a proxy
         ValidId = <<"AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0">>,
         Code = lists:flatten([
             "local id = '", binary_to_list(ValidId), "'\n",
             "local proxy = _G[id]\n",
             "return type(proxy) == 'table'\n"
         ]),
         {[Result], _} = luerl:do(Code, LuaState),
         ?assertEqual(true, Result),
         
         %% Test invalid process IDs return nil
         InvalidTests = [
             "AR8wJBpKbBS7kZb",  % Too short
             "AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0Extra",  % Too long
             "AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4!@",  % Invalid chars
             "not-a-process-id"  % Obviously wrong
         ],
         
         lists:foreach(fun(InvalidId) ->
             TestCode = lists:flatten([
                 "local id = '", InvalidId, "'\n",
                 "local proxy = _G[id]\n",
                 "return proxy == nil\n"
             ]),
             {[R], _} = luerl:do(TestCode, LuaState),
             ?assertEqual(true, R)
         end, InvalidTests)
     end}.

%% Test ao.resolve mock function
test_ao_resolve_mock({LuaState, _State}) ->
    {"ao.resolve mock function",
     fun() ->
         %% Test direct ao.resolve call
         {[Result], _} = luerl:do(
             "return ao.resolve('test/path/data')", 
             LuaState
         ),
         ?assertEqual(<<"Resolved: test/path/data">>, iolist_to_binary(Result))
     end}.

%% Test terminal property access
test_terminal_property_access({LuaState, _State}) ->
    {"Terminal property access triggers resolution",
     fun() ->
         ProcessId = "AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0",
         
         %% Test various terminal properties
         TerminalProps = ["supply", "balance", "owner", "name", "ticker"],
         
         lists:foreach(fun(Prop) ->
             Code = lists:flatten([
                 "local result = _G['", ProcessId, "'].", Prop, "\n",
                 "return result\n"
             ]),
             {[Result], _} = luerl:do(Code, LuaState),
             Expected = iolist_to_binary(["Resolved: ", ProcessId, "/", Prop]),
             ?assertEqual(Expected, iolist_to_binary(Result))
         end, TerminalProps)
     end}.

%% Test nested property chain building
test_nested_property_chain({LuaState, _State}) ->
    {"Nested property chain building",
     fun() ->
         ProcessId = "AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0",
         
         %% Test nested path: processId.now.supply
         Code1 = lists:flatten([
             "local result = _G['", ProcessId, "'].now.supply\n",
             "return result\n"
         ]),
         {[Result1], _} = luerl:do(Code1, LuaState),
         ?assertEqual(<<"Resolved: AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0/now/supply">>, 
                      iolist_to_binary(Result1)),
         
         %% Test deeper nesting: processId.token.info.name
         Code2 = lists:flatten([
             "local result = _G['", ProcessId, "'].token.info.name\n",
             "return result\n"
         ]),
         {[Result2], _} = luerl:do(Code2, LuaState),
         ?assertEqual(<<"Resolved: AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0/token/info/name">>, 
                      iolist_to_binary(Result2))
     end}.

%% Test proxy error handling
test_proxy_error_handling({LuaState, _State}) ->
    {"Proxy error handling for property setting",
     fun() ->
         ProcessId = "AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0",
         
         %% Try to set a property on proxy (should error)
         Code = lists:flatten([
             "local success, err = pcall(function()\n",
             "  _G['", ProcessId, "'].newProp = 'value'\n",
             "end)\n",
             "return success, err\n"
         ]),
         
         {[Success, Error], _} = luerl:do(Code, LuaState),
         ?assertEqual(false, Success),
         %% In LUERL, pcall returns the error message as a string
         ?assert(is_binary(Error) orelse is_list(Error)),
         ErrorStr = case is_binary(Error) of
             true -> binary_to_list(Error);
             false -> Error
         end,
         ?assert(string:str(ErrorStr, "Cannot set properties") > 0)
     end}.

%% Test force resolution using call syntax
test_proxy_force_resolution({LuaState, _State}) ->
    {"Force resolution using call syntax",
     fun() ->
         ProcessId = "AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0",
         
         %% Test calling proxy as function
         Code = lists:flatten([
             "local result = _G['", ProcessId, "'].process()\n",
             "return result\n"
         ]),
         {[Result], _} = luerl:do(Code, LuaState),
         ?assertEqual(<<"Resolved: AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0/process">>, 
                      iolist_to_binary(Result))
     end}.

%% Test multiple process IDs
test_multiple_process_ids({LuaState, _State}) ->
    {"Multiple process IDs work independently",
     fun() ->
         ProcessId1 = "AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0",
         ProcessId2 = "BR9xKCqLcCT8laBIuVcCa8W8WgRZyLDh8zUlsNOT5O1",
         
         Code = lists:flatten([
             "local r1 = _G['", ProcessId1, "'].balance\n",
             "local r2 = _G['", ProcessId2, "'].supply\n",
             "return r1, r2\n"
         ]),
         
         {[R1, R2], _} = luerl:do(Code, LuaState),
         ?assertEqual(<<"Resolved: AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0/balance">>, 
                      iolist_to_binary(R1)),
         ?assertEqual(<<"Resolved: BR9xKCqLcCT8laBIuVcCa8W8WgRZyLDh8zUlsNOT5O1/supply">>, 
                      iolist_to_binary(R2))
     end}.

%% Test integration with compute function
test_integration_with_compute({LuaState, State}) ->
    {"Integration with aos compute function",
     fun() ->
         ProcessId = "AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0",
         
         %% Create eval assignment that uses metatable
         %% Note: eval prepends "return " so we need to wrap in a function
         EvalCode = lists:flatten([
             "(function() ",
             "local balance = _G[\"", ProcessId, "\"].balance; ",
             "print('Balance: ' .. balance); ",
             "return balance ",
             "end)()"
         ]),
         
         Assignment = aos_test_helpers:create_eval_assignment(list_to_binary(EvalCode)),
         Result = aos_test_helpers:call_compute(LuaState, State, Assignment),
         Output = aos_test_helpers:extract_output_data(Result),
         
         %% Should contain the resolved balance
         ?assertMatch(<<"Balance: Resolved:", _/binary>>, Output)
     end}.

%% Test proxy cache behavior
test_proxy_cache_behavior({LuaState, _State}) ->
    {"Proxy cache behavior",
     fun() ->
         ProcessId = "AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0",
         
         %% Access same path multiple times to test caching
         Code = lists:flatten([
             "local p1 = _G['", ProcessId, "'].now\n",
             "local p2 = _G['", ProcessId, "'].now\n",
             "-- These should be the same cached proxy\n",
             "local same = (p1 == p2)\n",
             "-- Access terminal properties\n",
             "local r1 = p1.supply\n",
             "local r2 = p2.balance\n",
             "return same, r1, r2\n"
         ]),
         
         {[Same, R1, R2], _} = luerl:do(Code, LuaState),
         ?assertEqual(true, Same),
         ?assertEqual(<<"Resolved: AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0/now/supply">>, 
                      iolist_to_binary(R1)),
         ?assertEqual(<<"Resolved: AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0/now/balance">>, 
                      iolist_to_binary(R2))
     end}.

%% Test invalid process ID access
test_invalid_process_id({LuaState, _State}) ->
    {"Invalid process ID returns nil",
     fun() ->
         %% Try to access non-process-ID key
         Code = "return _G['regularKey'], _G['short']",
         {[R1, R2], _} = luerl:do(Code, LuaState),
         ?assertEqual(nil, R1),
         ?assertEqual(nil, R2)
     end}.

%% Helper function to load metatable integration code
load_metatable_integration_code() ->
    lists:flatten([
        %% Mock ao object
        "ao = {\n",
        "  resolve = function(path)\n",
        "    return 'Resolved: ' .. path\n",
        "  end\n",
        "}\n\n",
        
        %% Terminal properties
        "local AO_TERMINAL_PROPERTIES = {\n",
        "  supply = true, balance = true, owner = true,\n",
        "  name = true, ticker = true, denomination = true,\n",
        "  logo = true, state = true, inbox = true,\n",
        "  spawns = true, errors = true, value = true,\n",
        "  data = true, result = true, output = true\n",
        "}\n\n",
        
        %% Proxy cache
        "local proxyCache = setmetatable({}, { __mode = 'kv' })\n\n",
        
        %% Create process proxy function
        "function createProcessProxy(processId, path)\n",
        "  path = path or processId\n",
        "  local cacheKey = path\n",
        "  if proxyCache[cacheKey] then\n",
        "    return proxyCache[cacheKey]\n",
        "  end\n",
        "  local proxy = {}\n",
        "  local mt = {\n",
        "    __index = function(t, key)\n",
        "      local newPath = path .. '/' .. key\n",
        "      if AO_TERMINAL_PROPERTIES[key] then\n",
        "        return ao.resolve(newPath)\n",
        "      else\n",
        "        return createProcessProxy(processId, newPath)\n",
        "      end\n",
        "    end,\n",
        "    __call = function(t, ...)\n",
        "      return ao.resolve(path)\n",
        "    end,\n",
        "    __tostring = function(t)\n",
        "      return ao.resolve(path)\n",
        "    end,\n",
        "    __newindex = function(t, k, v)\n",
        "      error('Cannot set properties on process proxy. Use ao.send() to interact with processes.')\n",
        "    end,\n",
        "    __metatable = 'ProcessProxy'\n",
        "  }\n",
        "  setmetatable(proxy, mt)\n",
        "  proxyCache[cacheKey] = proxy\n",
        "  return proxy\n",
        "end\n\n",
        
        %% Process ID detection
        "function isProcessId(str)\n",
        "  if type(str) == 'string' and #str == 43 and str:match('^[A-Za-z0-9_-]+$') then\n",
        "    return true\n",
        "  else\n",
        "    return false\n",
        "  end\n",
        "end\n\n",
        
        %% Setup global metatable
        "local function setupGlobalMetatable()\n",
        "  local original_mt = getmetatable(_G) or {}\n",
        "  local original_index = original_mt.__index\n",
        "  local new_mt = {}\n",
        "  for k, v in pairs(original_mt) do\n",
        "    new_mt[k] = v\n",
        "  end\n",
        "  new_mt.__index = function(t, key)\n",
        "    local value = rawget(t, key)\n",
        "    if value ~= nil then\n",
        "      return value\n",
        "    end\n",
        "    if isProcessId(key) then\n",
        "      return createProcessProxy(key)\n",
        "    end\n",
        "    if original_index then\n",
        "      if type(original_index) == 'function' then\n",
        "        return original_index(t, key)\n",
        "      else\n",
        "        return original_index[key]\n",
        "      end\n",
        "    end\n",
        "    return nil\n",
        "  end\n",
        "  setmetatable(_G, new_mt)\n",
        "end\n\n",
        
        %% Initialize
        "setupGlobalMetatable()\n"
    ]).