-module(handlers_collision_test).
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
    
    L1 = case luerl:do(WrappedUtils, L0) of
        {ok, _, NewState1} -> 
            NewState1;
        {error, Reason1} ->
            error({failed_to_load_module, utils, Reason1})
    end,
    
    % Then load handlers-utils module
    WrappedHandlersUtils = "do\n" ++
                          "  local module = function()\n" ++
                          HandlersUtilsCode ++ "\n" ++
                          "  end\n" ++
                          "  _G.package.loaded['.handlers-utils'] = module()\n" ++
                          "end",
    
    L2 = case luerl:do(WrappedHandlersUtils, L1) of
        {ok, _, NewState2} -> 
            NewState2;
        {error, Reason2} ->
            error({failed_to_load_module, handlers_utils, Reason2})
    end,
    
    % Finally load handlers module
    WrappedHandlers = "do\n" ++
                      "  local module = function()\n" ++
                      HandlersCode ++ "\n" ++
                      "  end\n" ++
                      "  _G.package.loaded['.handlers'] = module()\n" ++
                      "  _G.Handlers = module()  -- Also set as global\n" ++
                      "end",
    
    L3 = case luerl:do(WrappedHandlers, L2) of
        {ok, _, NewState3} -> 
            NewState3;
        {error, Reason3} ->
            error({failed_to_load_module, handlers, Reason3})
    end,
    
    L3.

teardown(_) ->
    ok.

%% Test suite
handlers_collision_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(L) ->
         [
          test_handler_collision_allowed(L),
          test_handler_collision_prevented(L)
         ]
     end}.

test_handler_collision_allowed(L) ->
    {"handlers allow overwrite by default",
     fun() ->
         Code = "local h = require('.handlers')\n" ++
                "h.list = {}  -- Clear list\n" ++
                "h.add('test', 'Action1', function(msg) return 'first' end)\n" ++
                "h.add('test', 'Action2', function(msg) return 'second' end)\n" ++
                "return #h.list, h.list[1].pattern",
         {ok, Results, _} = luerl:do(Code, L),
         [Count, Pattern] = Results,
         ?assertEqual(1, Count),  % Still only one handler
         ?assertEqual(<<"Action2">>, Pattern)  % Pattern should be updated
     end}.

test_handler_collision_prevented(L) ->
    {"handlers prevent overwrite when disabled",
     fun() ->
         Code = "local h = require('.handlers')\n" ++
                "h.list = {}  -- Clear list\n" ++
                "h.allowHandlerOverwrite = false  -- Disable overwrites\n" ++
                "h.add('test', 'Action1', function(msg) return 'first' end)\n" ++
                "local status, err = pcall(function()\n" ++
                "  h.add('test', 'Action2', function(msg) return 'second' end)\n" ++
                "end)\n" ++
                "return status, #h.list, h.list[1].pattern",
         {ok, Results, _} = luerl:do(Code, L),
         [Status, Count, Pattern] = Results,
         ?assertNot(Status),  % Should error
         ?assertEqual(1, Count),  % Still only one handler
         ?assertEqual(<<"Action1">>, Pattern)  % Pattern should not be updated
     end}.
