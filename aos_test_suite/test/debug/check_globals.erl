-module(check_globals).
-export([test/0]).

test() ->
    LuaState0 = luerl:init(),
    
    % Simple test - define a function and check if it's global
    TestCode = "
        function testfunc()
            return 'hello'
        end
        
        -- Check if function is in _G
        return type(_G.testfunc)
    ",
    
    {[Result], LuaState1} = luerl:do(TestCode, LuaState0),
    io:format("Type of _G.testfunc after definition: ~p~n", [Result]),
    
    % Now load aos.lua and check
    {ok, AosContent} = file:read_file("../aos.lua"),
    {_, LuaState2} = luerl:do(binary_to_list(AosContent), LuaState1),
    
    % Check various globals
    CheckCode = "
        local results = {}
        results.eval_type = type(eval)
        results.g_eval_type = type(_G.eval)
        results.compute_type = type(compute)
        results.g_compute_type = type(_G.compute)
        results.print_type = type(print)
        results.g_print_type = type(_G.print)
        return results
    ",
    
    {[_Results], LuaState3} = luerl:do(CheckCode, LuaState2),
    io:format("~nGlobal function check results:~n"),
    
    % Get individual values
    {EvalType, _} = luerl:do("return type(eval)", LuaState3),
    {GEvalType, _} = luerl:do("return type(_G.eval)", LuaState3),
    {ComputeType, _} = luerl:do("return type(compute)", LuaState3),
    {PrintType, _} = luerl:do("return type(print)", LuaState3),
    
    io:format("  eval_type = ~p~n", [EvalType]),
    io:format("  g_eval_type = ~p~n", [GEvalType]),
    io:format("  compute_type = ~p~n", [ComputeType]),
    io:format("  print_type = ~p~n", [PrintType]),
    
    ok.