-module(test_luerl_function_serialize).
-export([test/0]).

test() ->
    % Create LUERL state
    L = luerl:init(),
    
    % Test 1: Check string.dump availability
    io:format("~n=== Testing LUERL Function Serialization ===~n"),
    
    Code1 = "
        -- Test string.dump
        if string.dump then
            return 'string.dump exists'
        else
            return 'string.dump NOT available'
        end
    ",
    {[Result1], L2} = luerl:do(Code1, L),
    io:format("1. String.dump status: ~s~n", [Result1]),
    
    % Test 2: Function source serialization
    Code2 = "
        -- Create a function from source
        local func_source = 'return function(x) return x * 2 end'
        local func_factory = load(func_source)
        local double = func_factory()
        return double(21)
    ",
    {[Result2], L3} = luerl:do(Code2, L2),
    io:format("2. Function from source: ~p~n", [Result2]),
    
    % Test 3: Persisting functions across states
    Code3 = "
        -- Store function definition
        FunctionDefs = FunctionDefs or {}
        FunctionDefs['adder'] = 'return function(a, b) return a + b end'
        
        -- Create the function
        local adder = load(FunctionDefs['adder'])()
        return adder(10, 15)
    ",
    {[Result3], L4} = luerl:do(Code3, L3),
    io:format("3. Persisted function result: ~p~n", [Result3]),
    
    % Test 4: Extract state and check if function definition persists
    Code4 = "
        -- Check if definition still exists
        if FunctionDefs and FunctionDefs['adder'] then
            local adder = load(FunctionDefs['adder'])()
            return adder(100, 200)
        else
            return 'Function definition lost'
        end
    ",
    {[Result4], _L5} = luerl:do(Code4, L4),
    io:format("4. Function definition persistence: ~p~n", [Result4]),
    
    % Test 5: Handler serialization pattern
    Code5 = "
        -- Handler storage pattern
        Handlers = Handlers or {}
        
        -- Store handler as table with source
        Handlers['test'] = {
            name = 'test_handler',
            pattern = { Action = 'Test' },
            source = [[
                return function(msg)
                    return 'Handled: ' .. (msg.Action or 'nil')
                end
            ]]
        }
        
        -- Load and use handler
        local handler_data = Handlers['test']
        local handler = load(handler_data.source)()
        return handler({ Action = 'TestAction' })
    ",
    {[Result5], L6} = luerl:do(Code5, L4),
    io:format("5. Handler pattern result: ~s~n", [Result5]),
    
    % Test 6: Complex serialization with environment
    Code6 = "
        -- Create a factory that generates functions with environment
        local function create_counter_source(initial)
            return string.format([[
                local count = %d
                return function(inc)
                    count = count + (inc or 1)
                    return count
                end
            ]], initial)
        end
        
        -- Store the source
        local counter_source = create_counter_source(100)
        CounterSources = CounterSources or {}
        CounterSources['main'] = counter_source
        
        -- Create and test counter
        local counter = load(counter_source)()
        local r1 = counter(10)
        local r2 = counter(20)
        return string.format('Counter: %d, %d', r1, r2)
    ",
    {[Result6], _L7} = luerl:do(Code6, L6),
    io:format("6. Complex serialization: ~s~n", [Result6]),
    
    io:format("~n=== Summary ===~n"),
    io:format("LUERL function serialization options:~n"),
    io:format("- string.dump is NOT available in LUERL~n"),
    io:format("- Best approach: Store function source as strings~n"),
    io:format("- Use load() to recreate functions from source~n"),
    io:format("- Store in global tables for persistence~n"),
    io:format("- Pattern works well for handlers and callbacks~n~n"),
    
    ok.