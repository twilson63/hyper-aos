-module(aos_dump_test).
-include_lib("eunit/include/eunit.hrl").

%% Helper function to load dump module
load_dump_module(LuaState) ->
    {ok, DumpBin} = file:read_file("../src/dump.lua"),
    DumpSrc = binary_to_list(DumpBin),
    
    % Load the dump module
    LoadCode = "do local module = function() " ++ DumpSrc ++ " end; _G.dump = module() end",
    {_, LuaState2} = luerl:do(LoadCode, LuaState),
    LuaState2.

%% Test setup
setup() ->
    LuaState = luerl:init(),
    load_dump_module(LuaState).

%% Test basic table dumping
basic_table_dump_test() ->
    LuaState = setup(),
    
    Code = "
        local t = {a = 1, b = 'hello', c = true, d = false}
        local result = dump.dump(t)
        return result
    ",
    
    {[Result], _} = luerl:do(Code, LuaState),
    ResultStr = binary_to_list(iolist_to_binary(Result)),
    
    % Check that it contains all the expected key-value pairs
    ?assert(string:str(ResultStr, "a = 1") > 0),
    ?assert(string:str(ResultStr, "b = \"hello\"") > 0),
    ?assert(string:str(ResultStr, "c = true") > 0),
    ?assert(string:str(ResultStr, "d = false") > 0).

%% Test array dumping
array_dump_test() ->
    LuaState = setup(),
    
    Code = "
        local t = {1, 2, 3, 'four', true}
        local result = dump.dump(t)
        return result
    ",
    
    {[Result], _} = luerl:do(Code, LuaState),
    ResultStr = binary_to_list(iolist_to_binary(Result)),
    
    % Check array indices are formatted correctly
    ?assert(string:str(ResultStr, "[1] = 1") > 0),
    ?assert(string:str(ResultStr, "[2] = 2") > 0),
    ?assert(string:str(ResultStr, "[3] = 3") > 0),
    ?assert(string:str(ResultStr, "[4] = \"four\"") > 0),
    ?assert(string:str(ResultStr, "[5] = true") > 0).

%% Test nested table dumping
nested_table_dump_test() ->
    LuaState = setup(),
    
    Code = "
        local t = {
            user = {
                name = 'Alice',
                age = 30,
                address = {
                    street = '123 Main St',
                    city = 'NYC'
                }
            }
        }
        local result = dump.dump(t)
        return result
    ",
    
    {[Result], _} = luerl:do(Code, LuaState),
    ResultStr = binary_to_list(iolist_to_binary(Result)),
    
    % Check nested structure is preserved
    ?assert(string:str(ResultStr, "user = {") > 0),
    ?assert(string:str(ResultStr, "name = \"Alice\"") > 0),
    ?assert(string:str(ResultStr, "age = 30") > 0),
    ?assert(string:str(ResultStr, "address = {") > 0),
    ?assert(string:str(ResultStr, "street = \"123 Main St\"") > 0),
    ?assert(string:str(ResultStr, "city = \"NYC\"") > 0).

%% Test circular reference handling
circular_reference_test() ->
    LuaState = setup(),
    
    Code = "
        local t = {a = 1, b = 2}
        t.self = t
        local result = dump.dump(t)
        return result
    ",
    
    {[Result], _} = luerl:do(Code, LuaState),
    ResultStr = binary_to_list(iolist_to_binary(Result)),
    
    % Should contain circular reference marker
    ?assert(string:str(ResultStr, "<Circular") > 0).

%% Test reserved word key handling
reserved_word_keys_test() ->
    LuaState = setup(),
    
    Code = "
        local t = {}
        t['end'] = 'reserved'
        t['function'] = 'also reserved'
        t.normal = 'not reserved'
        local result = dump.dump(t)
        return result
    ",
    
    {[Result], _} = luerl:do(Code, LuaState),
    ResultStr = binary_to_list(iolist_to_binary(Result)),
    
    % Reserved words should be quoted as keys
    ?assert(string:str(ResultStr, "[\"end\"] = \"reserved\"") > 0),
    ?assert(string:str(ResultStr, "[\"function\"] = \"also reserved\"") > 0),
    ?assert(string:str(ResultStr, "normal = \"not reserved\"") > 0).

%% Test special characters in strings
special_characters_test() ->
    LuaState = setup(),
    
    Code = "
        local t = {
            newline = 'hello\\nworld',
            tab = 'hello\\tworld',
            quote = 'hello\"world',
            backslash = 'hello\\\\world'
        }
        local result = dump.dump(t)
        return result
    ",
    
    {[Result], _} = luerl:do(Code, LuaState),
    ResultStr = binary_to_list(iolist_to_binary(Result)),
    
    % Check that string values are properly quoted and escaped
    ?assert(string:str(ResultStr, "newline = \"hello") > 0),
    ?assert(string:str(ResultStr, "world\"") > 0),
    ?assert(string:str(ResultStr, "tab = \"hello") > 0),
    ?assert(string:str(ResultStr, "quote = \"hello\\\"world\"") > 0),
    ?assert(string:str(ResultStr, "backslash = \"hello\\\\world\"") > 0).

%% Test empty table
empty_table_test() ->
    LuaState = setup(),
    
    Code = "
        local t = {}
        local result = dump.dump(t)
        return result
    ",
    
    {[Result], _} = luerl:do(Code, LuaState),
    ResultStr = binary_to_list(iolist_to_binary(Result)),
    
    ?assertEqual("{}", ResultStr).

%% Test non-table values
non_table_values_test() ->
    LuaState = setup(),
    
    Code = "
        local results = {}
        results[1] = dump.dump('hello')
        results[2] = dump.dump(42)
        results[3] = dump.dump(true)
        results[4] = dump.dump(false)
        results[5] = dump.dump(nil)
        return results[1], results[2], results[3], results[4], results[5]
    ",
    
    {[Str, Num, Bool1, Bool2, Nil], _} = luerl:do(Code, LuaState),
    
    ?assertEqual(<<"\"hello\"">>, iolist_to_binary(Str)),
    ?assertEqual(<<"\"42\"">>, iolist_to_binary(Num)),
    ?assertEqual(<<"\"true\"">>, iolist_to_binary(Bool1)),
    ?assertEqual(<<"\"false\"">>, iolist_to_binary(Bool2)),
    ?assertEqual(<<"\"nil\"">>, iolist_to_binary(Nil)).

%% Test different indentation settings
indentation_test() ->
    LuaState = setup(),
    
    Code = "
        local t = {a = 1, b = {c = 2}}
        local compact = dump.dump(t, 0)  -- No indentation
        local spaced = dump.dump(t, 2)   -- 2 spaces
        local default = dump.dump(t)     -- Default indentation
        return compact, spaced, default
    ",
    
    {[Compact, Spaced, Default], _} = luerl:do(Code, LuaState),
    CompactStr = binary_to_list(iolist_to_binary(Compact)),
    SpacedStr = binary_to_list(iolist_to_binary(Spaced)),
    DefaultStr = binary_to_list(iolist_to_binary(Default)),
    
    % Compact should use spaces instead of newlines
    ?assert(string:str(CompactStr, "\n") == 0),
    
    % Spaced and default should use newlines
    ?assert(string:str(SpacedStr, "\n") > 0),
    ?assert(string:str(DefaultStr, "\n") > 0).

%% Test custom filter function
custom_filter_test() ->
    LuaState = setup(),
    
    Code = "
        local t = {
            secret = 'password123',
            public = 'visible',
            hidden = 'classified'
        }
        
        -- Filter that hides keys starting with 'secret' or 'hidden'
        local function filter(val, depth, vtype, use, key, udata)
            if use == 'key' and type(val) == 'string' then
                if val == 'secret' or val == 'hidden' then
                    return nil  -- Hide this key-value pair
                end
            end
            return val
        end
        
        local result = dump.dump(t, nil, nil, filter)
        return result
    ",
    
    {[Result], _} = luerl:do(Code, LuaState),
    ResultStr = binary_to_list(iolist_to_binary(Result)),
    
    % Should contain public but not secret or hidden
    ?assert(string:str(ResultStr, "public = \"visible\"") > 0),
    ?assertEqual(0, string:str(ResultStr, "secret")),
    ?assertEqual(0, string:str(ResultStr, "hidden")).

%% Test mixed numeric and string keys
mixed_keys_test() ->
    LuaState = setup(),
    
    Code = "
        local t = {}
        t[1] = 'first'
        t[2] = 'second'
        t.name = 'Alice'
        t[true] = 'boolean key'
        local result = dump.dump(t)
        return result
    ",
    
    {[Result], _} = luerl:do(Code, LuaState),
    ResultStr = binary_to_list(iolist_to_binary(Result)),
    
    % Check all key types are handled correctly
    ?assert(string:str(ResultStr, "[1] = \"first\"") > 0),
    ?assert(string:str(ResultStr, "[2] = \"second\"") > 0),
    ?assert(string:str(ResultStr, "name = \"Alice\"") > 0),
    ?assert(string:str(ResultStr, "[true] = \"boolean key\"") > 0).

%% Test key sorting behavior
key_sorting_test() ->
    LuaState = setup(),
    
    Code = "
        local t = {}
        t.zebra = 'last'
        t.apple = 'first'
        local result = dump.dump(t)
        return result
    ",
    
    {[Result], _} = luerl:do(Code, LuaState),
    ResultStr = binary_to_list(iolist_to_binary(Result)),
    
    % Check that string keys are sorted correctly
    ?assert(string:str(ResultStr, "apple = \"first\"") > 0),
    ?assert(string:str(ResultStr, "zebra = \"last\"") > 0),
    
    % Check that apple comes before zebra (alphabetical order)
    ApplePos = string:str(ResultStr, "apple = \"first\""),
    ZebraPos = string:str(ResultStr, "zebra = \"last\""),
    ?assert(ApplePos > 0),
    ?assert(ZebraPos > 0),
    ?assert(ApplePos < ZebraPos).

%% Test error conditions
error_conditions_test() ->
    LuaState = setup(),
    
    % Test invalid indent parameter
    Code1 = "
        local status, err = pcall(dump.dump, {}, -1)  -- Negative indent
        return status, err
    ",
    
    {[Status1, Error1], _} = luerl:do(Code1, LuaState),
    ?assertEqual(false, Status1),
    ?assert(string:str(binary_to_list(iolist_to_binary(Error1)), "indent must be unsigned integer") > 0),
    
    % Test invalid padding parameter
    Code2 = "
        local status, err = pcall(dump.dump, {}, 4, -1)  -- Negative padding
        return status, err
    ",
    
    {[Status2, Error2], _} = luerl:do(Code2, LuaState),
    ?assertEqual(false, Status2),
    ?assert(string:str(binary_to_list(iolist_to_binary(Error2)), "padding must be unsigned integer") > 0),
    
    % Test invalid filter parameter
    Code3 = "
        local status, err = pcall(dump.dump, {}, 4, 0, 'not a function')
        return status, err
    ",
    
    {[Status3, Error3], _} = luerl:do(Code3, LuaState),
    ?assertEqual(false, Status3),
    ?assert(string:str(binary_to_list(iolist_to_binary(Error3)), "filter must be function") > 0).

%% Test deep nesting
deep_nesting_test() ->
    LuaState = setup(),
    
    Code = "
        local t = {
            level1 = {
                level2 = {
                    level3 = {
                        level4 = {
                            deep_value = 'found'
                        }
                    }
                }
            }
        }
        local result = dump.dump(t)
        return result
    ",
    
    {[Result], _} = luerl:do(Code, LuaState),
    ResultStr = binary_to_list(iolist_to_binary(Result)),
    
    % Should handle deep nesting
    ?assert(string:str(ResultStr, "level1 = {") > 0),
    ?assert(string:str(ResultStr, "level2 = {") > 0),
    ?assert(string:str(ResultStr, "level3 = {") > 0),
    ?assert(string:str(ResultStr, "level4 = {") > 0),
    ?assert(string:str(ResultStr, "deep_value = \"found\"") > 0).

%% Test version information
version_test() ->
    LuaState = setup(),
    
    Code = "
        return dump._version
    ",
    
    {[Version], _} = luerl:do(Code, LuaState),
    ?assertEqual(<<"1.0.0">>, iolist_to_binary(Version)).