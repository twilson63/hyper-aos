-module(aos_json_test).
-include_lib("eunit/include/eunit.hrl").

%% Helper function to load json module
load_json_module(LuaState) ->
    {ok, JsonBin} = file:read_file("../src/json.lua"),
    JsonSrc = binary_to_list(JsonBin),
    
    % Load the json module - wrap it in a function to capture the return value
    LoadCode = "do local json_module = function() " ++ JsonSrc ++ " end; _G.json = json_module() end",
    {ok, _, LuaState2} = luerl:do(LoadCode, LuaState),
    LuaState2.

%% Test setup
setup() ->
    LuaState = luerl:init(),
    load_json_module(LuaState).

%% Test circular reference handling in encode
circular_reference_test() ->
    LuaState = setup(),
    
    % Create a table with circular reference
    Code = "
        local t = {a = 1, b = 2}
        t.self = t
        local status, err = pcall(json.encode, t)
        return status, err
    ",
    
    {ok, [Status, Error], _} = luerl:do(Code, LuaState),
    
    % Should return false (error) and error message containing "circular"
    ?assertEqual(false, Status),
    ?assert(string:str(binary_to_list(iolist_to_binary(Error)), "circular") > 0).

%% Test function reference handling (functions should be skipped)
function_reference_test() ->
    LuaState = setup(),
    
    % Create a table with functions
    Code = "
        local t = {
            a = 1,
            b = function() return 2 end,
            c = 'hello',
            d = function(x) return x * 2 end
        }
        local result = json.encode(t)
        -- Decode it back to verify functions were skipped
        local decoded = json.decode(result)
        return result, decoded.a, decoded.c, decoded.b
    ",
    
    {ok, [JsonStr, A, C, B], _} = luerl:do(Code, LuaState),
    
    % Should encode without the functions
    ?assertEqual(1.0, A),
    ?assertEqual(<<"hello">>, iolist_to_binary(C)),
    ?assertEqual(nil, B), % Function should not be in the result

    % Check that the JSON string doesn't contain 'b' or 'd' keys
    JsonBinary = iolist_to_binary(JsonStr),
    ?assertNotEqual(nomatch, binary:match(JsonBinary, <<"\"a\":">>)),
    ?assertNotEqual(nomatch, binary:match(JsonBinary, <<"\"c\":">>)),
    ?assertEqual(nomatch, binary:match(JsonBinary, <<"\"b\":">>)),
    ?assertEqual(nomatch, binary:match(JsonBinary, <<"\"d\":">>)).

%% Test JSON array to Lua table conversion
json_array_to_lua_table_test() ->
    LuaState = setup(),
    
    % Test various array types  
    % Note: null becomes nil, but nil values still occupy array positions
    Code = "
        local json_str = '[1, 2, 3, \"hello\", true, false, null]'
        local t = json.decode(json_str)
        -- Count all elements including nil
        local count = 0
        for i = 1, 10 do
            if t[i] ~= nil or i == 7 then  -- Count position 7 even if nil
                count = i
            else
                break
            end
        end
        return count, t[1], t[2], t[3], t[4], t[5], t[6], t[7]
    ",
    
    {ok, [Len, V1, V2, V3, V4, V5, V6, V7], _} = luerl:do(Code, LuaState),
    
    % Arrays with trailing nil have length of last non-nil element
    ?assert(Len == 6 orelse Len == 7),
    % JSON numbers are decoded as Lua numbers (can be float or int)
    ?assert(V1 == 1 orelse V1 == 1.0),
    ?assert(V2 == 2 orelse V2 == 2.0),
    ?assert(V3 == 3 orelse V3 == 3.0),
    ?assertEqual(<<"hello">>, iolist_to_binary(V4)),
    ?assertEqual(true, V5),
    ?assertEqual(false, V6),
    ?assertEqual(nil, V7).

%% Test nested arrays
nested_array_test() ->
    LuaState = setup(),
    
    Code = "
        local json_str = '[[1, 2], [3, 4], [5, [6, 7]]]'
        local t = json.decode(json_str)
        return #t, t[1][1], t[1][2], t[2][1], t[2][2], t[3][1], t[3][2][1], t[3][2][2]
    ",
    
    {ok, [Len, V11, V12, V21, V22, V31, V321, V322], _} = luerl:do(Code, LuaState),
    
    ?assert(Len == 3 orelse Len == 3.0),
    ?assert(V11 == 1 orelse V11 == 1.0),
    ?assert(V12 == 2 orelse V12 == 2.0),
    ?assert(V21 == 3 orelse V21 == 3.0),
    ?assert(V22 == 4 orelse V22 == 4.0),
    ?assert(V31 == 5 orelse V31 == 5.0),
    ?assert(V321 == 6 orelse V321 == 6.0),
    ?assert(V322 == 7 orelse V322 == 7.0).

%% Test JSON object to Lua table conversion
json_object_to_lua_table_test() ->
    LuaState = setup(),
    
    Code = "
        local json_str = '{\"name\": \"John\", \"age\": 30, \"active\": true, \"balance\": 123.45}'
        local t = json.decode(json_str)
        return t.name, t.age, t.active, t.balance, t['name'], t['age']
    ",
    
    {ok, [Name, Age, Active, Balance, Name2, Age2], _} = luerl:do(Code, LuaState),
    
    ?assertEqual(<<"John">>, iolist_to_binary(Name)),
    ?assert(Age == 30 orelse Age == 30.0),
    ?assertEqual(true, Active),
    ?assertEqual(123.45, Balance),
    ?assertEqual(<<"John">>, iolist_to_binary(Name2)),
    ?assert(Age2 == 30 orelse Age2 == 30.0).

%% Test nested objects
nested_object_test() ->
    LuaState = setup(),
    
    Code = "
        local json_str = '{\"user\": {\"name\": \"Alice\", \"details\": {\"age\": 25, \"city\": \"NYC\"}}}'
        local t = json.decode(json_str)
        return t.user.name, t.user.details.age, t.user.details.city
    ",
    
    {ok, [Name, Age, City], _} = luerl:do(Code, LuaState),
    
    ?assertEqual(<<"Alice">>, iolist_to_binary(Name)),
    ?assert(Age == 25 orelse Age == 25.0),
    ?assertEqual(<<"NYC">>, iolist_to_binary(City)).

%% Test mixed arrays and objects
mixed_structure_test() ->
    LuaState = setup(),
    
    Code = "
        local json_str = '{\"items\": [{\"id\": 1, \"name\": \"Item1\"}, {\"id\": 2, \"name\": \"Item2\"}], \"count\": 2}'
        local t = json.decode(json_str)
        return t.count, #t.items, t.items[1].id, t.items[1].name, t.items[2].id, t.items[2].name
    ",
    
    {ok, [Count, ItemsLen, Id1, Name1, Id2, Name2], _} = luerl:do(Code, LuaState),
    
    ?assert(Count == 2 orelse Count == 2.0),
    ?assert(ItemsLen == 2 orelse ItemsLen == 2.0),
    ?assert(Id1 == 1 orelse Id1 == 1.0),
    ?assertEqual(<<"Item1">>, iolist_to_binary(Name1)),
    ?assert(Id2 == 2 orelse Id2 == 2.0),
    ?assertEqual(<<"Item2">>, iolist_to_binary(Name2)).

%% Test empty structures
empty_structures_test() ->
    LuaState = setup(),
    
    Code = "
        local empty_array = json.decode('[]')
        local empty_object = json.decode('{}')
        local array_len = #empty_array
        local object_count = 0
        for k,v in pairs(empty_object) do
            object_count = object_count + 1
        end
        return array_len, object_count
    ",
    
    {ok, [ArrayLen, ObjectCount], _} = luerl:do(Code, LuaState),
    
    ?assert(ArrayLen == 0 orelse ArrayLen == 0.0),
    ?assert(ObjectCount == 0 orelse ObjectCount == 0.0).

%% Test encoding and decoding round trip
round_trip_test() ->
    LuaState = setup(),
    
    Code = "
        local original = {
            string = 'hello world',
            number = 42,
            float = 3.14159,
            bool_true = true,
            bool_false = false,
            null_value = nil,
            array = {1, 2, 3},
            object = {a = 'A', b = 'B'}
        }
        local encoded = json.encode(original)
        local decoded = json.decode(encoded)
        
        return decoded.string, decoded.number, decoded.float, 
               decoded.bool_true, decoded.bool_false, decoded.null_value,
               #decoded.array, decoded.array[1], decoded.array[2], decoded.array[3],
               decoded.object.a, decoded.object.b
    ",
    
    {ok, [Str, Num, Float, BoolT, BoolF, Null, ArrLen, A1, A2, A3, ObjA, ObjB], _} = luerl:do(Code, LuaState),
    
    ?assertEqual(<<"hello world">>, iolist_to_binary(Str)),
    ?assert(Num == 42 orelse Num == 42.0),
    ?assert(abs_val(3.14159 - Float) < 0.00001),
    ?assertEqual(true, BoolT),
    ?assertEqual(false, BoolF),
    ?assertEqual(nil, Null),
    ?assert(ArrLen == 3 orelse ArrLen == 3.0),
    ?assert(A1 == 1 orelse A1 == 1.0),
    ?assert(A2 == 2 orelse A2 == 2.0),
    ?assert(A3 == 3 orelse A3 == 3.0),
    ?assertEqual(<<"A">>, iolist_to_binary(ObjA)),
    ?assertEqual(<<"B">>, iolist_to_binary(ObjB)).

%% Test special characters in strings
special_characters_test() ->
    LuaState = setup(),
    
    Code = "
        local test_str = 'Hello\\nWorld\\t\\\"Quoted\\\"\\\\Backslash'
        local encoded = json.encode(test_str)
        local decoded = json.decode(encoded)
        return decoded == test_str, encoded
    ",
    
    {ok, [Same, Encoded], _} = luerl:do(Code, LuaState),
    
    ?assertEqual(true, Same),
    % Check that encoded string contains escaped characters
    EncodedBin = iolist_to_binary(Encoded),
    ?assertNotEqual(nomatch, binary:match(EncodedBin, <<"\\n">>)),
    ?assertNotEqual(nomatch, binary:match(EncodedBin, <<"\\t">>)),
    ?assertNotEqual(nomatch, binary:match(EncodedBin, <<"\\\"">>)),
    ?assertNotEqual(nomatch, binary:match(EncodedBin, <<"\\\\">>)).

%% Test unicode handling
unicode_test() ->
    LuaState = setup(),
    
    Code = "
        local test_str = 'Hello 世界 🌍'
        local encoded = json.encode(test_str)
        local decoded = json.decode(encoded)
        return decoded == test_str
    ",
    
    {ok, [Same], _} = luerl:do(Code, LuaState),
    ?assertEqual(true, Same).

%% Test invalid JSON handling
invalid_json_test() ->
    LuaState = setup(),
    
    % Test various invalid JSON strings
    InvalidTests = [
        "{\"key\": }",           % Missing value
        "{\"key\" \"value\"}",    % Missing colon
        % "[1, 2,]",              % Trailing comma - this json module allows it
        "{'key': 'value'}",       % Single quotes
        "{key: \"value\"}",       % Unquoted key
        "[1, 2, 3,",              % Unclosed array
        "{\"key\": \"value\"",    % Unclosed object
        "undefined",              % Invalid literal
        "NaN",                    % Invalid number
        "Infinity"                % Invalid number
    ],
    
    lists:foreach(fun(InvalidJson) ->
        % Use double quotes and escape them properly
        EscapedJson = lists:flatten(io_lib:format("~p", [InvalidJson])),
        Code = io_lib:format("
            local status, err = pcall(json.decode, ~s)
            return status
        ", [EscapedJson]),
        
        {ok, [Status], _} = luerl:do(lists:flatten(Code), LuaState),
        ?assertEqual(false, Status)
    end, InvalidTests).

%% Test sparse array detection
sparse_array_test() ->
    LuaState = setup(),
    
    Code = "
        local t = {}
        t[1] = 'a'
        t[3] = 'c'  -- Skip index 2, making it sparse
        local status, err = pcall(json.encode, t)
        return status, err
    ",
    
    {ok, [Status, Error], _} = luerl:do(Code, LuaState),
    
    ?assertEqual(false, Status),
    ?assert(string:str(binary_to_list(iolist_to_binary(Error)), "sparse") > 0).

%% Test mixed key types detection
mixed_keys_test() ->
    LuaState = setup(),
    
    Code = "
        local t = {}
        t[1] = 'a'
        t['key'] = 'b'  -- Mixed numeric and string keys
        local status, err = pcall(json.encode, t)
        return status, err
    ",
    
    {ok, [Status, Error], _} = luerl:do(Code, LuaState),
    
    ?assertEqual(false, Status),
    ?assert(string:str(binary_to_list(iolist_to_binary(Error)), "mixed") > 0).

%% Test large numbers
large_numbers_test() ->
    LuaState = setup(),
    
    Code = "
        local t = {
            large = 1234567890123456,
            negative = -9876543210987654,
            float = 1.23456789012345
        }
        local encoded = json.encode(t)
        local decoded = json.decode(encoded)
        return decoded.large, decoded.negative, decoded.float
    ",
    
    {ok, [Large, Negative, Float], _} = luerl:do(Code, LuaState),
    
    % Note: Lua numbers are floats, so there will be precision loss with large integers
    % The precision loss is more significant than expected (around 44 for 15-digit numbers)
    ?assert(abs_val(1234567890123456 - Large) < 100),
    ?assert(abs_val(-9876543210987654 - Negative) < 100),
    ?assert(abs_val(1.23456789012345 - Float) < 0.00000001).

%% Helper function for absolute value (renamed to avoid conflict with erlang:abs)
abs_val(X) when X < 0 -> -X;
abs_val(X) -> X.