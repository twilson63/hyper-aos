-module(aos_chance_test).
-include_lib("eunit/include/eunit.hrl").

%% Helper function to load chance module
load_chance_module(LuaState) ->
    {ok, ChanceBin} = file:read_file("../src/chance.lua"),
    ChanceSrc = binary_to_list(ChanceBin),
    
    % Load the chance module
    LoadCode = "do local module = function() " ++ ChanceSrc ++ " end; _G.chance = module() end",
    {_, LuaState2} = luerl:do(LoadCode, LuaState),
    LuaState2.

%% Test setup
setup() ->
    LuaState = luerl:init(),
    load_chance_module(LuaState).

%% Test basic random number generation
basic_random_test() ->
    LuaState = setup(),
    
    Code = "
        chance.seed(12345)
        local results = {}
        for i = 1, 10 do
            results[i] = chance.random()
        end
        return results[1], results[2], results[3], results[4], results[5]
    ",
    
    {[R1, R2, R3, R4, R5], _} = luerl:do(Code, LuaState),
    
    % All values should be between 0 and 1
    ?assert(R1 >= 0 andalso R1 < 1),
    ?assert(R2 >= 0 andalso R2 < 1),
    ?assert(R3 >= 0 andalso R3 < 1),
    ?assert(R4 >= 0 andalso R4 < 1),
    ?assert(R5 >= 0 andalso R5 < 1),
    
    % Values should be different (very unlikely to be equal)
    ?assert(R1 =/= R2),
    ?assert(R2 =/= R3),
    ?assert(R3 =/= R4).

%% Test seeded random reproducibility
seeded_random_test() ->
    LuaState = setup(),
    
    Code = "
        -- First sequence with seed 123
        chance.seed(123)
        local seq1 = {}
        for i = 1, 5 do
            seq1[i] = chance.random()
        end
        
        -- Second sequence with same seed 123
        chance.seed(123)
        local seq2 = {}
        for i = 1, 5 do
            seq2[i] = chance.random()
        end
        
        return seq1[1], seq1[2], seq1[3], seq2[1], seq2[2], seq2[3]
    ",
    
    {[S1_1, S1_2, S1_3, S2_1, S2_2, S2_3], _} = luerl:do(Code, LuaState),
    
    % Same seed should produce same sequence
    ?assertEqual(S1_1, S2_1),
    ?assertEqual(S1_2, S2_2),
    ?assertEqual(S1_3, S2_3).

%% Test integer generation
integer_test() ->
    LuaState = setup(),
    
    Code = "
        chance.seed(456)
        local results = {}
        -- Generate integers between 1 and 10
        for i = 1, 20 do
            results[i] = chance.integer(1, 10)
        end
        
        -- Check min and max values
        local min_val = math.huge
        local max_val = -math.huge
        for i = 1, 20 do
            if results[i] < min_val then min_val = results[i] end
            if results[i] > max_val then max_val = results[i] end
        end
        
        return min_val, max_val, results[1], results[2], results[3]
    ",
    
    {[MinVal, MaxVal, R1, R2, R3], _} = luerl:do(Code, LuaState),
    
    % All values should be integers between 1 and 10
    ?assert(MinVal >= 1),
    ?assert(MaxVal =< 10),
    ?assert(R1 >= 1 andalso R1 =< 10),
    ?assert(R2 >= 1 andalso R2 =< 10),
    ?assert(R3 >= 1 andalso R3 =< 10),
    
    % Check they are actually integers
    ?assert(R1 == trunc(R1)),
    ?assert(R2 == trunc(R2)),
    ?assert(R3 == trunc(R3)).

%% Test integer range validation
integer_error_test() ->
    LuaState = setup(),
    
    Code = "
        local status, err = pcall(chance.integer, 10, 5)  -- max < min
        return status, err
    ",
    
    {[Status, Error], _} = luerl:do(Code, LuaState),
    ?assertEqual(false, Status),
    ?assert(string:str(binary_to_list(iolist_to_binary(Error)), "greater than") > 0).

%% Test boolean generation
bool_test() ->
    LuaState = setup(),
    
    Code = "
        chance.seed(789)
        local results = {}
        -- Generate 100 booleans with default 50% probability
        for i = 1, 100 do
            results[i] = chance.bool()
        end
        
        -- Count true values
        local true_count = 0
        for i = 1, 100 do
            if results[i] then
                true_count = true_count + 1
            end
        end
        
        return true_count, results[1], results[2]
    ",
    
    {[TrueCount, R1, R2], _} = luerl:do(Code, LuaState),
    
    % Should be roughly 50/50 (within reasonable bounds)
    ?assert(TrueCount > 30 andalso TrueCount < 70),
    
    % Values should be boolean
    ?assert(is_boolean(R1)),
    ?assert(is_boolean(R2)).

%% Test boolean with custom likelihood
bool_likelihood_test() ->
    LuaState = setup(),
    
    Code = "
        chance.seed(321)
        local results = {}
        -- Generate 100 booleans with 80% probability of true
        for i = 1, 100 do
            results[i] = chance.bool(0.8)
        end
        
        local true_count = 0
        for i = 1, 100 do
            if results[i] then
                true_count = true_count + 1
            end
        end
        
        return true_count
    ",
    
    {[TrueCount], _} = luerl:do(Code, LuaState),
    
    % Should be roughly 80% true (within reasonable bounds)
    ?assert(TrueCount > 60 andalso TrueCount < 95).

%% Test character generation
character_test() ->
    LuaState = setup(),
    
    Code = "
        chance.seed(654)
        local alpha_char = chance.character({alpha = true})
        local numeric_char = chance.character({numeric = true})
        local mixed_char = chance.character({alpha = true, numeric = true})
        local lower_char = chance.character({casing = 'lower'})
        local upper_char = chance.character({casing = 'upper'})
        
        return alpha_char, numeric_char, mixed_char, lower_char, upper_char
    ",
    
    {[AlphaChar, NumericChar, MixedChar, LowerChar, UpperChar], _} = luerl:do(Code, LuaState),
    
    % Convert to strings for easier testing
    AlphaStr = binary_to_list(iolist_to_binary(AlphaChar)),
    NumericStr = binary_to_list(iolist_to_binary(NumericChar)),
    MixedStr = binary_to_list(iolist_to_binary(MixedChar)),
    LowerStr = binary_to_list(iolist_to_binary(LowerChar)),
    UpperStr = binary_to_list(iolist_to_binary(UpperChar)),
    
    % Check character types
    ?assert(length(AlphaStr) == 1),
    ?assert(length(NumericStr) == 1),
    ?assert(length(MixedStr) == 1),
    ?assert(length(LowerStr) == 1),
    ?assert(length(UpperStr) == 1),
    
    % Check numeric character is a digit
    [NumChar] = NumericStr,
    ?assert(NumChar >= $0 andalso NumChar =< $9),
    
    % Check case conversions
    [LowerCharCode] = LowerStr,
    [UpperCharCode] = UpperStr,
    ?assert(LowerCharCode >= $a andalso LowerCharCode =< $z),
    ?assert(UpperCharCode >= $A andalso UpperCharCode =< $Z).

%% Test string generation
string_test() ->
    LuaState = setup(),
    
    Code = "
        chance.seed(987)
        local default_str = chance.string()
        local long_str = chance.string({length = 16})
        local numeric_str = chance.string({length = 10, numeric = true})
        local alpha_str = chance.string({length = 12, alpha = true})
        
        return default_str, long_str, numeric_str, alpha_str
    ",
    
    {[DefaultStr, LongStr, NumericStr, AlphaStr], _} = luerl:do(Code, LuaState),
    
    % Convert to strings
    DefaultString = binary_to_list(iolist_to_binary(DefaultStr)),
    LongString = binary_to_list(iolist_to_binary(LongStr)),
    NumericString = binary_to_list(iolist_to_binary(NumericStr)),
    AlphaString = binary_to_list(iolist_to_binary(AlphaStr)),
    
    % Check lengths
    ?assertEqual(8, length(DefaultString)),  % Default length
    ?assertEqual(16, length(LongString)),
    ?assertEqual(10, length(NumericString)),
    ?assertEqual(12, length(AlphaString)),
    
    % Check numeric string contains only digits
    ?assert(lists:all(fun(C) -> C >= $0 andalso C =< $9 end, NumericString)).

%% Test pick from array
pick_test() ->
    LuaState = setup(),
    
    Code = "
        chance.seed(111)
        local options = {'apple', 'banana', 'cherry', 'date'}
        local results = {}
        
        for i = 1, 20 do
            results[i] = chance.pick(options)
        end
        
        -- Check if all values are from the original array
        local valid = true
        for i = 1, 20 do
            local found = false
            for j = 1, #options do
                if results[i] == options[j] then
                    found = true
                    break
                end
            end
            if not found then
                valid = false
                break
            end
        end
        
        return valid, results[1], results[2], results[3]
    ",
    
    {[Valid, R1, R2, R3], _} = luerl:do(Code, LuaState),
    
    ?assertEqual(true, Valid),
    
    % Convert results to check they are valid strings
    R1Str = binary_to_list(iolist_to_binary(R1)),
    R2Str = binary_to_list(iolist_to_binary(R2)),
    R3Str = binary_to_list(iolist_to_binary(R3)),
    
    ValidOptions = ["apple", "banana", "cherry", "date"],
    ?assert(lists:member(R1Str, ValidOptions)),
    ?assert(lists:member(R2Str, ValidOptions)),
    ?assert(lists:member(R3Str, ValidOptions)).

%% Test pick error conditions
pick_error_test() ->
    LuaState = setup(),
    
    Code = "
        local status1, err1 = pcall(chance.pick, {})  -- Empty array
        local status2, err2 = pcall(chance.pick, 'not a table')  -- Not a table
        return status1, err1, status2, err2
    ",
    
    {[Status1, Error1, Status2, Error2], _} = luerl:do(Code, LuaState),
    
    ?assertEqual(false, Status1),
    ?assertEqual(false, Status2),
    ?assert(string:str(binary_to_list(iolist_to_binary(Error1)), "non-empty") > 0),
    ?assert(string:str(binary_to_list(iolist_to_binary(Error2)), "table") > 0).

%% Test shuffle function
shuffle_test() ->
    LuaState = setup(),
    
    Code = "
        chance.seed(222)
        local original = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
        local shuffled = chance.shuffle(original)
        
        -- Check all elements are present
        local all_present = true
        for i = 1, #original do
            local found = false
            for j = 1, #shuffled do
                if original[i] == shuffled[j] then
                    found = true
                    break
                end
            end
            if not found then
                all_present = false
                break
            end
        end
        
        -- Check lengths match
        local same_length = #original == #shuffled
        
        -- Check it's actually shuffled (not identical)
        local is_shuffled = false
        for i = 1, #original do
            if original[i] ~= shuffled[i] then
                is_shuffled = true
                break
            end
        end
        
        return all_present, same_length, is_shuffled, shuffled[1], shuffled[2]
    ",
    
    {[AllPresent, SameLength, IsShuffled, S1, S2], _} = luerl:do(Code, LuaState),
    
    ?assertEqual(true, AllPresent),
    ?assertEqual(true, SameLength),
    ?assertEqual(true, IsShuffled),
    
    % Check returned values are valid
    ?assert(S1 >= 1 andalso S1 =< 10),
    ?assert(S2 >= 1 andalso S2 =< 10).

%% Test weighted choice
weighted_test() ->
    LuaState = setup(),
    
    Code = "
        chance.seed(333)
        local choices = {'rare', 'common', 'uncommon'}
        local weights = {1, 10, 5}  -- common should appear most often
        local results = {}
        
        for i = 1, 100 do
            results[i] = chance.weighted(choices, weights)
        end
        
        -- Count occurrences
        local counts = {rare = 0, common = 0, uncommon = 0}
        for i = 1, 100 do
            if results[i] == 'rare' then
                counts.rare = counts.rare + 1
            elseif results[i] == 'common' then
                counts.common = counts.common + 1
            elseif results[i] == 'uncommon' then
                counts.uncommon = counts.uncommon + 1
            end
        end
        
        return counts.rare, counts.common, counts.uncommon
    ",
    
    {[RareCount, CommonCount, UncommonCount], _} = luerl:do(Code, LuaState),
    
    % Common should be most frequent, rare should be least
    ?assert(CommonCount > UncommonCount),
    ?assert(UncommonCount > RareCount),
    ?assert(RareCount >= 0).

%% Test weighted choice error conditions
weighted_error_test() ->
    LuaState = setup(),
    
    Code = "
        local status1, err1 = pcall(chance.weighted, {'a'}, {'b', 'c'})  -- Mismatched lengths
        local status2, err2 = pcall(chance.weighted, {'a'}, {0})  -- Zero weight
        return status1, err1, status2, err2
    ",
    
    {[Status1, Error1, Status2, Error2], _} = luerl:do(Code, LuaState),
    
    ?assertEqual(false, Status1),
    ?assertEqual(false, Status2),
    ?assert(string:str(binary_to_list(iolist_to_binary(Error1)), "same length") > 0),
    ?assert(string:str(binary_to_list(iolist_to_binary(Error2)), "positive") > 0).

%% Test normal distribution
normal_test() ->
    LuaState = setup(),
    
    Code = "
        chance.seed(444)
        local results = {}
        
        -- Generate normal distribution with mean=0, dev=1
        for i = 1, 100 do
            results[i] = chance.normal()
        end
        
        -- Calculate mean and check it's close to 0
        local sum = 0
        for i = 1, 100 do
            sum = sum + results[i]
        end
        local mean = sum / 100
        
        -- Generate normal with custom parameters
        local custom = chance.normal({mean = 10, dev = 2})
        
        return mean, custom, results[1], results[2]
    ",
    
    {[Mean, Custom, R1, R2], _} = luerl:do(Code, LuaState),
    
    % Mean should be close to 0 (within reasonable bounds for 100 samples)
    ?assert(abs_val(Mean) < 0.5),
    
    % Custom should be around 10 (within reasonable bounds)
    ?assert(Custom > 5 andalso Custom < 15),
    
    % Values should be reasonable for normal distribution
    ?assert(abs_val(R1) < 5),  % Very unlikely to be outside ±5 standard deviations
    ?assert(abs_val(R2) < 5).

%% Test version information
version_test() ->
    LuaState = setup(),
    
    Code = "
        return chance._version
    ",
    
    {[Version], _} = luerl:do(Code, LuaState),
    ?assertEqual(<<"1.0.0">>, iolist_to_binary(Version)).

%% Helper function for absolute value
abs_val(X) when X < 0 -> -X;
abs_val(X) -> X.