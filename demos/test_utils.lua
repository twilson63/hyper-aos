#!/usr/bin/env lua

--- Test suite for utils.lua module
-- Tests all 16 utility functions for LUERL compatibility and correctness
-- Run with: lua test_utils.lua

-- Load the utils module
require('utils')

-- Test framework functions
local tests_passed = 0
local tests_failed = 0

local function test(name, fn)
  local success, err = pcall(fn)
  if success then
    print("✓ " .. name)
    tests_passed = tests_passed + 1
  else
    print("✗ " .. name .. ": " .. tostring(err))
    tests_failed = tests_failed + 1
  end
end

local function assert_equals(expected, actual, message)
  if expected ~= actual then
    error(string.format("%s: expected %s, got %s", message or "Assertion failed", tostring(expected), tostring(actual)))
  end
end

local function assert_deep_equals(expected, actual, message)
  if type(expected) ~= type(actual) then
    error(string.format("%s: type mismatch - expected %s, got %s", message or "Deep assertion failed", type(expected), type(actual)))
  end
  
  if type(expected) == "table" then
    for k, v in pairs(expected) do
      if type(v) == "table" then
        assert_deep_equals(v, actual[k], message)
      else
        if actual[k] ~= v then
          error(string.format("%s: expected[%s] = %s, actual[%s] = %s", message or "Deep assertion failed", tostring(k), tostring(v), tostring(k), tostring(actual[k])))
        end
      end
    end
  else
    assert_equals(expected, actual, message)
  end
end

print("=== Testing Hyper-AOS Utils Module ===")
print("LUERL Compatibility & Functionality Tests\n")

-- Test 1: matchesPattern function
test("matchesPattern - wildcard", function()
  assert_equals(true, _G.utils.matchesPattern("_", "anything", {}))
end)

test("matchesPattern - exact string match", function()
  assert_equals(true, _G.utils.matchesPattern("test", "test", {}))
  assert_equals(false, _G.utils.matchesPattern("test", "other", {}))
end)

test("matchesPattern - regex pattern", function()
  assert_equals(true, _G.utils.matchesPattern("t.*t", "test", {}))
  assert_equals(false, _G.utils.matchesPattern("t.*t", "abc", {}))
end)

test("matchesPattern - function pattern", function()
  local pattern_fn = function(val, msg) return val > 5 end
  assert_equals(true, _G.utils.matchesPattern(pattern_fn, 10, {}))
  assert_equals(false, _G.utils.matchesPattern(pattern_fn, 3, {}))
end)

test("matchesPattern - table pattern", function()
  local pattern = {"test", "demo"}
  assert_equals(true, _G.utils.matchesPattern(pattern, "test", {}))
  assert_equals(true, _G.utils.matchesPattern(pattern, "demo", {}))
  assert_equals(false, _G.utils.matchesPattern(pattern, "other", {}))
end)

test("matchesPattern - nil handling", function()
  assert_equals(false, _G.utils.matchesPattern(nil, "test", {}))
end)

-- Test 2: matchesSpec function
test("matchesSpec - function spec", function()
  local spec_fn = function(msg) return msg.action == "test" end
  assert_equals(true, _G.utils.matchesSpec({action = "test"}, spec_fn))
  assert_equals(false, _G.utils.matchesSpec({action = "other"}, spec_fn))
end)

test("matchesSpec - table spec", function()
  local spec = {action = "test", type = "message"}
  assert_equals(true, _G.utils.matchesSpec({action = "test", type = "message"}, spec))
  assert_equals(false, _G.utils.matchesSpec({action = "test"}, spec)) -- missing type
end)

test("matchesSpec - string spec", function()
  assert_equals(true, _G.utils.matchesSpec({Action = "test"}, "test"))
  assert_equals(false, _G.utils.matchesSpec({Action = "other"}, "test"))
end)

test("matchesSpec - nil handling", function()
  assert_equals(false, _G.utils.matchesSpec(nil, {}))
  assert_equals(false, _G.utils.matchesSpec({}, nil))
end)

-- Test 3: curry function
test("curry - basic currying", function()
  local add = function(a, b) return a + b end
  local curried_add = _G.utils.curry(add, 2)
  local add5 = curried_add(5)
  assert_equals(8, add5(3))
end)

test("curry - three parameter function", function()
  local add3 = function(a, b, c) return a + b + c end
  local curried = _G.utils.curry(add3, 3)
  local result = curried(1)(2)(3)
  assert_equals(6, result)
end)

test("curry - immediate execution with enough args", function()
  local add = function(a, b) return a + b end
  local curried_add = _G.utils.curry(add, 2)
  assert_equals(7, curried_add(3, 4))
end)

-- Test 4: concat function
test("concat - basic concatenation", function()
  local result = _G.utils.concat({1, 2}, {3, 4})
  assert_deep_equals({1, 2, 3, 4}, result)
end)

test("concat - empty arrays", function()
  local result = _G.utils.concat({}, {1, 2})
  assert_deep_equals({1, 2}, result)
  
  result = _G.utils.concat({1, 2}, {})
  assert_deep_equals({1, 2}, result)
end)

test("concat - curried usage", function()
  local concat_with_123 = _G.utils.concat({1, 2, 3})
  local result = concat_with_123({4, 5})
  assert_deep_equals({1, 2, 3, 4, 5}, result)
end)

-- Test 5: reduce function
test("reduce - sum array", function()
  local sum = function(acc, val) return acc + val end
  local result = _G.utils.reduce(sum, 0, {1, 2, 3, 4})
  assert_equals(10, result)
end)

test("reduce - with nil initial", function()
  local sum = function(acc, val) return acc + val end
  local result = _G.utils.reduce(sum, nil, {1, 2, 3, 4})
  assert_equals(10, result) -- should start with first element
end)

test("reduce - curried usage", function()
  local sum = function(acc, val) return acc + val end
  local sum_reducer = _G.utils.reduce(sum)(0)
  local result = sum_reducer({1, 2, 3})
  assert_equals(6, result)
end)

-- Test 6: map function
test("map - double values", function()
  local double = function(x) return x * 2 end
  local result = _G.utils.map(double, {1, 2, 3})
  assert_deep_equals({2, 4, 6}, result)
end)

test("map - with index", function()
  local add_index = function(val, idx) return val + idx end
  local result = _G.utils.map(add_index, {10, 20, 30})
  assert_deep_equals({11, 22, 33}, result)
end)

test("map - curried usage", function()
  local double = function(x) return x * 2 end
  local doubler = _G.utils.map(double)
  local result = doubler({1, 2, 3})
  assert_deep_equals({2, 4, 6}, result)
end)

-- Test 7: filter function
test("filter - even numbers", function()
  local is_even = function(x) return x % 2 == 0 end
  local result = _G.utils.filter(is_even, {1, 2, 3, 4, 5, 6})
  assert_deep_equals({2, 4, 6}, result)
end)

test("filter - no matches", function()
  local greater_than_10 = function(x) return x > 10 end
  local result = _G.utils.filter(greater_than_10, {1, 2, 3})
  assert_deep_equals({}, result)
end)

test("filter - curried usage", function()
  local is_positive = function(x) return x > 0 end
  local positive_filter = _G.utils.filter(is_positive)
  local result = positive_filter({-1, 0, 1, 2, -3})
  assert_deep_equals({1, 2}, result)
end)

-- Test 8: find function
test("find - first match", function()
  local greater_than_2 = function(x) return x > 2 end
  local result = _G.utils.find(greater_than_2, {1, 2, 3, 4, 5})
  assert_equals(3, result)
end)

test("find - no match", function()
  local greater_than_10 = function(x) return x > 10 end
  local result = _G.utils.find(greater_than_10, {1, 2, 3})
  assert_equals(nil, result)
end)

test("find - curried usage", function()
  local is_string = function(x) return type(x) == "string" end
  local string_finder = _G.utils.find(is_string)
  local result = string_finder({1, 2, "hello", 4})
  assert_equals("hello", result)
end)

-- Test 9: propEq function
test("propEq - property equals", function()
  local obj = {name = "Lua", version = "5.3"}
  assert_equals(true, _G.utils.propEq("name", "Lua", obj))
  assert_equals(false, _G.utils.propEq("name", "Python", obj))
end)

test("propEq - missing property", function()
  local obj = {name = "Lua"}
  assert_equals(false, _G.utils.propEq("version", "5.3", obj))
end)

test("propEq - curried usage", function()
  local is_lua = _G.utils.propEq("name")("Lua")
  assert_equals(true, is_lua({name = "Lua", version = "5.3"}))
  assert_equals(false, is_lua({name = "Python", version = "3.8"}))
end)

-- Test 10: reverse function
test("reverse - basic array", function()
  local result = _G.utils.reverse({1, 2, 3, 4})
  assert_deep_equals({4, 3, 2, 1}, result)
end)

test("reverse - single element", function()
  local result = _G.utils.reverse({"only"})
  assert_deep_equals({"only"}, result)
end)

test("reverse - empty array", function()
  local result = _G.utils.reverse({})
  assert_deep_equals({}, result)
end)

-- Test 11: compose function
test("compose - two functions", function()
  local add1 = function(x) return x + 1 end
  local mult2 = function(x) return x * 2 end
  local composed = _G.utils.compose(add1, mult2) -- add1(mult2(x))
  assert_equals(7, composed(3)) -- 3 * 2 + 1 = 7
end)

test("compose - three functions", function()
  local add1 = function(x) return x + 1 end
  local mult2 = function(x) return x * 2 end
  local sub3 = function(x) return x - 3 end
  local composed = _G.utils.compose(add1, mult2, sub3) -- add1(mult2(sub3(x)))
  assert_equals(5, composed(5)) -- (5-3)*2+1 = 5
end)

-- Test 12: prop function
test("prop - get property", function()
  local obj = {name = "Lua", version = "5.3", year = 1993}
  assert_equals("Lua", _G.utils.prop("name", obj))
  assert_equals("5.3", _G.utils.prop("version", obj))
end)

test("prop - missing property", function()
  local obj = {name = "Lua"}
  assert_equals(nil, _G.utils.prop("missing", obj))
end)

test("prop - curried usage", function()
  local get_name = _G.utils.prop("name")
  assert_equals("Lua", get_name({name = "Lua", version = "5.3"}))
end)

-- Test 13: includes function
test("includes - value found", function()
  assert_equals(true, _G.utils.includes(2, {1, 2, 3, 4}))
  assert_equals(true, _G.utils.includes("hello", {"hi", "hello", "world"}))
end)

test("includes - value not found", function()
  assert_equals(false, _G.utils.includes(5, {1, 2, 3, 4}))
  assert_equals(false, _G.utils.includes("bye", {"hi", "hello", "world"}))
end)

test("includes - curried usage", function()
  local has_lua = _G.utils.includes("Lua")
  assert_equals(true, has_lua({"Python", "Lua", "JavaScript"}))
  assert_equals(false, has_lua({"Python", "Java", "JavaScript"}))
end)

-- Test 14: keys function
test("keys - object keys", function()
  local obj = {name = "Lua", version = "5.3", year = 1993}
  local result = _G.utils.keys(obj)
  
  -- Sort for consistent testing (pairs order is not guaranteed)
  table.sort(result)
  assert_deep_equals({"name", "version", "year"}, result)
end)

test("keys - empty object", function()
  local result = _G.utils.keys({})
  assert_deep_equals({}, result)
end)

test("keys - array indices", function()
  local result = _G.utils.keys({10, 20, 30})
  table.sort(result) -- Sort numeric keys for consistency
  assert_deep_equals({1, 2, 3}, result)
end)

-- Test 15: values function
test("values - object values", function()
  local obj = {a = 1, b = 2, c = 3}
  local result = _G.utils.values(obj)
  
  -- Sort for consistent testing
  table.sort(result)
  assert_deep_equals({1, 2, 3}, result)
end)

test("values - empty object", function()
  local result = _G.utils.values({})
  assert_deep_equals({}, result)
end)

test("values - mixed types", function()
  local obj = {str = "hello", num = 42, bool = true}
  local result = _G.utils.values(obj)
  
  -- Check that all values are present (order may vary)
  local found_str = false
  local found_num = false
  local found_bool = false
  
  for _, v in ipairs(result) do
    if v == "hello" then found_str = true end
    if v == 42 then found_num = true end
    if v == true then found_bool = true end
  end
  
  assert_equals(true, found_str and found_num and found_bool)
end)

-- Test 16: Module version and global access
test("module version", function()
  assert_equals("1.0.0", _G.utils._version)
end)

test("global namespace access", function()
  -- Test that utils is accessible globally
  assert_equals("function", type(_G.utils.map))
  assert_equals("function", type(_G.utils.filter))
  assert_equals("function", type(_G.utils.reduce))
end)

-- Error handling tests
test("error handling - invalid types", function()
  local success, err = pcall(_G.utils.map, "not_a_function", {1, 2, 3})
  assert_equals(false, success)
  
  success, err = pcall(_G.utils.filter, function() end, "not_an_array")
  assert_equals(false, success)
  
  success, err = pcall(_G.utils.reduce, function() end, 0, "not_an_array")
  assert_equals(false, success)
end)

-- LUERL-specific optimizations test
test("LUERL optimizations - large arrays", function()
  -- Test that large arrays work efficiently with # operator
  local large_array = {}
  for i = 1, 1000 do
    large_array[i] = i
  end
  
  local doubled = _G.utils.map(function(x) return x * 2 end, large_array)
  assert_equals(2000, doubled[1000])
  assert_equals(1000, #doubled)
end)

-- Summary
print(string.format("\n=== Test Results ==="))
print(string.format("Tests passed: %d", tests_passed))
print(string.format("Tests failed: %d", tests_failed))
print(string.format("Total tests: %d", tests_passed + tests_failed))

if tests_failed == 0 then
  print("\n🎉 All tests passed! Utils module is ready for LUERL VM.")
else
  print(string.format("\n❌ %d test(s) failed. Please review the implementation.", tests_failed))
  os.exit(1)
end