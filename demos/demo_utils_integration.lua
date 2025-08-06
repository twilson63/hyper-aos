#!/usr/bin/env lua

--- Demo script showing utils.lua integration with Hyper-AOS patterns
-- This demonstrates how the utils module works alongside the existing aos.lua
-- and follows the LUERL VM optimization patterns.

-- Load utils module (this adds utils to _G.utils)
require('utils')

print("=== Hyper-AOS Utils Integration Demo ===\n")

-- Demonstrate global namespace access (Hyper-AOS pattern)
print("1. Global Namespace Access:")
print("   utils available in _G:", _G.utils ~= nil)
print("   Version:", _G.utils._version)
print()

-- Demonstrate functional programming patterns
print("2. Functional Programming Patterns:")

-- Example: Processing message data with functional composition
local messages = {
  {type = "text", priority = 1, content = "Hello"},
  {type = "image", priority = 3, content = "photo.jpg"}, 
  {type = "text", priority = 2, content = "World"},
  {type = "audio", priority = 1, content = "sound.mp3"}
}

print("   Original messages:", #messages, "items")

-- Filter high priority text messages and get content
local high_priority_text = _G.utils.compose(
  _G.utils.map(_G.utils.prop("content")),
  _G.utils.filter(function(msg) 
    return msg.type == "text" and msg.priority >= 2 
  end)
)

local result = high_priority_text(messages)
print("   High priority text content:", table.concat(result, ", "))
print()

-- Demonstrate currying for reusable message handlers
print("3. Message Pattern Matching:")

-- Create curried message handlers
local is_message_type = _G.utils.curry(function(msg_type, pattern, msg)
  return _G.utils.matchesSpec(msg, {type = pattern, action = msg_type})
end, 3)

local is_eval_message = is_message_type("eval")
local is_send_message = is_message_type("send")

-- Test message patterns
local test_messages = {
  {type = "message", action = "eval", data = "1 + 1"},
  {type = "message", action = "send", target = "process123"},
  {type = "process", authority = "addr1,addr2"}
}

for i, msg in ipairs(test_messages) do
  local is_eval = is_eval_message("message", msg)
  local is_send = is_send_message("message", msg)
  print(string.format("   Message %d: eval=%s, send=%s", i, tostring(is_eval), tostring(is_send)))
end
print()

-- Demonstrate LUERL-optimized array operations
print("4. LUERL-Optimized Array Operations:")

-- Create large array to show performance
local large_array = {}
for i = 1, 1000 do
  large_array[i] = i
end

-- Chain operations efficiently
local process_large_data = _G.utils.compose(
  _G.utils.reduce(function(sum, x) return sum + x end, 0),
  _G.utils.filter(function(x) return x % 2 == 0 end),
  _G.utils.map(function(x) return x * 2 end)
)

local result = process_large_data(large_array)
print("   Processed 1000 items:", result, "(sum of doubled even numbers)")
print()

-- Demonstrate state-compatible operations (Hyper-AOS pattern)
print("5. State-Compatible Operations:")

-- Simulate _G state operations like in aos.lua
_G.inbox_processors = {
  text_filter = _G.utils.filter(_G.utils.propEq("type", "text")),
  content_extractor = _G.utils.map(_G.utils.prop("content")),
  priority_sorter = function(messages)
    local sorted = _G.utils.concat(messages, {})
    table.sort(sorted, function(a, b) return (a.priority or 0) > (b.priority or 0) end)
    return sorted
  end
}

-- Process messages using stored functions
local text_messages = _G.inbox_processors.text_filter(messages)
local contents = _G.inbox_processors.content_extractor(text_messages)
local sorted_messages = _G.inbox_processors.priority_sorter(messages)

print("   Text messages:", #text_messages)
print("   Text contents:", table.concat(contents, ", "))
print("   Sorted by priority:", _G.utils.map(_G.utils.prop("content"))(sorted_messages)[1])
print()

-- Demonstrate binary key compatibility (LUERL pattern)
print("6. Binary String Key Compatibility:")

local message_with_binary_keys = {}
message_with_binary_keys["from-process"] = "process123"
message_with_binary_keys["commitments"] = {
  ["key1"] = {type = "RSA-PSS-512", committer = "addr123"}
}

-- Extract using prop function with binary keys
local from_process = _G.utils.prop("from-process")(message_with_binary_keys)
local commitment_type = _G.utils.compose(
  _G.utils.prop("type"),
  _G.utils.prop("key1"),
  _G.utils.prop("commitments")
)(message_with_binary_keys)

print("   from-process:", from_process)
print("   commitment type:", commitment_type)
print()

print("✅ Utils integration demo completed successfully!")
print("   All patterns compatible with LUERL VM and Hyper-AOS architecture.")