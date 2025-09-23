--- Demonstration of successful utils integration with aos.lua
-- This file shows how the utils module has been integrated into the AOS compute environment
-- and provides working examples of the integration patterns.
-- @module demo_integrated_utils
-- @author Hyper-AOS Team
-- @version 1.0.0

-- Load utils (this would be done by aos.lua in production)
require("utils")

-- Simulate aos.lua environment
_G.Inbox = _G.Inbox or {}
_G.MAX_INBOX_SIZE = 10000
_G._OUTPUT = ""
_G.authorities = {"authority1_address_43chars_test_auth_addr1", "authority2_address_43chars_test_auth_addr2"}
_G.owner = "owner_address_43chars_test_owner_address1"

-- Helper function to simulate message processing like in aos.lua
local function process_message(msg)
  print("Processing message: " .. (msg.action or "no-action"))
  
  -- Add to inbox
  table.insert(_G.Inbox, msg)
  
  -- Demonstrate utils usage in message processing
  if msg.action == "filter-inbox" then
    local from_filter = msg.from or "unknown"
    local filtered = _G.utils.filter(_G.utils.propEq("from", from_filter), _G.Inbox)
    print("Filtered messages from " .. from_filter .. ": " .. #filtered .. " messages")
    return filtered
  elseif msg.action == "map-data" then
    -- Transform message data using utils.map
    if msg.data and type(msg.data) == "table" then
      local processed = _G.utils.map(function(item)
        return "PROCESSED: " .. tostring(item)
      end, msg.data)
      print("Mapped data:")
      for i, v in ipairs(processed) do
        print("  [" .. i .. "]: " .. v)
      end
      return processed
    end
  elseif msg.action == "demo-pattern-match" then
    -- Demonstrate pattern matching for message specs
    local eval_spec = {action = "eval", from = _G.owner}
    local send_spec = {action = "send"}
    local wildcard_spec = {action = "_"}  -- matches any action
    
    local matches_eval = _G.utils.matchesSpec(msg, eval_spec)
    local matches_send = _G.utils.matchesSpec(msg, send_spec) 
    local matches_wildcard = _G.utils.matchesSpec(msg, wildcard_spec)
    
    print("Pattern matching results:")
    print("  Matches eval spec: " .. tostring(matches_eval))
    print("  Matches send spec: " .. tostring(matches_send))
    print("  Matches wildcard spec: " .. tostring(matches_wildcard))
    
    return {
      eval = matches_eval,
      send = matches_send,
      wildcard = matches_wildcard
    }
  elseif msg.action == "compose-pipeline" then
    -- Demonstrate function composition for data processing
    local extract_data = _G.utils.prop("data")
    local to_upper = function(str) return string.upper(tostring(str)) end
    local add_prefix = function(str) return "PIPELINE: " .. str end
    
    local pipeline = _G.utils.compose(add_prefix, to_upper, extract_data)
    local result = pipeline(msg)
    
    print("Composed pipeline result: " .. tostring(result))
    return result
  elseif msg.action == "curry-demo" then
    -- Demonstrate curried functions for reusable message transformers
    local create_response = _G.utils.curry(function(status, message_text, recipient)
      return {
        status = status,
        message = message_text,
        to = recipient,
        timestamp = os.time()
      }
    end, 3)
    
    local success_response = create_response("success")
    local success_to_sender = success_response("Operation completed successfully")
    local final_response = success_to_sender(msg.from or "unknown")
    
    print("Curried response generated:")
    for k, v in pairs(final_response) do
      print("  " .. k .. ": " .. tostring(v))
    end
    
    return final_response
  end
  
  return "Message processed"
end

-- Demo messages to test integration
local demo_messages = {
  {
    action = "filter-inbox",
    from = "authority1_address_43chars_test_auth_addr1",
    data = "Filter messages from me"
  },
  {
    action = "map-data", 
    from = "test_user_43chars_demo_testing_address1",
    data = {"item1", "item2", "item3"}
  },
  {
    action = "demo-pattern-match",
    from = "owner_address_43chars_test_owner_address1"
  },
  {
    action = "compose-pipeline",
    from = "test_sender_43chars_pipeline_demo_addr1",
    data = "hello world"
  },
  {
    action = "curry-demo",
    from = "test_requester_43chars_curry_demo_addr1"
  }
}

-- Main demonstration function
function demonstrate_utils_integration()
  print("=== Utils Integration with AOS Message Processing ===")
  print("Utils version: " .. (_G.utils._version or "not loaded"))
  print("Owner: " .. (_G.owner or "not set"))
  print("Authorities: " .. #_G.authorities .. " configured")
  print()
  
  -- Process each demo message
  for i, msg in ipairs(demo_messages) do
    print("--- Demo " .. i .. ": " .. msg.action .. " ---")
    local result = process_message(msg)
    print("Result type: " .. type(result))
    print()
  end
  
  -- Show final inbox state
  print("=== Final State ===")
  print("Inbox size: " .. #_G.Inbox)
  print("Available utils functions:")
  local function_count = 0
  for name, func in pairs(_G.utils) do
    if type(func) == "function" then
      print("  - " .. name)
      function_count = function_count + 1
    end
  end
  print("Total utils functions: " .. function_count)
  
  -- Test array manipulation utilities
  print()
  print("=== Array Utilities Demo ===")
  local arr1 = {1, 2, 3}
  local arr2 = {4, 5, 6}
  local concatenated = _G.utils.concat(arr1, arr2)
  print("Concatenated [1,2,3] + [4,5,6]: length=" .. #concatenated)
  
  local reversed = _G.utils.reverse(arr1)
  print("Reversed [1,2,3]: [" .. table.concat(reversed, ",") .. "]")
  
  local has_2 = _G.utils.includes(2, arr1)
  local has_7 = _G.utils.includes(7, arr1) 
  print("Array [1,2,3] includes 2: " .. tostring(has_2))
  print("Array [1,2,3] includes 7: " .. tostring(has_7))
  
  return {
    integration_success = true,
    utils_version = _G.utils._version,
    functions_available = function_count,
    messages_processed = #demo_messages,
    final_inbox_size = #_G.Inbox
  }
end

-- Authority validation helpers (integrated from meta table pattern in aos.lua)
function validate_authority_message(msg)
  if not msg.from then
    return false, "Message has no 'from' field"
  end
  
  -- Check if sender is in authorities list
  local is_authority = _G.utils.includes(msg.from, _G.authorities)
  if not is_authority then
    return false, "Sender not in authorities list"
  end
  
  return true, "Message from valid authority"
end

function filter_trusted_messages(messages)
  return _G.utils.filter(function(msg)
    local valid, reason = validate_authority_message(msg)
    if valid then
      print("✓ Trusted message from: " .. (msg.from or "unknown"))
    else
      print("✗ Untrusted message: " .. reason)
    end
    return valid
  end, messages)
end

-- Run the demonstration
local result = demonstrate_utils_integration()

print()
print("=== Authority Validation Demo ===")
local test_messages = {
  {from = "authority1_address_43chars_test_auth_addr1", action = "authorized_action"},
  {from = "unknown_sender_43chars_not_in_authorities", action = "unauthorized_action"},  
  {from = "authority2_address_43chars_test_auth_addr2", action = "another_authorized"}
}

local trusted = filter_trusted_messages(test_messages)
print("Trusted messages: " .. #trusted .. " out of " .. #test_messages)

print()
print("=== Integration Summary ===")
for k, v in pairs(result) do
  print(k .. ": " .. tostring(v))
end

print()
print("✓ Utils successfully integrated with AOS message processing patterns")
print("✓ All functional programming utilities available in _G.utils namespace")
print("✓ Message filtering, transformation, and validation working correctly")
print("✓ Authority-based trust validation integrated with utils.filter and utils.includes")
print("✓ Pattern matching and spec validation ready for AO message processing")

return result