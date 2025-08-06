--- Basic Hello World Example for Hyper-AOS
-- This demonstrates the simplest possible AOS message handler that responds
-- to messages with a greeting. Shows fundamental concepts of message processing
-- and response generation in the AO ecosystem.
--
-- Usage:
--   1. Deploy this script to an AOS process
--   2. Send a message with Action = "Hello" 
--   3. The process will respond with a greeting
--
-- @module hello_world
-- @version 1.0.0

-- Initialize process state if not already done
_G.greetings_sent = _G.greetings_sent or 0

--- Simple greeting handler
-- Responds to "Hello" actions with a personalized greeting
-- Demonstrates basic message pattern matching and response
if Msg.Action == "Hello" then
  -- Extract sender information
  local sender = Msg.From or "Anonymous"
  
  -- Increment greeting counter (demonstrates state persistence)
  _G.greetings_sent = _G.greetings_sent + 1
  
  -- Create personalized greeting
  local greeting = "Hello, " .. sender .. "! This is greeting #" .. _G.greetings_sent
  
  -- Send response back to sender
  -- This demonstrates the basic AO message sending pattern
  Send({
    Target = Msg.From,
    Action = "HelloResponse",
    Data = greeting,
    ["Greeting-Number"] = tostring(_G.greetings_sent)
  })
  
  -- Print confirmation to process console
  print("Sent greeting #" .. _G.greetings_sent .. " to " .. sender)
end

--- Status inquiry handler
-- Responds to "Status" actions with current process state
-- Demonstrates reading and reporting internal state
if Msg.Action == "Status" then
  local status_report = {
    process_id = _G.id or "Unknown",
    owner = _G.owner or "Unknown", 
    total_greetings = _G.greetings_sent,
    message_count = #_G.Inbox or 0
  }
  
  -- Send status response
  Send({
    Target = Msg.From,
    Action = "StatusResponse", 
    Data = "Process Status Report",
    ["Process-ID"] = status_report.process_id,
    ["Owner"] = status_report.owner,
    ["Total-Greetings"] = tostring(status_report.total_greetings),
    ["Message-Count"] = tostring(status_report.message_count)
  })
  
  print("Status report sent to " .. (Msg.From or "Unknown"))
end

--- Help handler
-- Provides usage instructions to users
-- Demonstrates self-documenting process behavior
if Msg.Action == "Help" then
  local help_text = [[
Hello World Process Help:

Available Actions:
- Hello: Send a greeting (responds with personalized message)
- Status: Get process status and statistics  
- Help: Display this help message

Examples:
- Send message with Action = "Hello" to receive a greeting
- Send message with Action = "Status" to see process state
- Send message with Action = "Help" to see this help

This process demonstrates:
✓ Basic message handling patterns
✓ State persistence across messages
✓ Response message generation
✓ Process introspection
  ]]
  
  Send({
    Target = Msg.From,
    Action = "HelpResponse",
    Data = help_text
  })
  
  print("Help sent to " .. (Msg.From or "Unknown"))
end

--- Default handler for unknown actions
-- Demonstrates graceful handling of unexpected messages
-- Good practice for robust AO processes
if Msg.Action and Msg.Action ~= "Hello" and Msg.Action ~= "Status" and Msg.Action ~= "Help" then
  Send({
    Target = Msg.From,
    Action = "Error",
    Data = "Unknown action: " .. Msg.Action .. ". Send 'Help' for available actions.",
    ["Error-Type"] = "UnknownAction",
    ["Original-Action"] = Msg.Action
  })
  
  print("Unknown action '" .. Msg.Action .. "' from " .. (Msg.From or "Unknown"))
end