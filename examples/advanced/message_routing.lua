--- Message Routing Example for Hyper-AOS
-- Demonstrates advanced message processing patterns including routing,
-- forwarding, broadcasting, and inter-process communication in the AO ecosystem.
-- Shows how to build sophisticated message flows and process coordination.
--
-- This example implements a message router that can:
-- - Route messages to other processes based on patterns
-- - Forward messages with transformation
-- - Broadcast to multiple recipients
-- - Implement request-response patterns
-- - Handle complex message workflows
--
-- @module message_routing
-- @version 1.0.0

-- Initialize routing state in _G namespace
_G.routes = _G.routes or {}  -- Routing rules
_G.process_registry = _G.process_registry or {}  -- Known processes
_G.message_queue = _G.message_queue or {}  -- Pending messages
_G.routing_stats = _G.routing_stats or {
  messages_routed = 0,
  messages_forwarded = 0,
  broadcasts_sent = 0,
  routing_errors = 0
}
_G.pending_responses = _G.pending_responses or {}  -- Request tracking

--- Helper function to validate process ID format
-- @param process_id string The process ID to validate
-- @return boolean True if valid 43-character Arweave address
local function is_valid_process_id(process_id)
  return type(process_id) == "string" and #process_id == 43
end

--- Helper function to match message against routing pattern
-- @param msg table The message to check
-- @param pattern table The routing pattern to match against
-- @return boolean True if message matches pattern
local function matches_routing_pattern(msg, pattern)
  -- Check action pattern
  if pattern.action and msg.Action ~= pattern.action then
    return false
  end
  
  -- Check sender pattern (supports wildcards)
  if pattern.from then
    if pattern.from == "*" then
      -- Wildcard matches any sender
    elseif pattern.from ~= msg.From then
      return false
    end
  end
  
  -- Check target pattern
  if pattern.target and msg.Target ~= pattern.target then
    return false
  end
  
  -- Check custom tag patterns
  if pattern.tags then
    for tag_name, tag_value in pairs(pattern.tags) do
      if msg[tag_name] ~= tag_value then
        return false
      end
    end
  end
  
  return true
end

--- Add a routing rule
-- Action: "AddRoute" with RouteId, Pattern, and Destination tags
if Msg.Action == "AddRoute" then
  local route_id = Msg.RouteId
  local pattern_str = Msg.Pattern  -- JSON-encoded pattern
  local destination = Msg.Destination
  local transform_str = Msg.Transform  -- Optional transformation rules
  
  -- Validate required parameters
  if not route_id or not pattern_str or not destination then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Missing required parameters: RouteId, Pattern, and Destination",
      ["Error-Type"] = "ValidationError"
    })
    return
  end
  
  -- Validate destination process ID
  if not is_valid_process_id(destination) then
    Send({
      Target = Msg.From,
      Action = "Error", 
      Data = "Invalid destination process ID format",
      ["Error-Type"] = "InvalidProcessId"
    })
    return
  end
  
  -- Parse pattern (simplified JSON parsing for demo)
  local pattern = {}
  -- In a real implementation, you'd use a proper JSON parser
  -- For demo purposes, we'll accept simple key=value format
  if string.find(pattern_str, "action=") then
    pattern.action = string.match(pattern_str, "action=([^,]+)")
  end
  if string.find(pattern_str, "from=") then
    pattern.from = string.match(pattern_str, "from=([^,]+)")
  end
  
  -- Parse transformation rules if provided
  local transform = nil
  if transform_str then
    transform = {
      add_tags = {},
      remove_tags = {},
      modify_action = nil
    }
    -- Simple parsing for demo - in reality you'd use proper JSON
    if string.find(transform_str, "add_route_info=true") then
      transform.add_route_info = true
    end
  end
  
  -- Add the route
  _G.routes[route_id] = {
    id = route_id,
    pattern = pattern,
    destination = destination,
    transform = transform,
    created_by = Msg.From,
    created_at = os.time(),
    messages_routed = 0,
    active = true
  }
  
  Send({
    Target = Msg.From,
    Action = "AddRouteResponse",
    Data = "Route '" .. route_id .. "' added successfully",
    ["Route-Id"] = route_id,
    ["Destination"] = destination,
    ["Pattern"] = pattern_str
  })
  
  print("Added route: " .. route_id .. " -> " .. destination)
end

--- Remove a routing rule  
-- Action: "RemoveRoute" with RouteId tag
if Msg.Action == "RemoveRoute" then
  local route_id = Msg.RouteId
  
  if not route_id then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Missing required RouteId parameter",
      ["Error-Type"] = "ValidationError"
    })
    return
  end
  
  if not _G.routes[route_id] then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Route '" .. route_id .. "' not found",
      ["Error-Type"] = "RouteNotFound"
    })
    return
  end
  
  local route = _G.routes[route_id]
  _G.routes[route_id] = nil
  
  Send({
    Target = Msg.From,
    Action = "RemoveRouteResponse", 
    Data = "Route '" .. route_id .. "' removed successfully",
    ["Route-Id"] = route_id,
    ["Messages-Routed"] = tostring(route.messages_routed)
  })
  
  print("Removed route: " .. route_id)
end

--- Register a process in the process registry
-- Action: "RegisterProcess" with ProcessId and Name tags
if Msg.Action == "RegisterProcess" then
  local process_id = Msg.ProcessId
  local process_name = Msg.Name or "Unnamed"
  local description = Msg.Description or ""
  
  if not process_id then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Missing required ProcessId parameter",
      ["Error-Type"] = "ValidationError" 
    })
    return
  end
  
  if not is_valid_process_id(process_id) then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Invalid process ID format",
      ["Error-Type"] = "InvalidProcessId"
    })
    return
  end
  
  _G.process_registry[process_id] = {
    id = process_id,
    name = process_name,
    description = description,
    registered_by = Msg.From,
    registered_at = os.time(),
    last_seen = os.time()
  }
  
  Send({
    Target = Msg.From,
    Action = "RegisterProcessResponse",
    Data = "Process '" .. process_name .. "' registered successfully",
    ["Process-Id"] = process_id,
    ["Name"] = process_name
  })
  
  print("Registered process: " .. process_name .. " (" .. process_id .. ")")
end

--- Forward a message to specific destination with optional transformation
-- Action: "ForwardMessage" with Destination and MessageData tags
if Msg.Action == "ForwardMessage" then
  local destination = Msg.Destination
  local message_data = Msg.MessageData or Msg.Data
  local forward_action = Msg.ForwardAction or "ForwardedMessage"
  
  if not destination then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Missing required Destination parameter",
      ["Error-Type"] = "ValidationError"
    })
    return
  end
  
  if not is_valid_process_id(destination) then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Invalid destination process ID format", 
      ["Error-Type"] = "InvalidProcessId"
    })
    return
  end
  
  -- Create forwarded message
  local forwarded_msg = {
    Target = destination,
    Action = forward_action,
    Data = message_data,
    ["Original-Sender"] = Msg.From,
    ["Forwarded-By"] = _G.id or "Unknown",
    ["Forwarded-At"] = tostring(os.time()),
    ["Route-Type"] = "Manual-Forward"
  }
  
  -- Copy additional tags if specified
  for key, value in pairs(Msg) do
    if string.sub(key, 1, 8) == "Forward-" then
      local new_key = string.sub(key, 9)  -- Remove "Forward-" prefix
      forwarded_msg[new_key] = value
    end
  end
  
  -- Send the forwarded message
  Send(forwarded_msg)
  
  -- Update statistics
  _G.routing_stats.messages_forwarded = _G.routing_stats.messages_forwarded + 1
  
  -- Confirm to sender
  Send({
    Target = Msg.From,
    Action = "ForwardMessageResponse",
    Data = "Message forwarded to " .. destination,
    ["Destination"] = destination,
    ["Forward-Action"] = forward_action
  })
  
  print("Forwarded message to: " .. destination)
end

--- Broadcast a message to multiple destinations
-- Action: "BroadcastMessage" with Destinations (comma-separated) and MessageData tags
if Msg.Action == "BroadcastMessage" then
  local destinations_str = Msg.Destinations
  local message_data = Msg.MessageData or Msg.Data
  local broadcast_action = Msg.BroadcastAction or "BroadcastMessage"
  
  if not destinations_str then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Missing required Destinations parameter (comma-separated process IDs)",
      ["Error-Type"] = "ValidationError"
    })
    return
  end
  
  -- Parse destinations list
  local destinations = {}
  local start_pos = 1
  while true do
    local comma_pos = string.find(destinations_str, ",", start_pos)
    local dest_id
    if comma_pos then
      dest_id = string.sub(destinations_str, start_pos, comma_pos - 1)
    else
      dest_id = string.sub(destinations_str, start_pos)
    end
    
    -- Trim whitespace  
    dest_id = string.match(dest_id, "^%s*(.-)%s*$") or dest_id
    
    if is_valid_process_id(dest_id) then
      table.insert(destinations, dest_id)
    end
    
    if not comma_pos then
      break
    end
    start_pos = comma_pos + 1
  end
  
  if #destinations == 0 then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "No valid destination process IDs found",
      ["Error-Type"] = "NoValidDestinations"
    })
    return
  end
  
  -- Send broadcast to all destinations
  local broadcast_id = "bc_" .. os.time() .. "_" .. math.random(1000, 9999)
  local successful_sends = 0
  
  for _, dest_id in ipairs(destinations) do
    local broadcast_msg = {
      Target = dest_id,
      Action = broadcast_action,
      Data = message_data,
      ["Broadcast-Id"] = broadcast_id,
      ["Broadcast-From"] = Msg.From,
      ["Broadcast-By"] = _G.id or "Unknown",
      ["Broadcast-At"] = tostring(os.time()),
      ["Total-Recipients"] = tostring(#destinations)
    }
    
    Send(broadcast_msg)
    successful_sends = successful_sends + 1
  end
  
  -- Update statistics
  _G.routing_stats.broadcasts_sent = _G.routing_stats.broadcasts_sent + 1
  
  -- Confirm to sender
  Send({
    Target = Msg.From,
    Action = "BroadcastMessageResponse",
    Data = "Broadcast sent to " .. successful_sends .. " destinations",
    ["Broadcast-Id"] = broadcast_id,
    ["Recipients-Count"] = tostring(successful_sends),
    ["Recipients"] = destinations_str
  })
  
  print("Broadcast " .. broadcast_id .. " sent to " .. successful_sends .. " recipients")
end

--- Process a message through the routing engine
-- Action: "RouteMessage" with MessageData tag (processes routing rules)
if Msg.Action == "RouteMessage" then
  -- This action processes a message against all active routing rules
  local routes_matched = 0
  
  -- Check message against all active routes
  for route_id, route in pairs(_G.routes) do
    if route.active and matches_routing_pattern(Msg, route.pattern) then
      routes_matched = routes_matched + 1
      
      -- Create routed message
      local routed_msg = {
        Target = route.destination,
        Action = Msg.MessageAction or "RoutedMessage",
        Data = Msg.MessageData or Msg.Data,
        ["Original-Sender"] = Msg.From,
        ["Routed-By"] = _G.id or "Unknown",
        ["Route-Id"] = route_id,
        ["Routed-At"] = tostring(os.time())
      }
      
      -- Apply transformations if specified
      if route.transform then
        if route.transform.add_route_info then
          routed_msg["Route-Info"] = "Matched pattern for route " .. route_id
        end
        if route.transform.modify_action then
          routed_msg.Action = route.transform.modify_action
        end
      end
      
      -- Copy additional message tags
      for key, value in pairs(Msg) do
        if string.sub(key, 1, 8) == "Message-" then
          local new_key = string.sub(key, 9)  -- Remove "Message-" prefix
          routed_msg[new_key] = value
        end
      end
      
      -- Send the routed message
      Send(routed_msg)
      
      -- Update route statistics
      route.messages_routed = route.messages_routed + 1
      _G.routing_stats.messages_routed = _G.routing_stats.messages_routed + 1
      
      print("Routed message via: " .. route_id .. " -> " .. route.destination)
    end
  end
  
  -- Send response to original sender
  Send({
    Target = Msg.From,
    Action = "RouteMessageResponse",
    Data = routes_matched > 0 and ("Message routed via " .. routes_matched .. " routes") or "No matching routes found",
    ["Routes-Matched"] = tostring(routes_matched),
    ["Total-Routes"] = tostring(function() local c = 0; for _ in pairs(_G.routes) do c = c + 1 end; return c end())
  })
  
  if routes_matched == 0 then
    _G.routing_stats.routing_errors = _G.routing_stats.routing_errors + 1
  end
end

--- Send a request and expect a response (request-response pattern)
-- Action: "SendRequest" with Destination, RequestData, and Timeout tags
if Msg.Action == "SendRequest" then
  local destination = Msg.Destination
  local request_data = Msg.RequestData or Msg.Data
  local timeout = tonumber(Msg.Timeout) or 30  -- Default 30 second timeout
  local request_action = Msg.RequestAction or "Request"
  
  if not destination or not is_valid_process_id(destination) then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Missing or invalid Destination parameter",
      ["Error-Type"] = "ValidationError"
    })
    return
  end
  
  -- Generate unique request ID
  local request_id = "req_" .. os.time() .. "_" .. math.random(1000, 9999)
  
  -- Store pending request
  _G.pending_responses[request_id] = {
    original_sender = Msg.From,
    destination = destination, 
    sent_at = os.time(),
    timeout = timeout,
    request_data = request_data
  }
  
  -- Send the request
  Send({
    Target = destination,
    Action = request_action,
    Data = request_data,
    ["Request-Id"] = request_id,
    ["Response-Target"] = _G.id or "Unknown",
    ["Request-From"] = Msg.From,
    ["Timeout"] = tostring(timeout)
  })
  
  -- Confirm request sent
  Send({
    Target = Msg.From,
    Action = "SendRequestResponse",
    Data = "Request sent to " .. destination,
    ["Request-Id"] = request_id,
    ["Destination"] = destination,
    ["Timeout"] = tostring(timeout)
  })
  
  print("Sent request " .. request_id .. " to " .. destination)
end

--- Handle incoming response messages
-- Action: "Response" with Request-Id tag
if Msg.Action == "Response" then
  local request_id = Msg["Request-Id"]
  
  if not request_id then
    print("Received response without Request-Id")
    return
  end
  
  local pending = _G.pending_responses[request_id]
  if not pending then
    print("Received response for unknown request: " .. request_id)
    return
  end
  
  -- Forward response to original requester
  Send({
    Target = pending.original_sender,
    Action = "RequestResponse",
    Data = Msg.Data,
    ["Request-Id"] = request_id,
    ["Response-From"] = Msg.From,
    ["Response-Time"] = tostring(os.time() - pending.sent_at) .. " seconds"
  })
  
  -- Clean up pending request
  _G.pending_responses[request_id] = nil
  
  print("Forwarded response for request " .. request_id)
end

--- Get routing statistics and status
-- Action: "RoutingStatus"
if Msg.Action == "RoutingStatus" then
  local active_routes = 0
  local total_route_messages = 0
  
  for _, route in pairs(_G.routes) do
    if route.active then
      active_routes = active_routes + 1
      total_route_messages = total_route_messages + route.messages_routed
    end
  end
  
  local registered_processes = 0
  for _ in pairs(_G.process_registry) do
    registered_processes = registered_processes + 1
  end
  
  local pending_requests = 0
  for _ in pairs(_G.pending_responses) do
    pending_requests = pending_requests + 1
  end
  
  Send({
    Target = Msg.From,
    Action = "RoutingStatusResponse",
    Data = "Routing Engine Status Report",
    ["Active-Routes"] = tostring(active_routes),
    ["Total-Route-Messages"] = tostring(total_route_messages),
    ["Messages-Routed"] = tostring(_G.routing_stats.messages_routed),
    ["Messages-Forwarded"] = tostring(_G.routing_stats.messages_forwarded),
    ["Broadcasts-Sent"] = tostring(_G.routing_stats.broadcasts_sent),
    ["Routing-Errors"] = tostring(_G.routing_stats.routing_errors),
    ["Registered-Processes"] = tostring(registered_processes),
    ["Pending-Requests"] = tostring(pending_requests)
  })
  
  print("Routing status sent to " .. Msg.From)
end

--- Help for message routing system
-- Action: "Help"
if Msg.Action == "Help" then
  local help_text = [[
Message Routing System Help:

Route Management:
- AddRoute: Create routing rule (RouteId, Pattern, Destination)
- RemoveRoute: Delete routing rule (RouteId)
- RoutingStatus: Get system statistics and status

Process Registry:  
- RegisterProcess: Add process to registry (ProcessId, Name, Description)

Message Operations:
- ForwardMessage: Send message to specific destination (Destination, MessageData)
- BroadcastMessage: Send to multiple destinations (Destinations, MessageData)
- RouteMessage: Process message through routing rules (MessageData)

Request-Response:
- SendRequest: Send request expecting response (Destination, RequestData, Timeout)
- Response: Handle incoming response (Request-Id)

Pattern Matching:
Routes match messages based on:
- action=ActionName: Match specific action
- from=ProcessId or from=*: Match sender (wildcard supported)
- Custom tag patterns

Examples:
- Add route: Action="AddRoute", RouteId="log_route", Pattern="action=Log", Destination="<LOG_PROCESS_ID>"
- Forward: Action="ForwardMessage", Destination="<DEST_ID>", MessageData="Hello"
- Broadcast: Action="BroadcastMessage", Destinations="<ID1>,<ID2>", MessageData="Announcement"
- Request: Action="SendRequest", Destination="<DEST_ID>", RequestData="Query", Timeout="10"

Features:
✓ Pattern-based message routing
✓ Message transformation and forwarding
✓ Multi-destination broadcasting
✓ Request-response coordination
✓ Process registry and discovery
✓ Comprehensive statistics and monitoring
  ]]
  
  Send({
    Target = Msg.From,
    Action = "HelpResponse",
    Data = help_text
  })
  
  print("Help sent to " .. (Msg.From or "Unknown"))
end

--- Cleanup expired requests (run periodically)
-- This would typically be triggered by a timer or periodic message
local current_time = os.time()
for request_id, request in pairs(_G.pending_responses) do
  if current_time - request.sent_at > request.timeout then
    -- Notify original sender of timeout
    Send({
      Target = request.original_sender,
      Action = "RequestTimeout",
      Data = "Request " .. request_id .. " timed out after " .. request.timeout .. " seconds",
      ["Request-Id"] = request_id,
      ["Destination"] = request.destination,
      ["Timeout"] = tostring(request.timeout)
    })
    
    -- Remove expired request
    _G.pending_responses[request_id] = nil
    
    print("Request " .. request_id .. " timed out")
  end
end