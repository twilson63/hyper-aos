--- State Management Example for Hyper-AOS
-- Demonstrates comprehensive state persistence, management, and manipulation
-- in the AO environment. Shows how data persists across message processing
-- cycles and best practices for state organization.
--
-- This example implements a simple key-value store with versioning,
-- demonstrating various state management patterns used in production AOS processes.
--
-- @module state_management
-- @version 1.0.0

-- Initialize persistent state structures in _G namespace
-- All data stored in _G persists across message processing cycles
_G.store = _G.store or {}  -- Main key-value store
_G.store_history = _G.store_history or {}  -- Version history
_G.store_metadata = _G.store_metadata or {
  created_at = _G.store_metadata and _G.store_metadata.created_at or os.time(),
  version = 0,
  total_operations = 0,
  last_backup = nil
}

--- Helper function to create a timestamped history entry
-- @param action string The action performed (set, delete, etc.)
-- @param key string The key affected
-- @param old_value any The previous value (nil for new keys)
-- @param new_value any The new value (nil for deletions)
-- @return table History entry with timestamp and metadata
local function create_history_entry(action, key, old_value, new_value)
  return {
    timestamp = os.time(),
    action = action,
    key = key,
    old_value = old_value,
    new_value = new_value,
    version = _G.store_metadata.version + 1
  }
end

--- Set a value in the store with history tracking
-- Action: "Set" with Key and Value tags
if Msg.Action == "Set" then
  local key = Msg.Key
  local value = Msg.Value
  
  -- Validate required parameters
  if not key then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Missing required 'Key' parameter for Set operation",
      ["Error-Type"] = "ValidationError"
    })
    return
  end
  
  -- Store old value for history
  local old_value = _G.store[key]
  
  -- Update the store
  _G.store[key] = value
  
  -- Update metadata
  _G.store_metadata.version = _G.store_metadata.version + 1
  _G.store_metadata.total_operations = _G.store_metadata.total_operations + 1
  
  -- Add to history
  table.insert(_G.store_history, create_history_entry("set", key, old_value, value))
  
  -- Limit history size to prevent unbounded growth
  local max_history = 100
  if #_G.store_history > max_history then
    table.remove(_G.store_history, 1)
  end
  
  -- Send confirmation
  Send({
    Target = Msg.From,
    Action = "SetResponse",
    Data = "Successfully set key '" .. key .. "' to value '" .. tostring(value) .. "'",
    ["Key"] = key,
    ["Value"] = tostring(value),
    ["Version"] = tostring(_G.store_metadata.version),
    ["Previous-Value"] = old_value and tostring(old_value) or "null"
  })
  
  print("Set: " .. key .. " = " .. tostring(value) .. " (v" .. _G.store_metadata.version .. ")")
end

--- Get a value from the store
-- Action: "Get" with Key tag
if Msg.Action == "Get" then
  local key = Msg.Key
  
  if not key then
    Send({
      Target = Msg.From,
      Action = "Error", 
      Data = "Missing required 'Key' parameter for Get operation",
      ["Error-Type"] = "ValidationError"
    })
    return
  end
  
  local value = _G.store[key]
  local exists = value ~= nil
  
  Send({
    Target = Msg.From,
    Action = "GetResponse",
    Data = exists and tostring(value) or "Key not found",
    ["Key"] = key,
    ["Value"] = exists and tostring(value) or "null",
    ["Exists"] = tostring(exists),
    ["Store-Version"] = tostring(_G.store_metadata.version)
  })
  
  print("Get: " .. key .. " = " .. (exists and tostring(value) or "null"))
end

--- Delete a key from the store
-- Action: "Delete" with Key tag  
if Msg.Action == "Delete" then
  local key = Msg.Key
  
  if not key then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Missing required 'Key' parameter for Delete operation", 
      ["Error-Type"] = "ValidationError"
    })
    return
  end
  
  local old_value = _G.store[key]
  local existed = old_value ~= nil
  
  if existed then
    _G.store[key] = nil
    
    -- Update metadata
    _G.store_metadata.version = _G.store_metadata.version + 1
    _G.store_metadata.total_operations = _G.store_metadata.total_operations + 1
    
    -- Add to history
    table.insert(_G.store_history, create_history_entry("delete", key, old_value, nil))
  end
  
  Send({
    Target = Msg.From,
    Action = "DeleteResponse",
    Data = existed and ("Deleted key '" .. key .. "'") or ("Key '" .. key .. "' not found"),
    ["Key"] = key,
    ["Existed"] = tostring(existed),
    ["Previous-Value"] = existed and tostring(old_value) or "null",
    ["Version"] = tostring(_G.store_metadata.version)
  })
  
  print("Delete: " .. key .. " (existed: " .. tostring(existed) .. ")")
end

--- List all keys in the store
-- Action: "List" 
if Msg.Action == "List" then
  local keys = {}
  local count = 0
  
  for key, _ in pairs(_G.store) do
    table.insert(keys, key)
    count = count + 1
  end
  
  -- Sort keys for consistent output
  table.sort(keys)
  
  local keys_string = table.concat(keys, ", ")
  
  Send({
    Target = Msg.From,
    Action = "ListResponse",
    Data = count > 0 and keys_string or "Store is empty",
    ["Key-Count"] = tostring(count),
    ["Keys"] = keys_string,
    ["Store-Version"] = tostring(_G.store_metadata.version)
  })
  
  print("List: " .. count .. " keys - [" .. keys_string .. "]")
end

--- Get store statistics and metadata
-- Action: "Stats"
if Msg.Action == "Stats" then
  local key_count = 0
  for _ in pairs(_G.store) do
    key_count = key_count + 1
  end
  
  local stats = {
    key_count = key_count,
    version = _G.store_metadata.version,
    total_operations = _G.store_metadata.total_operations,
    created_at = _G.store_metadata.created_at,
    history_entries = #_G.store_history,
    uptime = os.time() - _G.store_metadata.created_at
  }
  
  Send({
    Target = Msg.From,
    Action = "StatsResponse", 
    Data = "Store Statistics",
    ["Key-Count"] = tostring(stats.key_count),
    ["Version"] = tostring(stats.version),
    ["Total-Operations"] = tostring(stats.total_operations),
    ["Created-At"] = tostring(stats.created_at),
    ["History-Entries"] = tostring(stats.history_entries),
    ["Uptime-Seconds"] = tostring(stats.uptime)
  })
  
  print("Stats: " .. key_count .. " keys, v" .. stats.version .. ", " .. stats.total_operations .. " ops")
end

--- Get operation history
-- Action: "History" with optional Limit tag
if Msg.Action == "History" then
  local limit = tonumber(Msg.Limit) or 10
  local history_count = #_G.store_history
  
  if history_count == 0 then
    Send({
      Target = Msg.From,
      Action = "HistoryResponse",
      Data = "No history available",
      ["Entry-Count"] = "0"
    })
    return
  end
  
  -- Get recent entries (up to limit)
  local start_index = math.max(1, history_count - limit + 1)
  local recent_entries = {}
  
  for i = start_index, history_count do
    local entry = _G.store_history[i]
    table.insert(recent_entries, {
      version = entry.version,
      action = entry.action, 
      key = entry.key,
      timestamp = entry.timestamp
    })
  end
  
  -- Create a simple string representation of history
  local history_lines = {}
  for _, entry in ipairs(recent_entries) do
    local line = "v" .. entry.version .. ": " .. entry.action .. " '" .. entry.key .. "' at " .. entry.timestamp
    table.insert(history_lines, line)
  end
  
  Send({
    Target = Msg.From,
    Action = "HistoryResponse",
    Data = table.concat(history_lines, "\n"),
    ["Entry-Count"] = tostring(#recent_entries),
    ["Total-History"] = tostring(history_count),
    ["Limit"] = tostring(limit)
  })
  
  print("History: " .. #recent_entries .. "/" .. history_count .. " entries sent")
end

--- Clear the entire store (dangerous operation!)
-- Action: "Clear" with Confirm = "yes"
if Msg.Action == "Clear" then
  if Msg.Confirm ~= "yes" then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Clear operation requires Confirm = 'yes' parameter for safety",
      ["Error-Type"] = "ConfirmationRequired"
    })
    return
  end
  
  local key_count = 0
  for _ in pairs(_G.store) do
    key_count = key_count + 1
  end
  
  -- Clear the store
  _G.store = {}
  
  -- Update metadata  
  _G.store_metadata.version = _G.store_metadata.version + 1
  _G.store_metadata.total_operations = _G.store_metadata.total_operations + 1
  
  -- Add clear operation to history
  table.insert(_G.store_history, create_history_entry("clear", "ALL", key_count, 0))
  
  Send({
    Target = Msg.From,
    Action = "ClearResponse",
    Data = "Store cleared successfully. " .. key_count .. " keys removed.",
    ["Keys-Removed"] = tostring(key_count),
    ["Version"] = tostring(_G.store_metadata.version)
  })
  
  print("Clear: " .. key_count .. " keys removed (v" .. _G.store_metadata.version .. ")")
end

--- Help for state management operations
-- Action: "Help"
if Msg.Action == "Help" then
  local help_text = [[
State Management Process Help:

Available Actions:
- Set: Store a value (requires Key and Value tags)
- Get: Retrieve a value (requires Key tag)  
- Delete: Remove a key (requires Key tag)
- List: Show all keys in the store
- Stats: Display store statistics and metadata
- History: Show recent operations (optional Limit tag, default 10)
- Clear: Remove all data (requires Confirm = "yes")
- Help: Display this help message

Examples:
- Set a value: Action = "Set", Key = "name", Value = "Alice"
- Get a value: Action = "Get", Key = "name"  
- Delete a key: Action = "Delete", Key = "name"
- View history: Action = "History", Limit = "5"
- Clear store: Action = "Clear", Confirm = "yes"

Features:
✓ Persistent state across messages
✓ Operation history and versioning  
✓ Comprehensive statistics
✓ Safe clear operations with confirmation
✓ Input validation and error handling
  ]]
  
  Send({
    Target = Msg.From,
    Action = "HelpResponse",
    Data = help_text
  })
  
  print("Help sent to " .. (Msg.From or "Unknown"))
end