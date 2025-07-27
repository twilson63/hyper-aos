-- set version for hyper-aos
_G.package.loaded['.process'] = { _version = "dev" }

-- Initialize global state variables directly in _G
-- These will be persisted across compute calls
_G.Inbox = _G.Inbox or {}
_G.MAX_INBOX_SIZE = 10000
_G._OUTPUT = ""

-- Private functions table
-- This table is local to this module and cannot be accessed from eval() or external code
-- We keep meta separate as it contains private functions and initialization state
---@diagnostic disable-next-line
local meta = { initialized = false }

-- List of Lua built-in keys to exclude when serializing state
-- This ensures we only return user data, not system functions/tables
local SYSTEM_KEYS = {
  -- Lua built-in functions
  "assert", "collectgarbage", "dofile", "error", "getmetatable", "ipairs", 
  "load", "loadfile", "loadstring", "next", "pairs", "pcall", "print", 
  "rawequal", "rawget", "rawlen", "rawset", "require", "select", 
  "setmetatable", "tonumber", "tostring", "type", "xpcall", "_VERSION",
  -- Lua built-in libraries
  "coroutine", "debug", "io", "math", "os", "package", "string", "table", "utf8",
  -- AOS specific functions that shouldn't be serialized
  "compute", "eval", "send", "prompt", "removeCR", "isSimpleArray", "stringify",
  -- Private/temporary variables
  "_OUTPUT", "MAX_INBOX_SIZE", "SYSTEM_KEYS", "meta",
  -- These will be handled specially or excluded
  "State", "_G"
}
--- Initialize process state from the first Process message
-- Stores owner, id, and authorities directly in _G
-- @param msg table The incoming message to process
function meta.init(msg)
  -- Initialize owner from first Process message
  if not meta.initialized and msg.type and string.lower(msg.type) == "process" and msg.commitments then
    -- Find first non-hmac commitment and set its committer as owner
    for key, commitment in pairs(msg.commitments) do
      if commitment.type and string.lower(commitment.type) ~= "hmac-sha256" and commitment.committer then
        -- Store process id and owner directly in _G
        _G.id = key
        _G.owner = commitment.committer
        meta.initialized = true
        
        -- Initialize authorities array in _G
        _G.authorities = _G.authorities or {}
        
        -- Parse authorities from comma-separated string
        if msg.authority then
          -- Split comma-separated authorities string manually
          local authorities_str = msg.authority
          local start_pos = 1
          while true do
            local comma_pos = string.find(authorities_str, ",", start_pos)
            local authority
            if comma_pos then
              authority = string.sub(authorities_str, start_pos, comma_pos - 1)
            else
              authority = string.sub(authorities_str, start_pos)
            end
            
            -- Trim whitespace
            authority = string.match(authority, "^%s*(.-)%s*$") or authority
            
            -- Check if it's 43 characters (valid Arweave address)
            if #authority == 43 then
              table.insert(_G.authorities, authority)
            end
            
            if not comma_pos then
              break
            end
            start_pos = comma_pos + 1
          end
        end
        
        break
      end
    end
  end
  
  -- Initialize colors table with terminal escape codes in _G
  if not _G.colors then
    _G.colors = {
      -- Reset
      reset = "\27[0m",
      
      -- Regular colors
      black = "\27[30m",
      red = "\27[31m",
      green = "\27[32m",
      yellow = "\27[33m",
      blue = "\27[34m",
      magenta = "\27[35m",
      cyan = "\27[36m",
      white = "\27[37m",
      
      -- Bright colors
      bright_black = "\27[90m",
      bright_red = "\27[91m",
      bright_green = "\27[92m",
      bright_yellow = "\27[93m",
      bright_blue = "\27[94m",
      bright_magenta = "\27[95m",
      bright_cyan = "\27[96m",
      bright_white = "\27[97m",
      
      -- Background colors
      bg_black = "\27[40m",
      bg_red = "\27[41m",
      bg_green = "\27[42m",
      bg_yellow = "\27[43m",
      bg_blue = "\27[44m",
      bg_magenta = "\27[45m",
      bg_cyan = "\27[46m",
      bg_white = "\27[47m",
      
      -- Text styles
      bold = "\27[1m",
      dim = "\27[2m",
      italic = "\27[3m",
      underline = "\27[4m",
      blink = "\27[5m",
      reverse = "\27[7m",
      hidden = "\27[8m",
      strikethrough = "\27[9m"
    }
  end

end

--- Check if a message is trusted based on authorities
-- A message is trusted if it has from-process equal to from 
-- and the committer is in the authorities list
-- @param msg table The message to validate
-- @return boolean True if message is trusted, false otherwise
function meta.is_trusted(msg)
  -- Check if message has both from and from-process fields
  if not msg.from or not msg["from-process"] then
    return false
  end
  
  -- Check if from equals from-process
  if msg.from ~= msg["from-process"] then
    return false
  end
  
  -- Check if any commitment's committer is in the authorities list
  if msg.commitments and _G.authorities then
    for _, commitment in pairs(msg.commitments) do
      if commitment.committer then
        -- Check if this committer is in the authorities list
        for _, authority in ipairs(_G.authorities) do
          if commitment.committer == authority then
            return true
          end
        end
      end
    end
  end
  
  return false
end

-- Private function to ensure message has a 'from' field and check trust
-- Sets msg.from based on from-process or first non-HMAC signed commitment
-- Also sets msg.trusted based on authorities verification
function meta.ensure_message(msg)
  -- If message already has 'from', leave it as is
  if msg.from then
    -- Still need to check trust even if from exists
    msg.trusted = meta.is_trusted(msg)
    return msg
  end
  -- First check if there's a from-process field
  if msg["from-process"] then
    msg.from = msg["from-process"]
    -- Check trust after setting from
    msg.trusted = meta.is_trusted(msg)
    return msg
  end
  -- Otherwise, find the first non-HMAC signed commitment's committer
  if msg.commitments then
    for key, commitment in pairs(msg.commitments) do
      if commitment.type and commitment.committer then
        -- Skip HMAC commitments
        if string.lower(commitment.type) ~= "hmac-sha256" then
          msg.from = commitment.committer
        end
      end
    end
  end
  -- If no from-process and no non-HMAC commitments, from remains nil
  -- Check trust after all from logic
  msg.trusted = meta.is_trusted(msg)
  return msg
end

--- Check if message has valid owner commitment
-- Validates that the message's from matches the global owner
-- @param msg table The message to validate
-- @return boolean True if message is from owner, false otherwise
function meta.is_owner(msg)
  -- Ensure message has 'from' field
  meta.ensure_message(msg)
  
  -- Check if msg.from matches the owner stored in _G
  if msg.from and _G.owner and msg.from == _G.owner then
    return true
  end
  
  return false
end

-- override print function with colorized table support
---@diagnostic disable-next-line
function print(...)
  local args = {...}
  local output = {}
  
  for i, v in ipairs(args) do
    if type(v) == "table" then
      table.insert(output, stringify(v))
    else
      table.insert(output, tostring(v))
    end
  end
  
  _OUTPUT = _OUTPUT .. table.concat(output, "\t") .. "\n"
end

-- utility function to remove last CR
---@diagnostic disable-next-line
function removeCR(str)
    if str:sub(-1) == "\r" or str:sub(-1) == "\n" then
        return str:sub(1, -2)
    end
    return str
end

-- stringify utilities for colorized table printing
---@diagnostic disable-next-line
function isSimpleArray(tbl)
  local arrayIndex = 1
  for k, v in pairs(tbl) do
    if k ~= arrayIndex or (type(v) ~= "number" and type(v) ~= "string") then
      return false
    end
    arrayIndex = arrayIndex + 1
  end
  return true
end

---@diagnostic disable-next-line
function stringify(tbl, indent, visited)
  -- Handle non-table types
  if type(tbl) ~= "table" then
    if type(tbl) == "string" then
      return _G.colors.green .. '"' .. tbl .. '"' .. _G.colors.reset
    else
      return _G.colors.blue .. tostring(tbl) .. _G.colors.reset
    end
  end
  
  indent = indent or 0
  local toIndent = string.rep(" ", indent)
  local toIndentChild = string.rep(" ", indent + 2)

  local result = {}
  local isArray = true
  local arrayIndex = 1

  -- Handle simple arrays
  if isSimpleArray(tbl) then
    for _, v in ipairs(tbl) do
      if type(v) == "string" then
        v = _G.colors.green .. '"' .. v .. '"' .. _G.colors.reset
      else
        v = _G.colors.blue .. tostring(v) .. _G.colors.reset
      end
      table.insert(result, v)
    end
    return "{ " .. table.concat(result, ", ") .. " }"
  end

  -- Handle complex tables
  for k, v in pairs(tbl) do
    if isArray then
      if k == arrayIndex then
        arrayIndex = arrayIndex + 1
        if type(v) == "table" then
          v = stringify(v, indent + 2, visited)
        elseif type(v) == "string" then
          v = _G.colors.green .. '"' .. v .. '"' .. _G.colors.reset
        else
          v = _G.colors.blue .. tostring(v) .. _G.colors.reset
        end
        table.insert(result, toIndentChild .. v)
      else
        isArray = false
        result = {}
      end
    end
    if not isArray then
      if type(v) == "table" then
        visited = visited or {}
        if visited[v] then
            v = _G.colors.dim .. "<circular reference>" .. _G.colors.reset
        else
          visited[v] = true
          v = stringify(v, indent + 2, visited)
        end
      elseif type(v) == "string" then
        v = _G.colors.green .. '"' .. v .. '"' .. _G.colors.reset
      else
        v = _G.colors.blue .. tostring(v) .. _G.colors.reset
      end
      -- Format key with color
      local keyStr = tostring(k)
      if type(k) == "string" then
        keyStr = _G.colors.red .. keyStr .. _G.colors.reset
      else
        keyStr = _G.colors.yellow .. "[" .. keyStr .. "]" .. _G.colors.reset
      end
      table.insert(result, toIndentChild .. keyStr .. " = " .. v)
    end
  end

  local prefix = isArray and "{\n" or "{\n"
  local suffix = isArray and "\n" .. toIndent .. "}" or "\n" .. toIndent .. "}"
  local separator = isArray and ",\n" or ",\n"
  return prefix .. table.concat(result, separator) .. suffix
end

-- prompt function for console with colors
---@diagnostic disable-next-line
function prompt()
  -- Use colors if available, otherwise fallback to plain text
  if _G.colors and _G.colors.cyan then
    local c = _G.colors
    return c.cyan .. c.bold .. "hyper" .. c.reset .. 
           c.white .. "~" .. c.reset .. 
           c.bright_green .. "aos" .. c.reset .. 
           c.white .. "@" .. c.reset .. 
           c.yellow .. require('.process')._version .. c.reset .. 
           c.white .. "[" .. c.reset .. 
           c.bright_magenta .. #Inbox .. c.reset .. 
           c.white .. "]" .. c.reset .. 
           c.bright_blue .. "> " .. c.reset
  else
    return "hyper~aos@" .. require('.process')._version .. "[" .. #Inbox .. "]> "
  end
end

-- send function for dispatching messages to other processes
---@diagnostic disable-next-line
function send(msg)
  -- Initialize results table if needed
  _G.results = _G.results or {}
  _G.results.outbox = _G.results.outbox or {}
  table.insert(_G.results.outbox, msg)
end

-- eval function, this function allows you update your process
---@diagnostic disable-next-line
function eval(msg)
  -- Security check: validate commitments
  if not meta.is_owner(msg) then
    print("Unauthorized: eval requires owner signed message")
    return "ok"
  end
  -- Original eval logic
  local expr = msg.body or msg.data or ""
  local func, err = load("return " .. expr, 'aos', 't', _G)
  local output = ""
  local e = nil
  if err then
    func, err = load(expr, 'aos', 't', _G)
  end
  if func then
    output, e = func()
  else
    return err
  end

  if e then
    return e
  end

  return output
end

--- Recursively copy a table, handling circular references
-- @param tbl table The table to copy
-- @param visited table Table tracking visited tables for circular reference detection
-- @return table The copied table
local function copy_table_recursive(tbl, visited)
  local copy = {}
  
  for k, v in pairs(tbl) do
    local value_type = type(v)
    
    if value_type == "table" then
      -- Check for circular reference
      if visited[v] then
        copy[k] = "<circular reference>"
      else
        -- Mark this table as visited
        visited[v] = true
        -- Recursively copy the table
        copy[k] = copy_table_recursive(v, visited)
        -- Unmark after processing
        visited[v] = nil
      end
    elseif value_type ~= "function" then
      -- Copy non-function values
      copy[k] = v
    end
    -- Skip functions entirely
  end
  
  return copy
end

--- Extract user state from _G, filtering out system keys and functions
-- Handles circular references properly to avoid infinite loops
-- @param visited table Optional table to track visited tables for circular reference detection
-- @return table The filtered state containing only user data
local function extract_state_from_global(visited)
  visited = visited or {}
  local state = {}
  
  -- Create a lookup table for system keys for O(1) access
  local system_keys_set = {}
  for _, key in ipairs(SYSTEM_KEYS) do
    system_keys_set[key] = true
  end
  
  -- Iterate through all keys in _G
  for key, value in pairs(_G) do
    -- Skip system keys and functions
    if not system_keys_set[key] and type(value) ~= "function" then
      local value_type = type(value)
      
      if value_type == "table" then
        -- Check for circular reference
        if visited[value] then
          state[key] = "<circular reference>"
        else
          -- Mark this table as visited
          visited[value] = true
          -- Recursively copy the table
          state[key] = copy_table_recursive(value, visited)
          -- Unmark after processing (allows same table in different paths)
          visited[value] = nil
        end
      else
        -- For non-table values, just copy them
        state[key] = value
      end
    end
  end
  
  return state
end

--- Main entry point for message processing
-- Processes messages and manages state directly in _G
-- @param state table The incoming state (merged into _G on first call)
-- @param assignment table The message assignment to process
-- @return string Status ("ok")
-- @return table The filtered state extracted from _G
function compute(state, assignment)
  -- Clear output buffer
  _G._OUTPUT = ""
  
  -- On first message or when state is provided, merge it into _G
  -- This allows the process to restore previous state
  if state and next(state) then
    -- Create a lookup table for system keys for O(1) access
    local system_keys_set = {}
    for _, key in ipairs(SYSTEM_KEYS) do
      system_keys_set[key] = true
    end
    
    for key, value in pairs(state) do
      -- Don't overwrite system keys or functions
      if type(_G[key]) ~= "function" and not system_keys_set[key] then
        _G[key] = value
      end
    end
  end
  
  -- Initialize results structure in _G
  _G.results = _G.results or {}
  _G.results.outbox = {}
  _G.results.output = { data = "", prompt = prompt() }
  _G.results.info = "hyper-aos"
  
  -- Extract message from assignment
  local msg = assignment.body or {}
  
  -- Ensure message has 'from' field
  msg = meta.ensure_message(msg)
  
  -- Initialize process state from first Process message
  if not meta.initialized then 
    meta.init(msg) 
  end

  -- Extract and normalize action
  local action = msg.action or ""
  action = string.lower(action)

  local status, result = false, ""

  -- Handle actions by calling global functions
  if action ~= "compute" and type(_G[action]) == "function" then
    status, result = pcall(_G[action], msg)
  else
    -- If not handled, add to inbox
    result = "New Message"
    table.insert(_G.Inbox, msg)
    -- Implement FIFO rotation when inbox exceeds limit
    if #_G.Inbox > _G.MAX_INBOX_SIZE then
      table.remove(_G.Inbox, 1)
    end
  end

  -- Set execution status
  _G.results.status = "ok"
  if not status and result ~= "" then
    _G.results.status = "error"
  end

  -- Format output based on result type
  if type(result) == "table" then
    _G.results.output.data = result
  else
    print(tostring(result))
    _G.results.output.data = removeCR(_G._OUTPUT)
  end

  -- Set print flag for non-eval actions
  if action ~= "eval" then
    _G.results.output.print = true
  end
  
  -- Extract state from _G, filtering out system keys and functions
  -- This creates a clean state object containing only user data
  local filtered_state = extract_state_from_global()
  
  -- Return status and filtered state
  -- The state will be persisted and passed back in the next compute call
  return "ok", filtered_state
end



