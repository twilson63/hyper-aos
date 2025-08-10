-- set version for hyper-aos
_G.package.loaded['.process'] = { _version = "dev" }
-- load handlers to global state if exists
if _G.package.loaded['.handlers'] then
  _G.Handlers = require('.handlers')
end

-- initialize inbox with max size
Inbox = Inbox or {}
MAX_INBOX_SIZE = 10000
-- state variable for prints
_OUTPUT = ""

-- Private functions table
-- This table is local to this module and cannot be accessed from eval() or external code
---@diagnostic disable-next-line
meta = meta or { initialized = false, owner = "", id = "", authorities = {}, colors = {} }
function meta.init(msg)
  -- Initialize owner from first Process message
  if not meta.initialized and msg.type and string.lower(msg.type) == "process" and msg.commitments then
    -- Find first non-hmac commitment and set its committer as owner
    for key, commitment in pairs(msg.commitments) do
      if commitment.type and string.lower(commitment.type) ~= "hmac-sha256" and commitment.committer then
        meta.id = key
        meta.owner = commitment.committer
        meta.initialized = true
        
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
            
            -- Check if it's 43 characters
            if #authority == 43 then
              table.insert(meta.authorities, authority)
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
  
  -- Initialize colors table with terminal escape codes
  if not next(meta.colors) then
    meta.colors = {
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

-- Private function to check if a message is trusted
-- A message is trusted if it has from-process equal to from and the committer is in authorities
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
  if msg.commitments then
    for _, commitment in pairs(msg.commitments) do
      if commitment.committer then
        -- Check if this committer is in the authorities list
        for _, authority in ipairs(meta.authorities) do
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

-- Private function to check if message has valid owner commitment
-- Validates that the message's from matches the meta.owner
function meta.is_owner(msg)
  -- Ensure message has 'from' field
  meta.ensure_message(msg)
  
  -- Check if msg.from matches the owner
  if msg.from and msg.from == meta.owner then
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
      return meta.colors.green .. '"' .. tbl .. '"' .. meta.colors.reset
    else
      return meta.colors.blue .. tostring(tbl) .. meta.colors.reset
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
        v = meta.colors.green .. '"' .. v .. '"' .. meta.colors.reset
      else
        v = meta.colors.blue .. tostring(v) .. meta.colors.reset
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
          v = meta.colors.green .. '"' .. v .. '"' .. meta.colors.reset
        else
          v = meta.colors.blue .. tostring(v) .. meta.colors.reset
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
            v = meta.colors.dim .. "<circular reference>" .. meta.colors.reset
        else
          visited[v] = true
          v = stringify(v, indent + 2, visited)
        end
      elseif type(v) == "string" then
        v = meta.colors.green .. '"' .. v .. '"' .. meta.colors.reset
      else
        v = meta.colors.blue .. tostring(v) .. meta.colors.reset
      end
      -- Format key with color
      local keyStr = tostring(k)
      if type(k) == "string" then
        keyStr = meta.colors.red .. keyStr .. meta.colors.reset
      else
        keyStr = meta.colors.yellow .. "[" .. keyStr .. "]" .. meta.colors.reset
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
  if meta.colors and meta.colors.cyan then
    local c = meta.colors
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
  table.insert(State.results.outbox, msg)
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

-- compute is the entry point of your process
---@diagnostic disable-next-line
function compute(state, assignment)
  -- clear output
  _OUTPUT = ""
  -- set State
  State = state
  State.results = state.results or {}
  State.results.outbox = { }
  State.results.output = { data = "", prompt = prompt() }
  state.results.info = "hyper-aos"
  local msg = assignment.body or {}
  -- Ensure message has 'from' field
  msg = meta.ensure_message(msg)
  
  if not meta.initialized then meta.init(msg) end

  local action = msg.action or ""
  action = string.lower(action)

  local status, result = false, ""

  -- handle actions being submitted 
  if action ~= "compute" and type(_G[action]) == "function" then
    status, result = pcall(_G[action], msg)
  else
    -- in not handled add to inbox
    result = "New Message"
    table.insert(Inbox, msg)
    -- implement FIFO rotation when inbox exceeds limit
    if #Inbox > MAX_INBOX_SIZE then
      table.remove(Inbox, 1)
    end
  end

  State.results.status = "ok"
  if not status and result ~= "" then
    --print("ERROR:")
    State.results.status = "error"
  end

  if type(result) == "table" then
    State.results.output.data = result -- TODO: need to format
  else
    print(tostring(result))
    State.results.output.data = removeCR(_OUTPUT)
  end

  if action ~= "eval" then
    State.results.output.print = true
  end
  -- for aos console to provide feedback the process needs to return:
  -- results:
  --   output
  --     data
  --     print
  --     prompt
  --   outbox
  --     - 1 message
  --     - 2 message
  --     - ...
  return "ok", State
end



