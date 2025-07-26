-- set version for hyper-aos
_G.package.loaded['.process'] = { _version = "dev" }

-- initialize inbox with max size
Inbox = Inbox or {}
MAX_INBOX_SIZE = 10000
-- state variable for prints
_OUTPUT = ""

-- Private functions table
-- This table is local to this module and cannot be accessed from eval() or external code
---@diagnostic disable-next-line
meta = meta or { initialized = false, owner = "", id = "", authorities = {} }
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

-- override print function
---@diagnostic disable-next-line
function print(txt)
  _OUTPUT = _OUTPUT .. txt .. "\n"
end

-- utility function to remove last CR
---@diagnostic disable-next-line
function removeCR(str)
    if str:sub(-1) == "\r" or str:sub(-1) == "\n" then
        return str:sub(1, -2)
    end
    return str
end

-- prompt function for console
---@diagnostic disable-next-line
function prompt()
  return "hyper~aos@" .. require('.process')._version .. "[" .. #Inbox .. "]> "
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



