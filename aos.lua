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
meta = meta or { initialized = false, owner = "", id = "" }
function meta.init(msg)
  -- Initialize owner from first Process message
  if not meta.initialized and msg.type and string.lower(msg.type) == "process" and msg.commitments then
    -- Find first non-hmac commitment and set its committer as owner
    for key, commitment in pairs(msg.commitments) do
      if commitment.type and string.lower(commitment.type) ~= "hmac-sha256" and commitment.committer then
        meta.id = key
        meta.owner = commitment.committer
        meta.initialized = true
        break
      end
    end
  end

end
-- Private function to check if message has valid owner commitment
-- Validates that the message's committer matches the State.owner
function meta.is_owner(msg)
  -- Message must have commitments
  if not msg.commitments then
    return false
  end
  -- Check each commitment in the message
  for key, commitment in pairs(msg.commitments) do
    if commitment.type and commitment.committer then
      if commitment.committer == meta.owner then
        return true
      end
    end
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



