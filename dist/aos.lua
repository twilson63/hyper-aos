-- HyperAOS Concatenated Module Bundle
-- Generated: 2025-08-06 15:45:15
-- Platform: LUERL/HyperBEAM
-- Modules: utils, aos

-- Initialize package system for LUERL compatibility
_G.package = _G.package or {}
_G.package.loaded = _G.package.loaded or {}

-- ============================================
-- Module: utils
-- Source: src/utils.lua
-- ============================================

do
  local function load_utils()
    --- The Utils module provides a collection of utility functions for functional programming in Lua.
    -- Optimized for LUERL VM compatibility in Hyper-AOS environment.
    -- Includes functions for array manipulation, pattern matching, functional composition,
    -- and object property operations.
    -- @module utils
    -- @author Hyper-AOS Team
    -- @version 1.0.0
    
    -- Store utils in global namespace following Hyper-AOS patterns
    _G.utils = _G.utils or {}
    
    -- Set version for tracking
    _G.utils._version = "1.0.0"
    
    -- Provide backwards compatibility alias: _G.Utils -> _G.utils
    -- This ensures code using Utils.map() continues to work with both cases
    _G.Utils = _G.utils
    
    --- LUERL-optimized helper function to check if a table is an array.
    -- An 'array' is defined as a table with integer keys starting from 1 and
    -- having no gaps between the keys. Optimized for LUERL VM performance.
    -- @param t table The table to check
    -- @return boolean Whether the table is an array
    local function isArray(t)
      -- Early type check for performance
      if type(t) ~= "table" then
        return false
      end
      
      -- Handle empty table case
      local len = #t
      if len == 0 then
        -- Check if truly empty or has non-numeric keys
        for _ in pairs(t) do
          return false -- Has keys but length is 0, not an array
        end
        return true -- Truly empty table is considered an array
      end
      
      -- LUERL optimization: use # operator instead of pairs iteration
      -- Check that all keys from 1 to len exist and no other keys exist
      local count = 0
      for k, _ in pairs(t) do
        if type(k) ~= "number" or k < 1 or math.floor(k) ~= k or k > len then
          return false
        end
        count = count + 1
      end
      
      -- Array if count equals length (no gaps, no extra keys)
      return count == len
    end
    
    --- Given a pattern, a value, and a message, returns whether there is a pattern match.
    -- Supports wildcards, functions, strings (exact and regex), and nested tables.
    -- LUERL-optimized with explicit nil handling and reduced function calls.
    -- @param pattern any The pattern to match against
    -- @param value any The value to check for in the pattern  
    -- @param msg table The message context for function patterns
    -- @return boolean Whether there is a pattern match
    function _G.utils.matchesPattern(pattern, value, msg)
      -- Explicit nil check for LUERL compatibility
      if pattern == nil then
        return false
      end
      
      -- Wildcard pattern always matches
      if pattern == '_' then
        return true
      end
      
      -- Function pattern execution
      if type(pattern) == "function" then
        local success, result = pcall(pattern, value, msg)
        return success and result == true
      end
      
      -- String pattern matching
      if type(pattern) == "string" then
        if value == nil then
          return false
        end
        
        local str_value = tostring(value)
        
        -- Check for regex special characters (excluding single '-')
        if string.match(pattern, "[%^%$%(%)%%%.%[%]%*%+%?]") then
          -- Regex pattern matching with error handling for LUERL
          local success, result = pcall(string.match, str_value, pattern)
          return success and result ~= nil
        else
          -- Exact string match
          return str_value == pattern
        end
      end
      
      -- Table pattern - check if any sub-pattern matches
      if type(pattern) == "table" then
        for _, subPattern in pairs(pattern) do
          if _G.utils.matchesPattern(subPattern, value, msg) then
            return true
          end
        end
      end
      
      return false
    end
    
    --- Given a message and a spec, returns whether there is a spec match.
    -- Supports function specs, table specs with pattern matching, and string action matching.
    -- LUERL-optimized with proper error handling.
    -- @param msg table The message to check against the spec
    -- @param spec any The specification to match (function, table, or string)
    -- @return boolean Whether there is a spec match
    function _G.utils.matchesSpec(msg, spec)
      -- Explicit nil checks for LUERL
      if msg == nil or spec == nil then
        return false
      end
      
      -- Function spec execution
      if type(spec) == "function" then
        local success, result = pcall(spec, msg)
        return success and result == true
      end
      
      -- Table spec - all patterns must match corresponding message fields
      if type(spec) == "table" then
        for key, pattern in pairs(spec) do
          -- Message must have the key
          if msg[key] == nil then
            return false
          end
          -- Pattern must match the message field value
          if not _G.utils.matchesPattern(pattern, msg[key], msg) then
            return false
          end
        end
        return true
      end
      
      -- String spec - match against Action field
      if type(spec) == "string" and msg.Action then
        return tostring(msg.Action) == spec
      end
      
      return false
    end
    
    --- Curries a function with specified arity.
    -- LUERL-optimized curry implementation using table.unpack.
    -- @param fn function The function to curry
    -- @param arity number Optional arity (defaults to function parameter count if available)
    -- @return function The curried function
    function _G.utils.curry(fn, arity)
      -- Type validation
      if type(fn) ~= "function" then
        error("first argument must be a function", 2)
      end
      
      -- Default arity handling for LUERL (debug may not be available)
      arity = arity or 2 -- Default to 2 if debug info not available
      
      -- LUERL performance: early return for simple functions
      if arity < 2 then
        return fn
      end
      
      return function(...)
        local args = {...}
        local arg_count = #args
        
        if arg_count >= arity then
          -- Enough arguments, call the function
          return fn(table.unpack(args))
        else
          -- Not enough arguments, return another curried function
          return _G.utils.curry(function(...)
            local new_args = {...}
            -- Create fresh combined args array to avoid reference issues
            local combined_args = {}
            for i = 1, #args do
              combined_args[i] = args[i]
            end
            for i = 1, #new_args do
              combined_args[#args + i] = new_args[i]
            end
            return fn(table.unpack(combined_args))
          end, arity - arg_count)
        end
      end
    end
    
    --- Concatenates two arrays into a new array.
    -- LUERL-optimized using # operator for length and direct indexing.
    -- @param a table The first array
    -- @param b table The second array  
    -- @return table The concatenated array
    _G.utils.concat = _G.utils.curry(function(a, b)
      -- Type and array validation
      if type(a) ~= "table" then
        error("first argument must be an array table", 2)
      end
      if type(b) ~= "table" then
        error("second argument must be an array table", 2)
      end
      if not isArray(a) then
        error("first argument must be an array", 2)
      end
      if not isArray(b) then
        error("second argument must be an array", 2)
      end
      
      -- LUERL optimization: pre-allocate result table and use # operator
      local result = {}
      local a_len = #a
      local b_len = #b
      
      -- Copy first array
      for i = 1, a_len do
        result[i] = a[i]
      end
      
      -- Copy second array
      for i = 1, b_len do
        result[a_len + i] = b[i]
      end
      
      return result
    end, 2)
    
    --- Reduces an array to a single value using a reducer function.
    -- LUERL-optimized with ipairs for better performance and explicit nil handling.
    -- @param fn function The reducer function (accumulator, value, index) -> accumulator
    -- @param initial any The initial accumulator value
    -- @param t table The array to reduce
    -- @return any The final reduced value
    _G.utils.reduce = _G.utils.curry(function(fn, initial, t)
      -- Type validation
      if type(fn) ~= "function" then
        error("first argument must be a function", 2)
      end
      if type(t) ~= "table" or not isArray(t) then
        error("third argument must be an array table", 2)
      end
      
      local result = initial
      
      -- LUERL optimization: use ipairs for array iteration
      for k, v in ipairs(t) do
        if result == nil then
          result = v
        else
          result = fn(result, v, k)
        end
      end
      
      return result
    end, 3)
    
    --- Maps a function over an array, creating a new array.
    -- LUERL-optimized implementation using reduce.
    -- @param fn function The mapping function (value, index) -> new_value
    -- @param data table The array to map over
    -- @return table The new mapped array
    _G.utils.map = _G.utils.curry(function(fn, data)
      -- Type validation
      if type(fn) ~= "function" then
        error("first argument must be a function", 2)
      end
      if type(data) ~= "table" or not isArray(data) then
        error("second argument must be an array table", 2)
      end
      
      -- Use reduce for mapping implementation
      local function mapper(result, v, k)
        result[k] = fn(v, k)
        return result
      end
      
      return _G.utils.reduce(mapper, {}, data)
    end, 2)
    
    --- Filters an array based on a predicate function.
    -- LUERL-optimized using reduce and table.insert.
    -- @param fn function The predicate function (value) -> boolean
    -- @param data table The array to filter
    -- @return table The filtered array
    _G.utils.filter = _G.utils.curry(function(fn, data)
      -- Type validation
      if type(fn) ~= "function" then
        error("first argument must be a function", 2)
      end
      if type(data) ~= "table" or not isArray(data) then
        error("second argument must be an array table", 2)
      end
      
      -- Use reduce for filtering implementation
      local function filterer(result, v, _k)
        if fn(v) then
          table.insert(result, v)
        end
        return result
      end
      
      return _G.utils.reduce(filterer, {}, data)
    end, 2)
    
    --- Finds the first element in an array that satisfies a predicate.
    -- LUERL-optimized with early return and ipairs iteration.
    -- @param fn function The predicate function (value) -> boolean
    -- @param t table The array to search
    -- @return any|nil The first matching element or nil
    _G.utils.find = _G.utils.curry(function(fn, t)
      -- Type validation
      if type(fn) ~= "function" then
        error("first argument must be a function", 2)
      end
      if type(t) ~= "table" then
        error("second argument must be a table", 2)
      end
      
      -- LUERL optimization: use ipairs for arrays, pairs for objects
      if isArray(t) then
        for _, v in ipairs(t) do
          if fn(v) then
            return v
          end
        end
      else
        for _, v in pairs(t) do
          if fn(v) then
            return v
          end
        end
      end
      
      return nil
    end, 2)
    
    --- Checks if a property of an object equals a specific value.
    -- LUERL-optimized with explicit type conversions.
    -- @param propName string The property name to check
    -- @param value any The value to compare against
    -- @param object table The object to check
    -- @return boolean Whether the property equals the value
    _G.utils.propEq = _G.utils.curry(function(propName, value, object)
      -- Type validation
      if type(propName) ~= "string" then
        error("first argument must be a string", 2)
      end
      if type(object) ~= "table" then
        error("third argument must be a table", 2)
      end
      
      -- Direct comparison with nil handling
      return object[propName] == value
    end, 3)
    
    --- Reverses an array, creating a new array.
    -- LUERL-optimized using # operator and direct indexing.
    -- @param data table The array to reverse
    -- @return table The reversed array
    function _G.utils.reverse(data)
      -- Type validation
      if type(data) ~= "table" then
        error("argument must be an array table", 2)
      end
      
      -- LUERL optimization: direct indexing instead of reduce
      local result = {}
      local len = #data
      
      for i = 1, len do
        result[len - i + 1] = data[i]
      end
      
      return result
    end
    
    --- Composes multiple functions into a single function (right to left).
    -- LUERL-optimized with proper argument handling.
    -- @param ... function Functions to compose
    -- @return function The composed function
    _G.utils.compose = _G.utils.curry(function(...)
      local fns = {...}
      
      return function(v)
        local result = v
        -- Apply functions in reverse order (right to left composition)
        for i = #fns, 1, -1 do
          local fn = fns[i]
          if type(fn) ~= "function" then
            error("all arguments must be functions", 2)
          end
          result = fn(result)
        end
        return result
      end
    end, 2)
    
    --- Gets a property value from an object.
    -- LUERL-optimized direct property access.
    -- @param propName string The property name to get
    -- @param object table The object to get the property from
    -- @return any The property value
    _G.utils.prop = _G.utils.curry(function(propName, object)
      -- Type validation
      if type(object) ~= "table" then
        error("second argument must be a table", 2)
      end
      
      return object[propName]
    end, 2)
    
    --- Checks if an array includes a specific value.
    -- LUERL-optimized using find function.
    -- @param val any The value to search for
    -- @param t table The array to search in
    -- @return boolean Whether the value is found
    _G.utils.includes = _G.utils.curry(function(val, t)
      -- Type validation
      if type(t) ~= "table" or not isArray(t) then
        error("second argument must be an array table", 2)
      end
      
      -- Use find with equality check
      return _G.utils.find(function(v) return v == val end, t) ~= nil
    end, 2)
    
    --- Gets all keys from a table as an array.
    -- LUERL-optimized with table.insert for performance.
    -- @param t table The table to get keys from
    -- @return table Array of keys
    function _G.utils.keys(t)
      -- Type validation
      if type(t) ~= "table" then
        error("argument must be a table", 2)
      end
      
      local keys = {}
      for key, _ in pairs(t) do
        table.insert(keys, key)
      end
      
      return keys
    end
    
    --- Gets all values from a table as an array.
    -- LUERL-optimized with table.insert for performance.
    -- @param t table The table to get values from
    -- @return table Array of values
    function _G.utils.values(t)
      -- Type validation
      if type(t) ~= "table" then
        error("argument must be a table", 2)
      end
      
      local values = {}
      for _, value in pairs(t) do
        table.insert(values, value)
      end
      
      return values
    end
    
    --- Export utils for require() compatibility while maintaining global access
    -- This allows both _G.utils.function() and local utils = require('utils') patterns
    return _G.utils
  end

  -- Register module in package.loaded
  local status, result = pcall(load_utils)
  if status then
    _G.package.loaded["utils"] = result
    print("✅ Loaded module: utils")
  else
    print("❌ Failed to load module utils: " .. tostring(result))
  end
end

-- ============================================
-- Module: aos
-- Source: src/aos.lua
-- ============================================

do
  local function load_aos()
    -- set version for hyper-aos
    _G.package.loaded['.process'] = { _version = "dev" }
    
    -- Load utils module and integrate into global namespace
    -- This makes utils functions available in _G.utils for message processing
    -- Only initialize if utils hasn't been loaded yet to avoid conflicts
    if not _G.utils or type(_G.utils) ~= "table" then
      _G.utils = {}
    end
    
    -- Provide backwards compatibility alias: _G.Utils -> _G.utils
    -- This ensures code using Utils.map() continues to work
    if _G.utils then
      _G.Utils = _G.utils
    end
    
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
    
    -- Utils helper functions removed to avoid LUERL loading conflicts
    -- Utils should be accessed directly via _G.utils or _G.Utils
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
      -- Utils module (system component, persisted separately)
      "utils",
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
               c.yellow .. _G.process._version .. c.reset .. 
               c.white .. "[" .. c.reset .. 
               c.bright_magenta .. #Inbox .. c.reset .. 
               c.white .. "]" .. c.reset .. 
               c.bright_blue .. "> " .. c.reset
      else
        return "hyper~aos@" .. _G.process._version .. "[" .. #Inbox .. "]> "
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
      
      -- Ensure utils backwards compatibility alias is set
      if _G.utils and not _G.Utils then
        _G.Utils = _G.utils
      end
    
      -- Extract and normalize action
      local action = msg.action or ""
      action = string.lower(action)
      
      -- Demonstrate utils integration for message processing
      if action == "demo-utils" then
        -- Show utils functionality with current message
        local demo_result = {
          utils_version = _G.utils._version or "not_loaded",
          message_keys = _G.utils.keys and _G.utils.keys(msg) or {},
          trusted = meta.is_trusted(msg),
          matches_eval_spec = _G.utils.matchesSpec and _G.utils.matchesSpec(msg, {action = "demo-utils"}) or false,
          inbox_count = #_G.Inbox,
          -- filtered_inbox removed due to luerl loading conflict
        }
        print("Utils Integration Demo:")
        print(demo_result)
        return "ok", extract_state_from_global()
      end
    
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
    
    
    
    

    -- AOS module doesn't return a value, it sets up _G
  end

  -- Register module in package.loaded
  local status, result = pcall(load_aos)
  if status then
    _G.package.loaded["aos"] = result
    print("✅ Loaded module: aos")
  else
    print("❌ Failed to load module aos: " .. tostring(result))
  end
end

-- ============================================
-- Bundle initialization complete
-- ============================================

-- Verify module loading
local loaded_count = 0
for name, _ in pairs(_G.package.loaded) do
  loaded_count = loaded_count + 1
end

if loaded_count > 0 then
  print("📦 Bundle loaded successfully with " .. loaded_count .. " modules")
else
  print("⚠️  Warning: No modules were loaded")
end

-- Return main AOS module for direct usage
return _G.package.loaded["aos"]