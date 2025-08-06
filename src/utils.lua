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