local json = { _version = "0.2.0" }

-- Maximum nesting depth to prevent stack overflow
local MAX_DEPTH = 1000

-------------------------------------------------------------------------------
-- Encode
-------------------------------------------------------------------------------

local encode

local escape_char_map = {
  [ "\\" ] = "\\",
  [ "\"" ] = "\"",
  [ "\b" ] = "b",
  [ "\f" ] = "f",
  [ "\n" ] = "n",
  [ "\r" ] = "r",
  [ "\t" ] = "t"
}

local escape_char_map_inv = { [ "/" ] = "/" }
for k, v in pairs(escape_char_map) do escape_char_map_inv[v] = k end

local function escape_char(c)
  return "\\" .. (escape_char_map[c] or string.format("u%04x", c:byte()))
end

local function encode_nil(val) return "null" end

local function encode_table(val, stack)
  local res = {}
  stack = stack or {}
  
  -- Circular reference?
  if stack[val] then error("circular reference") end
  
  -- Check depth
  local depth = 0
  for _ in pairs(stack) do depth = depth + 1 end
  if depth >= MAX_DEPTH then
    error("maximum nesting depth exceeded")
  end
  
  stack[val] = true
  if rawget(val, 1) ~= nil or next(val) == nil then
    -- Treat as array -- check keys are valid and it is not sparse
    local n = 0
    for k in pairs(val) do
      if type(k) ~= "number" then
        error("invalid table: mixed or invalid key types")
      end
      n = n + 1
    end
    if n ~= #val then error("invalid table: sparse array") end
    -- Encode
    for i = 1, #val do res[i] = encode(val[i], stack) end
    stack[val] = nil
    return "[" .. table.concat(res, ",") .. "]"
  else
    -- Treat as an object
    local i = 1
    for k, v in pairs(val) do
      if type(k) ~= "string" then
        error("invalid table: mixed or invalid key types")
      end
      if type(v) ~= "function" then
        res[i] = encode(k, stack) .. ":" .. encode(v, stack)
        i = i + 1
      end
    end
    stack[val] = nil
    return "{" .. table.concat(res, ",") .. "}"
  end
end

local function encode_string(val)
  return '"' .. val:gsub('[%z\1-\31\\"]', escape_char) .. '"'
end

local function encode_number(val)
  -- Check for NaN, -inf and inf
  if val ~= val or val <= -math.huge or val >= math.huge then
    error("unexpected number value '" .. tostring(val) .. "'")
  end
  return string.format("%.14g", val)
end

local type_func_map = {
  [ "nil" ]     = encode_nil,
  [ "table" ]   = encode_table,
  [ "string" ]  = encode_string,
  [ "number" ]  = encode_number,
  [ "boolean" ] = tostring
}

encode = function(val, stack)
  local t = type(val)
  local f = type_func_map[t]
  if f then return f(val, stack) end
  error("unexpected type '" .. t .. "'")
end

function json.encode(val) return (encode(val)) end

-------------------------------------------------------------------------------
-- Decode
-------------------------------------------------------------------------------

local parse

local function create_set(...)
  local res = {}
  for i = 1, select("#", ...) do res[select(i, ...)] = true end
  return res
end

local space_chars = create_set(" ", "\t", "\r", "\n")
local delim_chars = create_set(" ", "\t", "\r", "\n", "]", "}", ",")
local escape_chars = create_set("\\", "/", '"', "b", "f", "n", "r", "t", "u")
local literals = create_set("true", "false", "null")

local literal_map = { [ "true" ] = true, [ "false" ] = false, [ "null" ] = nil }

local function next_char(str, idx, set, negate)
  for i = idx, #str do if set[str:sub(i, i)] ~= negate then return i end end
  return #str + 1
end

local function decode_error(str, idx, msg)
  local line_count = 1
  local col_count = 1
  for i = 1, idx - 1 do
    col_count = col_count + 1
    if str:sub(i, i) == "\n" then
      line_count = line_count + 1
      col_count = 1
    end
  end
  error(string.format("%s at line %d col %d", msg, line_count, col_count))
end

local function codepoint_to_utf8(n)
  local f = math.floor
  if n <= 0x7f then
    return string.char(n)
  elseif n <= 0x7ff then
    return string.char(f(n / 64) + 192, n % 64 + 128)
  elseif n <= 0xffff then
    return string.char(f(n / 4096) + 224, f(n % 4096 / 64) + 128,
                       n % 64 + 128)
  elseif n <= 0x10ffff then
    return string.char(f(n / 262144) + 240, f(n % 262144 / 4096) + 128,
                       f(n % 4096 / 64) + 128, n % 64 + 128)
  end
  error(string.format("invalid unicode codepoint '%x'", n))
end

local function parse_unicode_escape(s)
  local n1 = tonumber(s:sub(1, 4), 16)
  local n2 = tonumber(s:sub(7, 10), 16)
  
  -- Check if n1 is a high surrogate (0xD800-0xDBFF)
  if n1 >= 0xd800 and n1 <= 0xdbff then
    -- High surrogate must be followed by low surrogate
    if not n2 then
      error("invalid unicode escape: lone high surrogate")
    end
    if n2 < 0xdc00 or n2 > 0xdfff then
      error("invalid unicode escape: high surrogate not followed by low surrogate")
    end
    -- Valid surrogate pair
    return codepoint_to_utf8((n1 - 0xd800) * 0x400 + (n2 - 0xdc00) + 0x10000)
  -- Check if n1 is a low surrogate (0xDC00-0xDFFF)
  elseif n1 >= 0xdc00 and n1 <= 0xdfff then
    error("invalid unicode escape: lone low surrogate")
  elseif n2 then
    -- n2 exists but n1 is not a high surrogate - this shouldn't happen in valid JSON
    error("invalid unicode escape: unexpected surrogate sequence")
  else
    return codepoint_to_utf8(n1)
  end
end

local function parse_string(str, i, depth)
  local res = {}
  local j = i + 1
  local k = j
  
  while j <= #str do
    local x = str:byte(j)
    
    if x < 32 then
      decode_error(str, j, "control character in string")
    elseif x == 92 then -- `\`: Escape
      res[#res + 1] = str:sub(k, j - 1)
      j = j + 1
      local c = str:sub(j, j)
      if c == "u" then
        local hex = str:match("^[dD][89aAbB]%x%x\\u%x%x%x%x", j + 1) or
                    str:match("^%x%x%x%x", j + 1) or
                    decode_error(str, j - 1,
                                 "invalid unicode escape in string")
        res[#res + 1] = parse_unicode_escape(hex)
        j = j + #hex
      else
        if not escape_chars[c] then
          decode_error(str, j - 1,
                       "invalid escape char '" .. c .. "' in string")
        end
        res[#res + 1] = escape_char_map_inv[c]
      end
      k = j + 1
    elseif x == 34 then -- `"`: End of string
      res[#res + 1] = str:sub(k, j - 1)
      return table.concat(res), j + 1
    end
    
    j = j + 1
  end
  
  decode_error(str, i, "expected closing quote for string")
end

local function parse_number(str, i, depth)
  local x = next_char(str, i, delim_chars)
  local s = str:sub(i, x - 1)
  local n = tonumber(s)
  if not n then decode_error(str, i, "invalid number '" .. s .. "'") end
  return n, x
end

local function parse_literal(str, i, depth)
  local x = next_char(str, i, delim_chars)
  local word = str:sub(i, x - 1)
  if not literals[word] then
    decode_error(str, i, "invalid literal '" .. word .. "'")
  end
  return literal_map[word], x
end

local function parse_array(str, i, depth)
  local res = {}
  local n = 1
  depth = depth or 0
  
  -- Check depth
  if depth >= MAX_DEPTH then
    decode_error(str, i, "maximum nesting depth exceeded")
  end
  
  i = i + 1
  while true do
    local x
    i = next_char(str, i, space_chars, true)
    if str:sub(i, i) == "]" then
      i = i + 1
      break
    end
    x, i = parse(str, i, depth + 1)
    res[n] = x
    n = n + 1
    i = next_char(str, i, space_chars, true)
    local chr = str:sub(i, i)
    i = i + 1
    if chr == "]" then break end
    if chr ~= "," then decode_error(str, i, "expected ']' or ','") end
  end
  return res, i
end

local function parse_object(str, i, depth)
  local res = {}
  depth = depth or 0
  
  -- Check depth
  if depth >= MAX_DEPTH then
    decode_error(str, i, "maximum nesting depth exceeded")
  end
  
  i = i + 1
  while true do
    local key, val
    i = next_char(str, i, space_chars, true)
    if str:sub(i, i) == "}" then
      i = i + 1
      break
    end
    if str:sub(i, i) ~= '"' then
      decode_error(str, i, "expected string for key")
    end
    key, i = parse(str, i, depth + 1)
    i = next_char(str, i, space_chars, true)
    if str:sub(i, i) ~= ":" then
      decode_error(str, i, "expected ':' after key")
    end
    i = next_char(str, i + 1, space_chars, true)
    val, i = parse(str, i, depth + 1)
    res[key] = val
    i = next_char(str, i, space_chars, true)
    local chr = str:sub(i, i)
    i = i + 1
    if chr == "}" then break end
    if chr ~= "," then decode_error(str, i, "expected '}' or ','") end
  end
  return res, i
end

local char_func_map = {
  [ '"' ] = parse_string,
  [ "0" ] = parse_number,
  [ "1" ] = parse_number,
  [ "2" ] = parse_number,
  [ "3" ] = parse_number,
  [ "4" ] = parse_number,
  [ "5" ] = parse_number,
  [ "6" ] = parse_number,
  [ "7" ] = parse_number,
  [ "8" ] = parse_number,
  [ "9" ] = parse_number,
  [ "-" ] = parse_number,
  [ "t" ] = parse_literal,
  [ "f" ] = parse_literal,
  [ "n" ] = parse_literal,
  [ "[" ] = parse_array,
  [ "{" ] = parse_object
}

parse = function(str, idx, depth)
  local chr = str:sub(idx, idx)
  local f = char_func_map[chr]
  if f then return f(str, idx, depth) end
  decode_error(str, idx, "unexpected character '" .. chr .. "'")
end

function json.decode(str)
  if type(str) ~= "string" then
    error("expected argument of type string, got " .. type(str))
  end
  local res, idx = parse(str, next_char(str, 1, space_chars, true), 0)
  idx = next_char(str, idx, space_chars, true)
  if idx <= #str then decode_error(str, idx, "trailing garbage") end
  return res
end

return json