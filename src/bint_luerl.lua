--[[
bint_luerl - Optimized version for LUERL environment
Leverages LUERL's native large integer support for efficient arbitrary-precision arithmetic

This is a simplified API-compatible version of bint that directly uses LUERL's
native large integer capabilities instead of the array-based implementation.
]]

local memo = {}

--- Create a new bint module
-- For LUERL, we can use native integers of any size
local function newmodule(bits, wordbits)
  bits = bits or 256
  
  -- Memoize modules
  local memoindex = bits * 64 + (wordbits or 0)
  if memo[memoindex] then
    return memo[memoindex]
  end
  
  -- Create bint module
  local bint = {}
  bint.__index = bint
  
  -- Store bits for compatibility
  bint.bits = bits
  
  -- In LUERL, we store the value directly as a large integer
  -- wrapped in a table for metatable support
  
  --- Create a new bint with 0 value
  function bint.zero()
    return setmetatable({value = 0}, bint)
  end
  
  --- Create a new bint with 1 value
  function bint.one()
    return setmetatable({value = 1}, bint)
  end
  
  --- Create a bint from an unsigned integer
  function bint.fromuinteger(x)
    x = tonumber(x)
    if x then
      return setmetatable({value = math.floor(x)}, bint)
    end
  end
  
  --- Create a bint from a signed integer
  function bint.frominteger(x)
    x = tonumber(x)
    if x then
      return setmetatable({value = math.floor(x)}, bint)
    end
  end
  
  --- Create a bint from a string in base
  function bint.frombase(s, base)
    if type(s) ~= 'string' then
      return nil
    end
    base = base or 10
    if not (base >= 2 and base <= 36) then
      return nil
    end
    
    -- Use native tonumber for conversion
    local value = tonumber(s, base)
    if value then
      return setmetatable({value = math.floor(value)}, bint)
    end
    
    -- For very large numbers, parse manually
    local sign, int = s:lower():match('^([+-]?)(%w+)$')
    if not int then
      return nil
    end
    
    local result = 0
    local power = 1
    for i = #int, 1, -1 do
      local digit = tonumber(int:sub(i, i), base)
      if not digit then
        return nil
      end
      result = result + digit * power
      power = power * base
    end
    
    if sign == '-' then
      result = -result
    end
    
    return setmetatable({value = result}, bint)
  end
  
  --- Create a bint from a string
  function bint.fromstring(s)
    if type(s) ~= 'string' then
      return nil
    end
    
    if s:find('^[+-]?[0-9]+$') then
      return bint.frombase(s, 10)
    elseif s:find('^[+-]?0[xX][0-9a-fA-F]+$') then
      return bint.frombase(s:gsub('0[xX]', '', 1), 16)
    elseif s:find('^[+-]?0[bB][01]+$') then
      return bint.frombase(s:gsub('0[bB]', '', 1), 2)
    end
  end
  
  --- Create a new bint from a value
  function bint.new(x)
    if getmetatable(x) == bint then
      return setmetatable({value = x.value}, bint)
    end
    
    local ty = type(x)
    if ty == 'number' then
      return bint.frominteger(x)
    elseif ty == 'string' then
      return bint.fromstring(x)
    end
    
    assert(false, 'value cannot be represented by a bint')
  end
  
  --- Convert to bint if possible
  function bint.tobint(x, clone)
    if getmetatable(x) == bint then
      if clone then
        return setmetatable({value = x.value}, bint)
      end
      return x
    end
    
    local ty = type(x)
    if ty == 'number' then
      return bint.frominteger(x)
    elseif ty == 'string' then
      return bint.fromstring(x)
    end
  end
  
  --- Parse to bint or number
  function bint.parse(x, clone)
    local b = bint.tobint(x, clone)
    if b then
      return b
    end
    return tonumber(x)
  end
  
  --- Convert to unsigned integer
  function bint.touinteger(x)
    if getmetatable(x) == bint then
      return x.value
    end
    return math.floor(tonumber(x) or 0)
  end
  
  --- Convert to signed integer
  function bint.tointeger(x)
    if getmetatable(x) == bint then
      return x.value
    end
    return math.floor(tonumber(x) or 0)
  end
  
  --- Convert to number
  function bint.tonumber(x)
    if getmetatable(x) == bint then
      return x.value
    end
    return tonumber(x)
  end
  
  --- Convert to string in base
  function bint.tobase(x, base, unsigned)
    x = bint.tobint(x)
    if not x then
      return nil
    end
    
    base = base or 10
    if not (base >= 2 and base <= 36) then
      return nil
    end
    
    if unsigned == nil then
      unsigned = base ~= 10
    end
    
    local value = x.value
    if not unsigned and value < 0 then
      return '-' .. bint.tobase(setmetatable({value = -value}, bint), base, true)
    end
    
    if value == 0 then
      return '0'
    end
    
    local digits = '0123456789abcdefghijklmnopqrstuvwxyz'
    local result = {}
    
    while value > 0 do
      local remainder = value % base
      table.insert(result, 1, digits:sub(remainder + 1, remainder + 1))
      value = math.floor(value / base)
    end
    
    return table.concat(result)
  end
  
  --- Check if zero
  function bint.iszero(x)
    if getmetatable(x) == bint then
      return x.value == 0
    end
    return x == 0
  end
  
  --- Check if one
  function bint.isone(x)
    if getmetatable(x) == bint then
      return x.value == 1
    end
    return x == 1
  end
  
  --- Check if minus one
  function bint.isminusone(x)
    if getmetatable(x) == bint then
      return x.value == -1
    end
    return x == -1
  end
  
  --- Check if bint
  function bint.isbint(x)
    return getmetatable(x) == bint
  end
  
  --- Check if integral
  function bint.isintegral(x)
    return getmetatable(x) == bint or (type(x) == 'number' and x == math.floor(x))
  end
  
  --- Check if numeric
  function bint.isnumeric(x)
    return getmetatable(x) == bint or type(x) == 'number'
  end
  
  --- Get type
  function bint.type(x)
    if getmetatable(x) == bint then
      return 'bint'
    elseif type(x) == 'number' then
      if x == math.floor(x) then
        return 'integer'
      else
        return 'float'
      end
    end
  end
  
  --- Check if negative
  function bint.isneg(x)
    if getmetatable(x) == bint then
      return x.value < 0
    end
    return x < 0
  end
  
  --- Check if positive
  function bint.ispos(x)
    if getmetatable(x) == bint then
      return x.value > 0
    end
    return x > 0
  end
  
  --- Check if even
  function bint.iseven(x)
    if getmetatable(x) == bint then
      return x.value % 2 == 0
    end
    return math.floor(x) % 2 == 0
  end
  
  --- Check if odd
  function bint.isodd(x)
    if getmetatable(x) == bint then
      return x.value % 2 ~= 0
    end
    return math.floor(x) % 2 ~= 0
  end
  
  --- Absolute value
  function bint.abs(x)
    local bx = bint.tobint(x)
    if bx then
      return setmetatable({value = math.abs(bx.value)}, bint)
    end
    return math.abs(x)
  end
  
  --- Increment
  function bint.inc(x)
    local bx = bint.tobint(x, true)
    if bx then
      bx.value = bx.value + 1
      return bx
    end
    return x + 1
  end
  
  --- Decrement
  function bint.dec(x)
    local bx = bint.tobint(x, true)
    if bx then
      bx.value = bx.value - 1
      return bx
    end
    return x - 1
  end
  
  --- Addition
  function bint.__add(x, y)
    local bx, by = bint.tobint(x), bint.tobint(y)
    if bx and by then
      return setmetatable({value = bx.value + by.value}, bint)
    end
    return (tonumber(x) or 0) + (tonumber(y) or 0)
  end
  
  --- Subtraction
  function bint.__sub(x, y)
    local bx, by = bint.tobint(x), bint.tobint(y)
    if bx and by then
      return setmetatable({value = bx.value - by.value}, bint)
    end
    return (tonumber(x) or 0) - (tonumber(y) or 0)
  end
  
  --- Multiplication
  function bint.__mul(x, y)
    local bx, by = bint.tobint(x), bint.tobint(y)
    if bx and by then
      return setmetatable({value = bx.value * by.value}, bint)
    end
    return (tonumber(x) or 0) * (tonumber(y) or 0)
  end
  
  --- Integer division
  function bint.__idiv(x, y)
    local bx, by = bint.tobint(x), bint.tobint(y)
    if bx and by then
      assert(by.value ~= 0, 'attempt to divide by zero')
      -- LUERL handles large integer division correctly
      return setmetatable({value = bx.value // by.value}, bint)
    end
    return math.floor((tonumber(x) or 0) / (tonumber(y) or 1))
  end
  
  --- Float division
  function bint.__div(x, y)
    local nx = getmetatable(x) == bint and x.value or tonumber(x) or 0
    local ny = getmetatable(y) == bint and y.value or tonumber(y) or 1
    return nx / ny
  end
  
  --- Modulo
  function bint.__mod(x, y)
    local bx, by = bint.tobint(x), bint.tobint(y)
    if bx and by then
      assert(by.value ~= 0, 'attempt to divide by zero')
      return setmetatable({value = bx.value % by.value}, bint)
    end
    return (tonumber(x) or 0) % (tonumber(y) or 1)
  end
  
  --- Power
  function bint.__pow(x, y)
    local nx = getmetatable(x) == bint and x.value or tonumber(x) or 0
    local ny = getmetatable(y) == bint and y.value or tonumber(y) or 0
    return nx ^ ny
  end
  
  --- Integer power
  function bint.ipow(x, y)
    local bx, by = bint.tobint(x), bint.tobint(y)
    if bx and by then
      -- Use exponentiation by squaring for efficiency
      local result = setmetatable({value = 1}, bint)
      local base = setmetatable({value = bx.value}, bint)
      local exp = by.value
      
      if exp < 0 then
        return setmetatable({value = 0}, bint)
      end
      
      if exp == 0 then
        return setmetatable({value = 1}, bint)
      end
      
      while exp > 0 do
        if exp % 2 == 1 then
          result = result * base
        end
        base = base * base
        exp = exp // 2
      end
      
      return result
    end
    
    return math.floor((tonumber(x) or 0) ^ (tonumber(y) or 0))
  end
  
  --- Unary minus
  function bint.__unm(x)
    if getmetatable(x) == bint then
      return setmetatable({value = -x.value}, bint)
    end
    return -(tonumber(x) or 0)
  end
  
  --- Bitwise AND
  function bint.__band(x, y)
    local bx, by = bint.tobint(x), bint.tobint(y)
    if bx and by then
      -- LUERL should support bitwise operations on large integers
      return setmetatable({value = bx.value & by.value}, bint)
    end
    return 0
  end
  
  --- Bitwise OR
  function bint.__bor(x, y)
    local bx, by = bint.tobint(x), bint.tobint(y)
    if bx and by then
      return setmetatable({value = bx.value | by.value}, bint)
    end
    return 0
  end
  
  --- Bitwise XOR
  function bint.__bxor(x, y)
    local bx, by = bint.tobint(x), bint.tobint(y)
    if bx and by then
      return setmetatable({value = bx.value ~ by.value}, bint)
    end
    return 0
  end
  
  --- Bitwise NOT
  function bint.__bnot(x)
    if getmetatable(x) == bint then
      return setmetatable({value = ~x.value}, bint)
    end
    return ~(tonumber(x) or 0)
  end
  
  --- Left shift
  function bint.__shl(x, y)
    local bx = bint.tobint(x)
    local shiftn = tonumber(y) or 0
    if bx then
      return setmetatable({value = bx.value << shiftn}, bint)
    end
    return (tonumber(x) or 0) << shiftn
  end
  
  --- Right shift
  function bint.__shr(x, y)
    local bx = bint.tobint(x)
    local shiftn = tonumber(y) or 0
    if bx then
      return setmetatable({value = bx.value >> shiftn}, bint)
    end
    return (tonumber(x) or 0) >> shiftn
  end
  
  --- Equality
  function bint.__eq(x, y)
    -- Both must be bints for metamethod to be called
    return x.value == y.value
  end
  
  --- General equality check
  function bint.eq(x, y)
    local bx, by = bint.tobint(x), bint.tobint(y)
    if bx and by then
      return bx.value == by.value
    end
    return x == y
  end
  
  --- Less than
  function bint.__lt(x, y)
    local bx, by = bint.tobint(x), bint.tobint(y)
    if bx and by then
      return bx.value < by.value
    end
    return (tonumber(x) or 0) < (tonumber(y) or 0)
  end
  
  --- Less than or equal
  function bint.__le(x, y)
    local bx, by = bint.tobint(x), bint.tobint(y)
    if bx and by then
      return bx.value <= by.value
    end
    return (tonumber(x) or 0) <= (tonumber(y) or 0)
  end
  
  --- To string
  function bint:__tostring()
    return tostring(self.value)
  end
  
  --- Division and modulo operations
  function bint.idivmod(x, y)
    local bx, by = bint.tobint(x), bint.tobint(y)
    if bx and by then
      assert(by.value ~= 0, 'attempt to divide by zero')
      local q = bx.value // by.value
      local r = bx.value % by.value
      return setmetatable({value = q}, bint), setmetatable({value = r}, bint)
    end
    local nx, ny = tonumber(x) or 0, tonumber(y) or 1
    return math.floor(nx / ny), nx % ny
  end
  
  --- Max/min functions
  function bint.max(x, y)
    local bx, by = bint.tobint(x), bint.tobint(y)
    if bx and by then
      return setmetatable({value = math.max(bx.value, by.value)}, bint)
    end
    return math.max(tonumber(x) or 0, tonumber(y) or 0)
  end
  
  function bint.min(x, y)
    local bx, by = bint.tobint(x), bint.tobint(y)
    if bx and by then
      return setmetatable({value = math.min(bx.value, by.value)}, bint)
    end
    return math.min(tonumber(x) or 0, tonumber(y) or 0)
  end
  
  -- In-place operations for API compatibility
  function bint:_add(y)
    local by = bint.tobint(y)
    if by then
      self.value = self.value + by.value
    end
    return self
  end
  
  function bint:_sub(y)
    local by = bint.tobint(y)
    if by then
      self.value = self.value - by.value
    end
    return self
  end
  
  function bint:_unm()
    self.value = -self.value
    return self
  end
  
  function bint:_abs()
    self.value = math.abs(self.value)
    return self
  end
  
  function bint:_inc()
    self.value = self.value + 1
    return self
  end
  
  function bint:_dec()
    self.value = self.value - 1
    return self
  end
  
  -- Allow calling bint as constructor
  setmetatable(bint, {
    __call = function(_, x)
      return bint.new(x)
    end
  })
  
  memo[memoindex] = bint
  return bint
end

return newmodule