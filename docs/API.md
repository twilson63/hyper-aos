# Hyper-AOS API Reference

## Overview

This document provides comprehensive API reference for Hyper-AOS, covering the core functions in `aos.lua`, the complete `utils` module, message structures, and usage examples.

## Core aos.lua API

### Primary Functions

#### `compute(message)`

The main computation function that processes incoming AO messages.

**Parameters:**
- `message` (table): The incoming AO message to process

**Returns:**
- `{ok, State}` (tuple): Success result with updated global state
- `{error, Reason}` (tuple): Error result with failure reason

**Description:**
Processes incoming messages through the AO protocol pipeline, including security validation, state management, and response generation.

**Example:**
```lua
local message = {
  type = "message",
  action = "eval", 
  data = "print('Hello AO')",
  commitments = {
    ["tx_id"] = {
      type = "RSA-PSS-512",
      committer = "owner_address_43_chars",
      commit = "signature_hash"
    }
  }
}

local result = compute(message)
-- Returns: {ok, updated_state}
```

#### `eval(code)`

Executes Lua code within the AO process context.

**Parameters:**
- `code` (string): Lua code to execute

**Returns:**
- Mixed: Result of the evaluated code
- Updates `_G._OUTPUT` with execution results

**Description:**
Safely executes user-provided Lua code with access to the global process state and utils functions.

**Example:**
```lua
eval("_G.counter = (_G.counter or 0) + 1")
eval("return _G.counter") -- Returns: 1

-- Using utils functions
eval("_G.numbers = utils.map(function(x) return x * 2 end, {1,2,3})")
-- _G.numbers becomes {2, 4, 6}
```

#### `send(message)`

Sends a message to another AO process.

**Parameters:**
- `message` (table): Message structure to send

**Returns:**
- Message with updated fields and formatting

**Description:**
Formats and prepares messages for transmission to other AO processes, applying colorization and validation.

**Example:**
```lua
local msg = {
  target = "target_process_id",
  action = "transfer",
  data = "100",
  tags = {
    {name = "quantity", value = "100"},
    {name = "recipient", value = "recipient_address"}
  }
}

send(msg)
-- Outputs formatted, colorized message
```

### Security Functions (Private meta table)

#### `meta.init(message)`

Initializes process state from the first Process message.

**Parameters:**
- `message` (table): Process initialization message

**Effects:**
- Sets `_G.id` from first non-HMAC commitment key
- Sets `_G.owner` from commitment committer
- Parses `_G.authorities` from authority field
- Marks process as initialized

**Example Process Message:**
```lua
{
  type = "process",
  authority = "addr1,addr2,addr3",
  commitments = {
    ["process_id"] = {
      type = "RSA-PSS-512", 
      committer = "owner_address_43_chars",
      commit = "signature"
    }
  }
}
```

#### `meta.is_owner(message)`

Validates if message sender is the process owner.

**Parameters:**
- `message` (table): Message to validate

**Returns:**
- `true` if sender is owner, `false` otherwise

**Validation Logic:**
```lua
-- Checks non-HMAC commitments against _G.owner
for key, commitment in pairs(message.commitments) do
  if commitment.type ~= "hmac-sha256" and 
     commitment.committer == _G.owner then
    return true
  end
end
return false
```

#### `meta.is_trusted(message)`

Validates if message sender is a trusted authority.

**Parameters:**
- `message` (table): Message to validate

**Returns:**
- `true` if sender is trusted authority, `false` otherwise

**Validation Logic:**
```lua
-- Verifies from-process matches and is in authorities list
if message["from-process"] and message["from-process"] == message.From then
  for _, auth in ipairs(_G.authorities) do
    if auth == message["from-process"] then
      return true
    end
  end
end
```

#### `meta.ensure_message(message)`

Ensures message has proper `from` field with priority resolution.

**Parameters:**
- `message` (table): Message to process

**Returns:**
- `message` (table): Message with resolved `from` field

**Resolution Priority:**
1. Existing `from` field (highest priority)
2. `from-process` field
3. First commitment committer (lowest priority)

### Utility Functions (Private meta table)

#### `stringify(object, depth)`

Converts Lua objects to formatted, colorized strings.

**Parameters:**
- `object` (any): Object to stringify
- `depth` (number, optional): Maximum recursion depth (default: 3)

**Returns:**
- `string`: Formatted string representation

**Features:**
- ANSI color coding for different types
- Nested table handling with depth limiting
- Array vs object detection
- Circular reference protection

**Color Coding:**
- Strings: Green
- Numbers: Blue  
- Booleans: Yellow
- nil: Gray
- Functions: Magenta
- Tables: White brackets

**Example:**
```lua
local data = {
  name = "Alice",
  age = 30,
  scores = {95, 87, 92},
  active = true
}

print(stringify(data))
-- Outputs colorized:
-- {
--   name = "Alice",      -- Green
--   age = 30,           -- Blue  
--   scores = {95, 87, 92}, -- Blue numbers
--   active = true       -- Yellow
-- }
```

## Utils Module API

### Pattern Matching Functions

#### `utils.matchesPattern(pattern, value, message)`

Determines if a value matches a given pattern with context.

**Parameters:**
- `pattern` (any): Pattern to match against
- `value` (any): Value to test
- `message` (table): Message context for function patterns

**Returns:**
- `boolean`: True if pattern matches

**Pattern Types:**
- `'_'`: Wildcard (always matches)
- `function`: Called with (value, message) - matches if returns true
- `string`: Regex or exact match
- `table`: Matches if any sub-pattern matches
- Other types: Exact equality match

**Examples:**
```lua
-- Wildcard
utils.matchesPattern('_', "anything", {}) -- true

-- Function pattern  
local isPositive = function(val) return type(val) == "number" and val > 0 end
utils.matchesPattern(isPositive, 5, {}) -- true
utils.matchesPattern(isPositive, -3, {}) -- false

-- Regex pattern
utils.matchesPattern("^test.*", "test123", {}) -- true
utils.matchesPattern("^test.*", "demo", {}) -- false

-- Exact match
utils.matchesPattern("hello", "hello", {}) -- true
utils.matchesPattern("hello", "world", {}) -- false

-- Table pattern (any sub-pattern)
utils.matchesPattern({"admin", "user"}, "admin", {}) -- true
```

#### `utils.matchesSpec(message, spec)`

Validates if a message matches a specification object.

**Parameters:**
- `message` (table): Message to validate
- `spec` (table): Specification with field patterns

**Returns:**
- `boolean`: True if message matches all spec patterns

**Example:**
```lua
local message = {
  action = "transfer",
  from = "alice", 
  to = "bob",
  quantity = "100"
}

local spec = {
  action = "transfer",
  from = function(val) return type(val) == "string" and #val > 0 end,
  to = {"alice", "bob", "charlie"},
  quantity = "^%d+$" -- numeric string regex
}

utils.matchesSpec(message, spec) -- true
```

### Functional Programming Functions

#### `utils.map(fn, array)`

Applies a function to each element of an array, returning a new array.

**Parameters:**
- `fn` (function): Function to apply to each element
- `array` (table): Input array

**Returns:**
- `table`: New array with function applied to each element

**Example:**
```lua
local numbers = {1, 2, 3, 4, 5}
local doubled = utils.map(function(x) return x * 2 end, numbers)
-- Result: {2, 4, 6, 8, 10}

local names = {"alice", "bob", "charlie"}  
local upper = utils.map(string.upper, names)
-- Result: {"ALICE", "BOB", "CHARLIE"}
```

#### `utils.filter(predicate, array)`

Filters an array based on a predicate function.

**Parameters:**
- `predicate` (function): Function that returns true for elements to keep
- `array` (table): Input array

**Returns:**
- `table`: New array containing only elements that match predicate

**Example:**
```lua
local numbers = {1, 2, 3, 4, 5, 6}
local evens = utils.filter(function(x) return x % 2 == 0 end, numbers)
-- Result: {2, 4, 6}

local words = {"apple", "banana", "apricot", "cherry"}
local startsWithA = utils.filter(function(w) return string.sub(w, 1, 1) == "a" end, words)
-- Result: {"apple", "apricot"}
```

#### `utils.reduce(fn, initial, array)`

Reduces an array to a single value using an accumulator function.

**Parameters:**
- `fn` (function): Reducer function (accumulator, current_value) -> new_accumulator
- `initial` (any): Initial accumulator value
- `array` (table): Input array

**Returns:**
- `any`: Final accumulated value

**Example:**
```lua
local numbers = {1, 2, 3, 4, 5}
local sum = utils.reduce(function(acc, x) return acc + x end, 0, numbers)
-- Result: 15

local words = {"hello", "world", "from", "aos"}
local sentence = utils.reduce(function(acc, word) return acc .. " " .. word end, "", words)
-- Result: " hello world from aos"
```

#### `utils.curry(fn)`

Creates a curried version of a function (partial application support).

**Parameters:**
- `fn` (function): Function to curry

**Returns:**
- `function`: Curried function

**Note:** Limited LUERL support - use direct calls when possible.

**Example:**
```lua
local add = function(a, b) return a + b end
local curriedAdd = utils.curry(add)
local add5 = curriedAdd(5)
local result = add5(3) -- 8
```

#### `utils.compose(...)`

Composes multiple functions into a single function (right-to-left evaluation).

**Parameters:**
- `...` (functions): Functions to compose

**Returns:**
- `function`: Composed function

**Example:**
```lua
local addOne = function(x) return x + 1 end
local double = function(x) return x * 2 end
local addOneThenDouble = utils.compose(double, addOne)

local result = addOneThenDouble(5) -- (5 + 1) * 2 = 12
```

### Array Operations

#### `utils.concat(arr1, arr2)`

Concatenates two arrays into a new array.

**Parameters:**
- `arr1` (table): First array
- `arr2` (table): Second array  

**Returns:**
- `table`: New array containing all elements from both arrays

**Example:**
```lua
local first = {1, 2, 3}
local second = {4, 5, 6}
local combined = utils.concat(first, second)
-- Result: {1, 2, 3, 4, 5, 6}
```

#### `utils.reverse(array)`

Returns a new array with elements in reverse order.

**Parameters:**
- `array` (table): Input array

**Returns:**
- `table`: New array with reversed elements

**Example:**
```lua
local original = {1, 2, 3, 4, 5}
local reversed = utils.reverse(original)
-- Result: {5, 4, 3, 2, 1}
```

#### `utils.find(predicate, array)`

Finds the first element matching a predicate.

**Parameters:**
- `predicate` (function): Function to test elements
- `array` (table): Input array

**Returns:**
- `any`: First matching element, or nil if not found

**Example:**
```lua
local numbers = {1, 3, 5, 8, 9}
local firstEven = utils.find(function(x) return x % 2 == 0 end, numbers)
-- Result: 8

local people = {
  {name = "Alice", age = 30},
  {name = "Bob", age = 25},
  {name = "Charlie", age = 35}
}
local adult = utils.find(function(p) return p.age >= 30 end, people)
-- Result: {name = "Alice", age = 30}
```

#### `utils.includes(array, value)`

Checks if an array contains a specific value.

**Parameters:**
- `array` (table): Array to search
- `value` (any): Value to find

**Returns:**
- `boolean`: True if value is found

**Example:**
```lua
local fruits = {"apple", "banana", "cherry"}
utils.includes(fruits, "banana") -- true
utils.includes(fruits, "grape") -- false

local numbers = {1, 2, 3, 4, 5}
utils.includes(numbers, 3) -- true
utils.includes(numbers, 7) -- false
```

### Object Operations

#### `utils.prop(key, object)`

Safely retrieves a property value from an object.

**Parameters:**
- `key` (string): Property key to retrieve
- `object` (table): Object to query

**Returns:**
- `any`: Property value, or nil if not found

**Example:**
```lua
local person = {name = "Alice", age = 30, city = "Portland"}
local name = utils.prop("name", person) -- "Alice"
local country = utils.prop("country", person) -- nil
```

#### `utils.propEq(key, value, object)`

Tests if an object property equals a specific value.

**Parameters:**
- `key` (string): Property key
- `value` (any): Expected value
- `object` (table): Object to test

**Returns:**
- `boolean`: True if property equals value

**Example:**
```lua
local person = {name = "Alice", age = 30}
utils.propEq("name", "Alice", person) -- true
utils.propEq("age", 25, person) -- false
utils.propEq("city", "Portland", person) -- false (property doesn't exist)
```

#### `utils.keys(object)`

Returns array of object keys.

**Parameters:**
- `object` (table): Object to get keys from

**Returns:**
- `table`: Array of keys

**Example:**
```lua
local person = {name = "Alice", age = 30, city = "Portland"}
local keyList = utils.keys(person) 
-- Result: {"name", "age", "city"} (order may vary)
```

#### `utils.values(object)`

Returns array of object values.

**Parameters:**
- `object` (table): Object to get values from

**Returns:**
- `table`: Array of values

**Example:**
```lua
local person = {name = "Alice", age = 30, city = "Portland"}
local valueList = utils.values(person)
-- Result: {"Alice", 30, "Portland"} (order may vary)
```

## Message Structures

### Process Message

Used for process initialization and setup.

```lua
{
  type = "process",                    -- Required: Message type
  authority = "addr1,addr2,addr3",     -- Optional: Comma-separated authorities
  commitments = {                      -- Required: Authentication commitments
    ["process_id"] = {
      type = "RSA-PSS-512",           -- Commitment type
      committer = "43_char_address",   -- Process owner address
      commit = "signature_hash"        -- Cryptographic signature
    }
  }
}
```

### Regular Message

Standard message for process interaction.

```lua
{
  type = "message",                    -- Required: Message type
  action = "eval",                     -- Required: Action to perform
  data = "print('Hello')",            -- Optional: Message data
  from = "sender_address",            -- Optional: Explicit sender
  ["from-process"] = "sender_addr",   -- Optional: Originating process
  commitments = {                     -- Required: Authentication
    ["message_id"] = {
      type = "RSA-PSS-512",
      committer = "sender_address",
      commit = "signature"
    }
  },
  tags = {                           -- Optional: Additional metadata
    {name = "action", value = "transfer"},
    {name = "quantity", value = "100"}
  }
}
```

### Response Format

Standard response from `compute()` function.

```lua
-- Success response
{ok, {
  -- Global state (_G contents minus system keys)
  id = "process_id",
  owner = "owner_address", 
  authorities = {"addr1", "addr2"},
  Inbox = {message1, message2},
  user_variable = "user_value",
  utils = utils_module
}}

-- Error response  
{error, "Error description"}
```

## Error Handling

### Common Error Types

#### Security Errors
- `"Unauthorized: Not owner"` - Message sender is not process owner
- `"Unauthorized: Not trusted"` - Message sender is not in authorities
- `"Invalid commitment"` - Commitment validation failed

#### Processing Errors
- `"Invalid message format"` - Required fields missing
- `"Evaluation error: <details>"` - Lua code execution failed
- `"State corruption"` - Global state in invalid condition

#### System Errors
- `"LUERL error: <details>"` - Sandbox execution error
- `"Memory limit exceeded"` - Resource constraints violated
- `"Timeout"` - Operation exceeded time limit

### Error Handling Examples

```lua
-- Safe evaluation with error handling
local success, result = pcall(function()
  return compute(message)
end)

if not success then
  print("Error:", result)
else
  local status, state = table.unpack(result)
  if status == "ok" then
    print("Success:", state)
  else
    print("Compute error:", state)
  end
end

-- Pattern matching with validation
local isValid = utils.matchesSpec(message, {
  action = function(val) return type(val) == "string" end,
  from = "^[a-zA-Z0-9_-]{43}$" -- Arweave address format
})

if not isValid then
  error("Invalid message format")
end
```

## Best Practices

### Security
- Always validate message formats before processing
- Use `meta.is_owner()` for ownership checks
- Use `meta.is_trusted()` for authority validation
- Never bypass security functions

### Performance  
- Use utils functions for array operations (LUERL optimized)
- Minimize string concatenation in loops
- Prefer direct table access over function calls
- Cache pattern compilations when possible

### State Management
- Store persistent data in `_G` namespace
- Use descriptive variable names
- Clean up temporary variables
- Avoid circular references in stored data

### Testing
- Write comprehensive test cases
- Test both success and error scenarios
- Validate security mechanisms
- Test with realistic data volumes

### Code Organization
- Group related functions together
- Use clear, descriptive function names
- Document complex logic with comments
- Follow consistent coding style

This API reference provides the foundation for building robust AO processes using Hyper-AOS components.