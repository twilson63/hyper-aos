# Sandbox Requirements for HyperAOS Modules

## Overview

This document defines the complete runtime environment requirements for executing HyperAOS modules (aos.lua and utils.lua) in sandboxed environments. These requirements ensure proper module functionality while maintaining security isolation.

## Core Philosophy

The sandbox environment must:
1. **Provide** all necessary Lua standard libraries for module execution
2. **Restrict** dangerous operations that could compromise security
3. **Maintain** compatibility with LUERL VM execution model
4. **Mimic** the actual AO compute environment

## Required Global Functions and Libraries

### Essential Tables and Namespaces

#### `_G` (Global Table)
- **Purpose**: Root namespace containing all globals
- **Requirement**: Must self-reference (`_G._G = _G`)
- **Security**: Contains filtered set of safe globals

#### `table` Library
- **Purpose**: Table manipulation operations
- **Required Functions**:
  - `table.insert` - Add elements to arrays
  - `table.remove` - Remove elements from arrays  
  - `table.concat` - Concatenate array elements
  - `table.sort` - Sort array elements
  - `table.unpack` or `unpack` - Unpack array to multiple values
- **Used By**: aos.lua (line 748), utils.lua (throughout)

#### `string` Library
- **Purpose**: String manipulation and pattern matching
- **Required Functions**:
  - `string.match` - Pattern matching
  - `string.gmatch` - Global pattern matching iterator
  - `string.gsub` - Pattern substitution
  - `string.find` - Find patterns in strings
  - `string.sub` - Extract substring
  - `string.format` - String formatting
  - `string.byte` - Convert to byte values
  - `string.char` - Convert from byte values
  - `string.len` - String length
  - `string.lower` - Convert to lowercase
  - `string.upper` - Convert to uppercase
  - `string.rep` - Repeat string
  - `string.reverse` - Reverse string
- **Used By**: aos.lua, utils.lua pattern matching

#### `math` Library
- **Purpose**: Mathematical operations
- **Required Functions**:
  - All standard math functions (sin, cos, sqrt, random, etc.)
  - `math.huge` - Infinity constant
  - `math.pi` - Pi constant
- **Used By**: Optional for calculations

### Core Language Functions

#### Type System
- `type(value)` - Get type of value
- `tostring(value)` - Convert to string
- `tonumber(value [, base])` - Convert to number
- `rawequal(v1, v2)` - Raw equality check
- `rawget(table, key)` - Raw table access
- `rawset(table, key, value)` - Raw table assignment
- `rawlen(table)` - Raw length operator

#### Iteration
- `pairs(table)` - Iterate over all key-value pairs
- `ipairs(array)` - Iterate over array indices
- `next(table [, index])` - Get next key-value pair
- `select(index, ...)` - Select from variable arguments

#### Metatables
- `setmetatable(table, metatable)` - Set metatable
- `getmetatable(object)` - Get metatable

#### Error Handling
- `pcall(func, ...)` - Protected call
- `xpcall(func, errhandler, ...)` - Protected call with error handler
- `error(message [, level])` - Raise error
- `assert(condition [, message])` - Assert condition

#### Code Loading
- `load(chunk [, chunkname [, mode [, env]]])` - Load Lua code
- `loadstring(string [, chunkname])` - Load string as code (Lua 5.1 compat)

### Module System

#### `package` Table
- **Purpose**: Module loading system
- **Required Structure**:
  ```lua
  package = {
      loaded = {},     -- Loaded modules cache
      path = "",       -- Lua module search path
      cpath = "",      -- C module search path
      preload = {}     -- Preloaded module functions
  }
  ```
- **Security Note**: `require` function should be stubbed or restricted

### Optional but Recommended

#### `coroutine` Library
- **Purpose**: Cooperative multitasking
- **Functions**: create, resume, yield, status, wrap, running
- **Used By**: Advanced async patterns

#### `utf8` Library (Lua 5.3+)
- **Purpose**: UTF-8 string handling
- **Functions**: len, char, codes, codepoint, offset
- **Used By**: International text processing

#### `os` Library (Restricted)
- **Safe Functions Only**:
  - `os.date` - Date/time formatting
  - `os.time` - Get timestamp
  - `os.clock` - CPU time
  - `os.difftime` - Time difference
- **Blocked Functions**:
  - `os.execute` - System commands (DANGEROUS)
  - `os.exit` - Process termination (DANGEROUS)
  - `os.getenv` - Environment variables (INFORMATION LEAK)
  - `os.remove` - File deletion (DANGEROUS)
  - `os.rename` - File operations (DANGEROUS)
  - `os.setlocale` - Locale changes (SIDE EFFECTS)
  - `os.tmpname` - Temp files (DANGEROUS)

#### `io` Library (Highly Restricted)
- **Safe Functions Only**:
  - `io.open` - For reading only, specific paths
  - File handle `:read()` methods
  - File handle `:close()` methods
- **Blocked Functions**:
  - `io.write` - Writing to files (DANGEROUS)
  - `io.popen` - Process execution (DANGEROUS)
  - `io.tmpfile` - Temp files (DANGEROUS)
  - `io.output` - Change output (DANGEROUS)
  - `io.input` - Change input (DANGEROUS)

## Blocked/Dangerous Functions

The following should NEVER be available in the sandbox:

### System Access
- `os.execute` - Execute system commands
- `os.exit` - Terminate process
- `io.popen` - Execute commands with I/O
- `dofile` - Load and execute file
- `loadfile` - Load file as function

### File System Writes
- `io.write` - Write to files
- `io.output` - Change output stream
- `os.remove` - Delete files
- `os.rename` - Move/rename files

### Network Access
- Any socket libraries
- HTTP client libraries
- Database connections

### Process Control
- `debug` library (entire library)
- FFI (Foreign Function Interface)
- C module loading

## Environment Validation Checklist

Before executing modules, validate that:

✅ `_G` table exists and self-references  
✅ `table` library with all functions present  
✅ `string` library with all functions present  
✅ `math` library with all functions present  
✅ Core functions (type, tostring, pairs, etc.) available  
✅ `package.loaded` table exists  
✅ Metatable functions available  
✅ Error handling functions available  
✅ No dangerous functions exposed  
✅ I/O operations properly restricted  

## LUERL-Specific Considerations

### Binary String Keys
- LUERL may handle binary strings differently
- Ensure proper encoding for message field keys
- Test with actual LUERL runtime

### Partial Application Issues
- LUERL has known issues with curry/partial application
- Utils module has workarounds for these limitations
- Test functional programming patterns carefully

### Type Conversions
- Erlang maps ↔ Lua tables conversion
- Number precision differences
- String encoding considerations

## Testing the Sandbox

### Basic Validation
```lua
-- Test essential globals exist
assert(_G ~= nil, "_G missing")
assert(table ~= nil, "table library missing")
assert(string ~= nil, "string library missing")
assert(type(pairs) == "function", "pairs missing")
```

### Module Loading Test
```lua
-- Test module system
assert(package ~= nil, "package missing")
assert(package.loaded ~= nil, "package.loaded missing")
```

### Security Test
```lua
-- Ensure dangerous functions are blocked
assert(os.execute == nil, "os.execute should be blocked")
assert(io.popen == nil, "io.popen should be blocked")
assert(loadfile == nil, "loadfile should be blocked")
```

## Diagnostic Mode Support

The test environment supports diagnostic modes:

### Verbose Mode (`VERBOSE=1`)
- Shows execution flow
- Reports environment validation progress
- Logs module loading milestones

### Diagnostic Mode (`DIAGNOSTIC=1`)
- Tracks all global accesses
- Reports standard library usage
- Identifies missing dependencies
- Shows usage frequency statistics

### Usage
```bash
# Standard run
lua scripts/test.lua

# With diagnostics
DIAGNOSTIC=1 lua scripts/test.lua

# Full verbosity
VERBOSE=1 DIAGNOSTIC=1 lua scripts/test.lua
```

## Maintenance Notes

### Adding New Requirements
1. Update this document with new globals
2. Add to sandbox environment in `scripts/test.lua`
3. Update environment validation function
4. Add tests for new requirements

### Removing Requirements
1. Verify no modules use the global
2. Remove from sandbox environment
3. Update this documentation
4. Update validation tests

## Version History

- **2025-08-06**: Initial documentation created
- **2025-08-06**: Added diagnostic mode support
- **2025-08-06**: Enhanced with LUERL-specific considerations

## References

- [Lua 5.3 Reference Manual](https://www.lua.org/manual/5.3/)
- [LUERL Documentation](https://github.com/rvirding/luerl)
- [AO Protocol Specification](https://ao.arweave.net/)
- [HyperBEAM Security Model](https://github.com/permaweb/hyperbeam)