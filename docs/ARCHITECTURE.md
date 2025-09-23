# Hyper-AOS Architecture Documentation

## System Overview

Hyper-AOS is a high-performance implementation of the AO (Arweave Operating System) protocol using Hyperbeam's native Lua device with LUERL sandboxing on the BEAM platform. The system provides a secure, colorized console for interacting with AO processes through message passing in a decentralized supercomputer.

## Design Principles

### Security First
- **Commitment-based validation**: All operations validated through RSA-PSS-512 commitments
- **Authority-based trust**: Granular permission system using trusted authorities
- **Sandboxed execution**: LUERL provides isolated execution environment
- **State immutability**: Critical state protected from external modification

### Performance Optimized
- **Native Lua execution**: Direct execution on Hyperbeam's Lua device
- **LUERL optimizations**: Tailored for Erlang VM performance characteristics
- **Efficient state management**: Direct `_G` namespace usage for persistence
- **Minimal overhead**: Streamlined message processing pipeline

### Developer Experience
- **Colorized output**: Enhanced terminal experience with syntax highlighting
- **Comprehensive testing**: Full EUnit test suite with 51+ test cases
- **Clear error messages**: Detailed feedback for debugging
- **Functional programming**: Rich utils library for message processing

## Component Architecture

### Core Components

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│    aos.lua      │    │   utils.lua     │    │  LUERL Sandbox  │
│                 │    │                 │    │                 │
│ • Message proc  │◄───┤ • Functional    │    │ • Secure exec   │
│ • State mgmt    │    │   utilities     │    │ • Type safety   │
│ • Security      │    │ • Pattern match │    │ • Isolation     │
│ • Color output  │    │ • Array ops     │    │                 │
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                       │                       │
         └───────────────────────┼───────────────────────┘
                                 │
                    ┌─────────────────┐
                    │   Test Suite    │
                    │                 │
                    │ • EUnit tests   │
                    │ • Security      │
                    │ • Integration   │
                    │ • Utils         │
                    └─────────────────┘
```

### 1. aos.lua - Core Compute Module

**Purpose**: Main computation engine implementing AO protocol compliance

**Key Responsibilities**:
- Process initialization from commitment messages
- Message validation and security enforcement
- State persistence and management
- Output formatting and colorization
- Trust verification and authority management

**Architecture**:
```lua
-- Global state in _G namespace
_G.id          -- Process identifier
_G.owner       -- Process owner address  
_G.authorities -- Trusted authority addresses
_G.Inbox       -- Message inbox
_G.utils       -- Functional utilities

-- Private meta table (local scope)
meta = {
  initialized = false,
  init = function(msg),
  is_owner = function(msg),
  is_trusted = function(msg),
  ensure_message = function(msg)
}
```

**Security Model**:
- Owner validation through RSA-PSS-512 commitment verification
- Authority-based trust system for privileged operations
- Message authentication with commitment chain validation
- Protected state variables using local meta table

### 2. utils.lua - Functional Programming Library

**Purpose**: Comprehensive functional programming utilities optimized for LUERL

**Key Responsibilities**:
- Pattern matching for message specifications
- Functional primitives (map, filter, reduce, compose)
- Array operations (concat, reverse, find, includes)
- Object property access and manipulation
- LUERL-specific optimizations and type safety

**Architecture**:
```lua
_G.utils = {
  -- Pattern Matching
  matchesPattern = function(pattern, value, msg),
  matchesSpec = function(msg, spec),
  
  -- Functional Primitives  
  map = function(fn, array),
  filter = function(predicate, array),
  reduce = function(fn, initial, array),
  curry = function(fn),
  compose = function(...),
  
  -- Array Operations
  concat = function(arr1, arr2),
  reverse = function(array),
  find = function(predicate, array),
  includes = function(array, value),
  
  -- Object Operations
  prop = function(key, obj),
  propEq = function(key, value, obj),
  keys = function(obj),
  values = function(obj)
}
```

**LUERL Optimizations**:
- Use `#array` operator instead of `ipairs` for length calculation
- Explicit nil handling for edge cases
- Type safety checks to prevent runtime errors
- Reduced function call overhead with direct implementations

### 3. LUERL Sandbox Environment

**Purpose**: Secure Lua execution environment on the BEAM platform

**Key Responsibilities**:
- Code isolation and security enforcement
- Type conversion between Erlang and Lua
- Resource limiting and execution control
- Error handling and recovery

**Integration Points**:
```erlang
% Erlang test interface
{ok, State1} = luerl:eval(LuaCode, State0),
{ok, Result} = luerl:call_function([aos, compute], [Message], State1),
LuaValue = luerl:decode(Result, State1).
```

### 4. Test Suite Framework

**Purpose**: Comprehensive validation of all system components

**Test Categories**:
- **Security Tests**: Owner validation, authority checks, trust verification
- **Message Processing**: State persistence, message handling, validation
- **Output Tests**: Color formatting, table stringification  
- **Utils Tests**: Functional programming operations, pattern matching
- **Integration Tests**: Multi-step scenarios, end-to-end flows

## Message Flow and Processing

### 1. Message Ingestion

```
Incoming Message
      │
      ▼
┌─────────────────┐
│ Dedupe Check    │ ◄─── Hyperbeam dedupe device
└─────────────────┘
      │
      ▼
┌─────────────────┐
│ Message         │
│ Normalization   │
└─────────────────┘
      │
      ▼
┌─────────────────┐
│ Security        │
│ Validation      │
└─────────────────┘
      │
      ▼
┌─────────────────┐
│ aos.compute()   │
│ Processing      │
└─────────────────┘
```

### 2. Security Validation Pipeline

```
Message Commitments
      │
      ▼
┌─────────────────┐
│ RSA-PSS-512     │ ◄─── meta.is_owner()
│ Validation      │
└─────────────────┘
      │
      ▼
┌─────────────────┐
│ Authority       │ ◄─── meta.is_trusted()
│ Verification    │
└─────────────────┘
      │
      ▼
┌─────────────────┐
│ From Field      │ ◄─── meta.ensure_message()
│ Resolution      │
└─────────────────┘
      │
      ▼
┌─────────────────┐
│ State Update    │
│ & Response      │
└─────────────────┘
```

### 3. State Management

**Global State (_G namespace)**:
```lua
-- Process metadata
_G.id = "process_identifier"
_G.owner = "43_char_arweave_address" 
_G.authorities = {"addr1", "addr2", "addr3"}

-- Message handling
_G.Inbox = {message1, message2, ...}
_G.MAX_INBOX_SIZE = 10000

-- Utilities
_G.utils = utils_module
_G.Utils = utils_module  -- backwards compatibility

-- User state (eval persistence)
_G.user_variable_1 = value
_G.user_variable_2 = value
```

**Private State (local meta table)**:
```lua
local meta = {
  initialized = false,
  -- Security functions
  init = function(msg),
  is_owner = function(msg),
  is_trusted = function(msg), 
  ensure_message = function(msg),
  -- Output functions
  removeCR = function(str),
  isSimpleArray = function(t),
  stringify = function(obj)
}
```

## Security Model

### Process Initialization Security

**Owner Establishment**:
1. Extract first non-HMAC commitment from Process message
2. Validate RSA-PSS-512 signature 
3. Set committer as `_G.owner`
4. Store commitment key as `_G.id`

**Authority Setup**:
1. Parse comma-separated authority string
2. Validate 43-character Arweave addresses
3. Store in `_G.authorities` array
4. Enable trust verification for future messages

### Message Validation Security

**Ownership Validation (`meta.is_owner`)**:
```lua
-- Check if any non-HMAC commitment matches process owner
for key, commitment in pairs(msg.commitments) do
  if commitment.type ~= "hmac-sha256" and 
     commitment.committer == _G.owner then
    return true
  end
end
return false
```

**Trust Validation (`meta.is_trusted`)**:
```lua
-- Verify from-process matches message and committer is authorized
if msg["from-process"] and msg["from-process"] == msg.From then
  for _, auth in ipairs(_G.authorities) do
    if auth == msg["from-process"] then
      return true
    end
  end
end
return false
```

### State Protection

**System Key Exclusion**:
- Lua built-ins excluded from state serialization
- Private meta functions inaccessible from eval()
- System components protected from modification
- User state cleanly separated and persisted

## Integration Points

### Hyperbeam Integration

**Device Integration**:
- Native Lua device for performance
- Dedupe device for message uniqueness
- Authority validation through commitment chain
- Process lifecycle management

**Message Protocol**:
- Binary string keys for message fields
- Commitment-based authentication  
- From-process resolution and validation
- State persistence across compute calls

### Arweave Integration

**Transaction Publishing**:
- Module deployment via ARX toolkit
- Data protocol tagging for AO compliance
- Wallet-based authentication
- Transaction ID for process instantiation

**Address Validation**:
- 43-character base64url addresses
- RSA-PSS-512 signature verification
- Public key derivation from commitments
- Authority address parsing and validation

### Test Framework Integration

**EUnit Test Structure**:
- Profile-based conditional compilation
- Helper modules for common test patterns
- Assertion macros for validation
- Test data generation and fixtures

**LUERL Test Integration**:
- Lua code evaluation in Erlang context
- State persistence testing
- Type conversion validation
- Error handling verification

## Performance Considerations

### Optimization Strategies

**LUERL Specific Optimizations**:
- Use `#array` operator instead of `ipairs`
- Minimize function call overhead
- Direct table access for performance
- Reduced string concatenation in output

**Memory Management**:
- Inbox size limiting (MAX_INBOX_SIZE = 10000)
- State cleanup through system key exclusion
- Efficient table operations in utils
- Minimal object creation in hot paths

**Execution Efficiency**:
- Native Lua device execution
- Cached pattern compilation
- Optimized color output formatting
- Streamlined security validation

### Scalability Factors

**Message Throughput**:
- Parallel message processing capability
- Efficient state persistence
- Minimal computation overhead
- Fast commitment validation

**Resource Usage**:
- Controlled memory footprint
- CPU-efficient operations
- Network optimization through dedupe
- Storage efficiency in state management

## Extension Points

### Custom Modules

**Loading Mechanism**:
- Global namespace integration (_G.custom_module)
- Compatibility with existing utils pattern
- Version tracking and management
- Backwards compatibility maintenance

**Development Guidelines**:
- Follow LUERL optimization patterns
- Implement comprehensive error handling
- Provide extensive test coverage
- Document API interfaces clearly

### Security Extensions

**Additional Validation**:
- Custom commitment types
- Extended authority mechanisms
- Message routing and filtering
- Advanced trust calculations

**Audit and Monitoring**:
- Message logging and analysis
- Security event tracking
- Performance monitoring
- State change auditing

This architecture provides a robust, secure, and performant foundation for AO process execution while maintaining developer experience and system reliability.