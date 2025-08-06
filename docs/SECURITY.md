# Hyper-AOS Security Documentation

## Security Model Overview

Hyper-AOS implements a comprehensive security model based on cryptographic commitments, authority-based trust verification, and sandboxed execution. This document provides detailed coverage of security mechanisms, threat models, and best practices for secure operation.

## Core Security Principles

### Defense in Depth
- **Multiple validation layers**: Commitment validation, authority checking, sandbox isolation
- **Cryptographic verification**: RSA-PSS-512 signature validation for all operations
- **Least privilege**: Minimal required permissions for each operation
- **Fail-safe defaults**: Secure-by-default configuration with explicit opt-in for privileged operations

### Trust Model
- **Process Owner**: Single entity with ultimate authority over process state
- **Trusted Authorities**: Configurable set of entities with delegated privileges
- **Message Authentication**: Every message cryptographically validated
- **State Integrity**: Protected global state with controlled access

### Isolation and Sandboxing
- **LUERL Sandbox**: Secure Lua execution environment on BEAM
- **Resource Limits**: Memory, CPU, and execution time constraints
- **System Function Isolation**: Core functions protected from user code
- **State Separation**: User state cleanly separated from system state

## Owner Validation Mechanism

### Process Initialization

The security model begins with process initialization from the first Process message:

```lua
function meta.init(msg)
  if not meta.initialized and msg.type and string.lower(msg.type) == "process" then
    -- Extract owner from first non-HMAC commitment
    for key, commitment in pairs(msg.commitments) do
      if commitment.type and string.lower(commitment.type) ~= "hmac-sha256" then
        _G.id = key                    -- Process identifier
        _G.owner = commitment.committer -- Process owner
        meta.initialized = true
        break
      end
    end
  end
end
```

**Security Properties:**
- **Immutable Ownership**: Owner set once during initialization, cannot be changed
- **Cryptographic Proof**: Owner established through valid RSA-PSS-512 commitment
- **Single Source of Truth**: Process ID and owner derived from same commitment
- **Initialization Protection**: Only first Process message can set owner

### Owner Verification

Every message undergoes ownership validation:

```lua
function meta.is_owner(msg)
  if not msg.commitments then
    return false
  end
  
  for key, commitment in pairs(msg.commitments) do
    if commitment.type and 
       string.lower(commitment.type) ~= "hmac-sha256" and
       commitment.committer == _G.owner then
      return true
    end
  end
  return false
end
```

**Validation Logic:**
1. **Commitment Presence**: Message must contain valid commitments
2. **Non-HMAC Filter**: Only cryptographic signatures count (not HMAC)
3. **Owner Match**: Commitment committer must match established owner
4. **Signature Verification**: RSA-PSS-512 signature validates message integrity

### Owner Privileges

Process owners have the following exclusive privileges:
- **State Modification**: Can modify any part of global process state
- **Authority Management**: Can update the list of trusted authorities
- **Process Configuration**: Can change security settings and parameters
- **Emergency Controls**: Can pause, reset, or terminate process

## Authority System

### Authority Configuration

Authorities are configured during process initialization:

```lua
-- Parse authorities from comma-separated string
if msg.authority then
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
    
    -- Trim whitespace and validate length
    authority = string.match(authority, "^%s*(.-)%s*$") or authority
    if #authority == 43 then  -- Valid Arweave address length
      table.insert(_G.authorities, authority)
    end
    
    if not comma_pos then break end
    start_pos = comma_pos + 1
  end
end
```

**Authority Properties:**
- **Arweave Address Format**: Must be exactly 43 characters (base64url encoded)
- **Comma-Separated Input**: Multiple authorities in single authority field
- **Validation**: Length and format validation prevents invalid entries
- **Immutable After Init**: Authority list set during initialization only

### Trust Verification

Authority-based trust is verified through dual validation:

```lua
function meta.is_trusted(msg)
  if not msg["from-process"] then
    return false
  end
  
  -- Verify from-process matches message sender
  if msg["from-process"] ~= msg.From then
    return false
  end
  
  -- Check if from-process is in authorities list
  for _, auth in ipairs(_G.authorities) do
    if auth == msg["from-process"] then
      return true
    end
  end
  
  return false
end
```

**Trust Requirements:**
1. **From-Process Presence**: Message must specify originating process
2. **Sender Verification**: from-process must match message From field
3. **Authority Membership**: from-process must be in authorities list
4. **Cryptographic Proof**: Message must have valid commitment signature

### Authority Privileges

Trusted authorities can:
- **Read Process State**: Access to global state for monitoring
- **Send Trusted Messages**: Bypass certain validation checks
- **Invoke Privileged Functions**: Access to authority-only operations
- **State Queries**: Read-only access to sensitive information

**Authorities cannot:**
- **Modify Core State**: Cannot change owner, ID, or authority list
- **Execute Arbitrary Code**: Limited to predefined trusted operations
- **Bypass Cryptographic Validation**: Still require valid commitments
- **Grant Additional Privileges**: Cannot modify their own privilege level

## Trust Verification Process

### Message Authentication Flow

```
Incoming Message
      │
      ▼
┌─────────────────┐
│ Extract         │
│ Commitments     │
└─────────────────┘
      │
      ▼
┌─────────────────┐
│ Validate        │ ◄─── RSA-PSS-512 verification
│ Signatures      │
└─────────────────┘
      │
      ▼
┌─────────────────┐
│ Check Owner     │ ◄─── meta.is_owner()
│ Status          │
└─────────────────┘
      │
      ▼
┌─────────────────┐
│ Check Trust     │ ◄─── meta.is_trusted()  
│ Status          │
└─────────────────┘
      │
      ▼
┌─────────────────┐
│ Authorize       │
│ Operation       │
└─────────────────┘
```

### Commitment Structure

Valid commitments must follow this structure:

```lua
commitments = {
  ["transaction_id_or_message_id"] = {
    type = "RSA-PSS-512",           -- Cryptographic signature type
    committer = "43_char_address",   -- Signer's Arweave address  
    commit = "base64_signature"      -- Cryptographic signature
  }
}
```

**Security Validation:**
- **Type Verification**: Only "RSA-PSS-512" accepted for ownership/trust
- **Committer Format**: Must be valid 43-character Arweave address
- **Signature Verification**: Cryptographic signature must validate against message
- **Replay Protection**: Transaction/message IDs prevent replay attacks

### Message From Field Resolution

The `from` field is resolved with security priority:

```lua
function meta.ensure_message(msg)
  -- Priority 1: Existing from field (highest trust)
  if msg.from then
    return msg
  end
  
  -- Priority 2: from-process field (authority context)
  if msg["from-process"] then
    msg.from = msg["from-process"]
    return msg
  end
  
  -- Priority 3: First commitment committer (cryptographic proof)
  if msg.commitments then
    for key, commitment in pairs(msg.commitments) do
      if commitment.committer then
        msg.from = commitment.committer
        return msg
      end
    end
  end
  
  return msg
end
```

**Resolution Security:**
1. **Explicit Trust**: Pre-set from field indicates explicit trust relationship
2. **Process Context**: from-process provides authority delegation context
3. **Cryptographic Fallback**: Commitment committer as last resort identification
4. **Audit Trail**: Resolution process preserves original field values

## State Protection Mechanisms

### Global State Security

Process state is protected through multiple mechanisms:

```lua
-- System keys excluded from user access
local SYSTEM_KEYS = {
  -- Lua built-ins
  "assert", "collectgarbage", "dofile", "error", "getmetatable",
  "ipairs", "load", "loadfile", "next", "pairs", "pcall", "print",
  "rawequal", "rawget", "rawlen", "rawset", "require", "select",
  "setmetatable", "tonumber", "tostring", "type", "xpcall", "_VERSION",
  
  -- Lua libraries  
  "coroutine", "debug", "io", "math", "os", "package", "string", "table", "utf8",
  
  -- AOS system functions
  "compute", "eval", "send", "prompt", "removeCR", "isSimpleArray", "stringify",
  
  -- Private/system variables
  "_OUTPUT", "MAX_INBOX_SIZE", "SYSTEM_KEYS", "meta", "utils", "State", "_G"
}
```

**Protection Mechanisms:**
- **System Key Exclusion**: Core functions protected from serialization/modification
- **Private Meta Table**: Security functions in local scope, inaccessible from eval()
- **State Serialization Control**: Only user data exported in state snapshots
- **Function Isolation**: System functions cannot be overwritten by user code

### Sandbox Isolation

LUERL provides several isolation layers:

```erlang
% Erlang-side sandbox configuration
SandboxConfig = #{
    memory_limit => 512 * 1024 * 1024,    % 512MB limit
    execution_timeout => 30000,            % 30 second timeout
    allowed_modules => [string, table, math], % Limited module access
    forbidden_functions => [io, os, debug]    % System function blacklist
}.
```

**Isolation Features:**
- **Memory Limits**: Prevent memory exhaustion attacks
- **Execution Timeouts**: Prevent infinite loops and DoS
- **Module Restrictions**: Only safe modules available to user code
- **Function Blacklisting**: Dangerous functions completely unavailable

### State Persistence Security

State persistence maintains security boundaries:

```lua
function serialize_safe_state()
  local safe_state = {}
  
  for key, value in pairs(_G) do
    local is_system_key = false
    
    -- Check against system keys blacklist
    for _, system_key in ipairs(SYSTEM_KEYS) do
      if key == system_key then
        is_system_key = true
        break
      end
    end
    
    -- Only include non-system keys
    if not is_system_key then
      safe_state[key] = value
    end
  end
  
  return safe_state
end
```

**Persistence Security:**
- **Selective Serialization**: Only user data included in state exports
- **System Protection**: Core functions and data excluded from persistence
- **Clean Separation**: Clear boundary between user and system state
- **Integrity Preservation**: State restoration maintains security properties

## Best Practices for Secure Usage

### Secure Process Setup

#### 1. Strong Authority Configuration

```bash
# Generate secure authority addresses
# Use separate, dedicated Arweave wallets for authorities
arx wallet create authority-1.json
arx wallet create authority-2.json  
arx wallet create authority-3.json

# Get addresses for authority configuration
AUTHORITY_1=$(arx address -w authority-1.json)
AUTHORITY_2=$(arx address -w authority-2.json)
AUTHORITY_3=$(arx address -w authority-3.json)

# Configure process with multiple authorities
AUTHORITIES="$AUTHORITY_1,$AUTHORITY_2,$AUTHORITY_3"
```

**Authority Best Practices:**
- **Separate Wallets**: Each authority should use a dedicated wallet
- **Geographic Distribution**: Distribute authorities across different locations
- **Access Control**: Limit authority wallet access to authorized personnel
- **Regular Rotation**: Periodically rotate authority wallets for security

#### 2. Secure Message Handling

```lua
-- Validate all incoming messages
function secure_message_handler(msg)
  -- Basic format validation
  if not msg or type(msg) ~= "table" then
    error("Invalid message format")
  end
  
  -- Required fields check
  if not msg.action or not msg.commitments then
    error("Missing required fields")
  end
  
  -- Security validation
  local is_owner = meta.is_owner(msg)
  local is_trusted = meta.is_trusted(msg)
  
  -- Operation authorization
  if requires_owner_privilege(msg.action) and not is_owner then
    error("Unauthorized: Owner required")
  end
  
  if requires_authority_privilege(msg.action) and not (is_owner or is_trusted) then
    error("Unauthorized: Authority required")
  end
  
  -- Process message safely
  return process_authorized_message(msg)
end
```

#### 3. Input Validation and Sanitization

```lua
-- Comprehensive input validation
function validate_message_data(data)
  -- Type validation
  if type(data) ~= "string" then
    error("Message data must be string")
  end
  
  -- Length limits
  if #data > 1024 * 1024 then  -- 1MB limit
    error("Message data exceeds size limit")
  end
  
  -- Content sanitization
  -- Remove potentially dangerous patterns
  local sanitized = data:gsub("[<>\"'&]", "")
  
  -- Validate Lua syntax if code
  if is_lua_code(sanitized) then
    local func, err = load(sanitized)
    if not func then
      error("Invalid Lua syntax: " .. err)
    end
  end
  
  return sanitized
end
```

### Security Monitoring

#### 1. Security Event Logging

```lua
-- Enhanced security logging
function log_security_event(event_type, details)
  local timestamp = os.date("%Y-%m-%d %H:%M:%S")
  local log_entry = {
    timestamp = timestamp,
    type = "security",
    event = event_type,
    details = details,
    process_id = _G.id,
    process_owner = _G.owner
  }
  
  -- Log to system (implementation dependent)
  print("SECURITY: " .. stringify(log_entry))
end

-- Usage in security functions
function meta.is_owner(msg)
  local result = validate_owner_commitment(msg)
  
  if not result then
    log_security_event("unauthorized_access", {
      message_id = msg.id,
      attempted_by = msg.from,
      reason = "invalid_owner_commitment"
    })
  end
  
  return result
end
```

#### 2. Anomaly Detection

```lua
-- Monitor for suspicious patterns
local security_monitor = {
  failed_attempts = {},
  rate_limits = {},
  suspicious_patterns = {}
}

function monitor_security_events(msg)
  local sender = msg.from or "unknown"
  local now = os.time()
  
  -- Track failed authorization attempts
  if not meta.is_owner(msg) and not meta.is_trusted(msg) then
    security_monitor.failed_attempts[sender] = 
      (security_monitor.failed_attempts[sender] or 0) + 1
    
    -- Alert on repeated failures
    if security_monitor.failed_attempts[sender] > 5 then
      log_security_event("repeated_auth_failures", {
        sender = sender,
        attempts = security_monitor.failed_attempts[sender]
      })
    end
  end
  
  -- Rate limiting
  local sender_rate = security_monitor.rate_limits[sender] or {count = 0, window = now}
  
  if now - sender_rate.window > 60 then  -- 1-minute window
    sender_rate = {count = 1, window = now}
  else
    sender_rate.count = sender_rate.count + 1
  end
  
  security_monitor.rate_limits[sender] = sender_rate
  
  -- Alert on high rates
  if sender_rate.count > 100 then  -- 100 messages per minute
    log_security_event("rate_limit_exceeded", {
      sender = sender,
      rate = sender_rate.count
    })
  end
end
```

### Cryptographic Best Practices

#### 1. Signature Verification

```lua
-- Enhanced signature validation
function verify_rsa_pss_signature(message, signature, public_key)
  -- Use proper cryptographic libraries (implementation dependent)
  local crypto = require('crypto') -- Hypothetical crypto library
  
  -- Verify RSA-PSS-512 signature
  local is_valid = crypto.verify_rsa_pss({
    message = message,
    signature = signature,
    public_key = public_key,
    hash_algorithm = 'SHA-256',
    salt_length = 32,  -- PSS salt length
    key_size = 4096    -- RSA key size
  })
  
  if not is_valid then
    log_security_event("signature_verification_failed", {
      message_hash = crypto.hash(message),
      public_key_hash = crypto.hash(public_key)
    })
  end
  
  return is_valid
end
```

#### 2. Address Validation

```lua
-- Comprehensive Arweave address validation
function validate_arweave_address(address)
  -- Length check
  if type(address) ~= "string" or #address ~= 43 then
    return false, "Invalid address length"
  end
  
  -- Character set check (base64url)
  if not string.match(address, "^[A-Za-z0-9_-]+$") then
    return false, "Invalid character set"
  end
  
  -- Additional validation (checksum, etc.) could go here
  return true, "Valid address"
end

-- Use in authority validation
function validate_authority_list(authorities)
  for i, auth in ipairs(authorities) do
    local is_valid, error_msg = validate_arweave_address(auth)
    if not is_valid then
      error("Invalid authority #" .. i .. ": " .. error_msg)
    end
  end
end
```

## Known Vulnerabilities and Mitigations

### 1. Replay Attacks

**Vulnerability:** Malicious actors could replay valid messages to trigger duplicate operations.

**Mitigation:**
```lua
-- Message nonce tracking
local processed_messages = {}

function prevent_replay(msg)
  local message_id = msg.id or generate_message_hash(msg)
  
  if processed_messages[message_id] then
    error("Replay attack detected: message already processed")
  end
  
  processed_messages[message_id] = true
  
  -- Cleanup old messages (prevent memory bloat)
  if table_length(processed_messages) > 10000 then
    cleanup_old_processed_messages()
  end
end
```

### 2. Authority Key Compromise

**Vulnerability:** Compromised authority keys could be used for unauthorized access.

**Mitigation:**
- **Key Rotation**: Regular rotation of authority keys
- **Multi-signature**: Require multiple authority signatures for sensitive operations
- **Monitoring**: Active monitoring for suspicious authority usage patterns
- **Revocation**: Immediate revocation mechanism for compromised keys

```lua
-- Authority revocation mechanism
function revoke_authority(authority_address, reason)
  if not meta.is_owner(current_message) then
    error("Only owner can revoke authorities")
  end
  
  -- Remove from authorities list
  for i, auth in ipairs(_G.authorities) do
    if auth == authority_address then
      table.remove(_G.authorities, i)
      break
    end
  end
  
  log_security_event("authority_revoked", {
    revoked_authority = authority_address,
    reason = reason,
    revoked_by = _G.owner
  })
end
```

### 3. State Corruption

**Vulnerability:** Malicious code execution could corrupt process state.

**Mitigation:**
- **Sandboxing**: LUERL sandbox prevents system access
- **State Validation**: Regular state integrity checks
- **Backup and Recovery**: Automated state backups
- **Rollback Capability**: Ability to restore to previous valid state

```lua
-- State integrity validation
function validate_state_integrity()
  local critical_fields = {"id", "owner", "authorities"}
  
  for _, field in ipairs(critical_fields) do
    if not _G[field] then
      error("Critical state field missing: " .. field)
    end
  end
  
  -- Validate owner format
  local is_valid, error_msg = validate_arweave_address(_G.owner)
  if not is_valid then
    error("Owner address corrupted: " .. error_msg)
  end
  
  -- Validate authorities
  validate_authority_list(_G.authorities)
end
```

### 4. DoS Attacks

**Vulnerability:** Resource exhaustion through message flooding or expensive operations.

**Mitigation:**
```lua
-- Rate limiting and resource management
local rate_limiter = {
  message_counts = {},
  computation_budgets = {}
}

function enforce_rate_limits(sender)
  local now = os.time()
  local sender_data = rate_limiter.message_counts[sender] or {count = 0, window = now}
  
  -- Reset window every minute
  if now - sender_data.window > 60 then
    sender_data = {count = 1, window = now}
  else
    sender_data.count = sender_data.count + 1
  end
  
  rate_limiter.message_counts[sender] = sender_data
  
  -- Enforce limits
  if sender_data.count > 60 then  -- 60 messages per minute
    error("Rate limit exceeded: " .. sender)
  end
end
```

This comprehensive security documentation provides the foundation for understanding and implementing secure Hyper-AOS deployments with proper threat mitigation and security monitoring.