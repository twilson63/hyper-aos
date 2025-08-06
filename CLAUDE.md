# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Hyper-AOS is a high-performance implementation of the AO (Arweave Operating System) protocol using Hyperbeam's native Lua device with LUERL sandboxing on the BEAM platform. The project provides a secure, colorized console for interacting with AO processes through message passing in a decentralized supercomputer.

## Architecture

The system consists of four main components:

1. **aos.lua** - Core Lua module implementing the AO compute environment with:
   - Commitment-based security (RSA-PSS-512 validation)
   - Authority-based trust verification
   - Message normalization and `from` field resolution
   - Colorized output and enhanced developer experience
   - Global state persistence across compute calls using `_G`
   - Integrated utils module for functional programming operations

2. **utils.lua** - Functional programming utilities module providing:
   - Pattern matching functions (matchesPattern, matchesSpec)
   - Functional primitives (curry, compose, reduce, map, filter)
   - Array operations (concat, reverse, find, includes)
   - Object operations (prop, propEq, keys, values)
   - LUERL-optimized implementations with type safety

3. **LUERL Sandbox** - Erlang-based Lua interpreter providing:
   - Secure execution environment
   - Type conversion between Erlang maps and Lua tables
   - Protection against malicious code execution

4. **Test Suite** - Comprehensive EUnit-based testing framework validating:
   - Security features (ownership, authorities, trust)
   - Message processing and state management
   - Color output and stringify functionality
   - Multi-step evaluation scenarios
   - Utils module functionality with 51+ test cases

## Critical Security Model

### Process Initialization
- **Owner**: Set from first non-HMAC commitment committer in Process message
- **ID**: Set from first non-HMAC commitment key in Process message
- **Authorities**: Parsed from comma-separated string (43-char addresses)

### Message Validation
1. **is_owner**: Validates RSA-PSS-512 commitments match `_G.owner`
2. **is_trusted**: Checks if `from == from-process` AND committer in authorities
3. **ensure_message**: Sets `from` field priority: existing > from-process > first committer

### State Management
- All persistent state stored directly in `_G` (not in State table)
- Private functions in local `meta` table (inaccessible from eval)
- System keys excluded from state serialization

## Development Commands

### Building and Testing
```bash
# Run Lua tests
hype run test.lua

# Run EUnit test suite (recommended)
cd aos_test_suite
make eunit

# Run specific test modules
rebar3 eunit -m aos_colors_test
rebar3 eunit -m aos_stringify_test
rebar3 eunit -m aos_authorities_test
rebar3 eunit -m aos_utils_test

# Run with authorities profile
rebar3 as authorities_test eunit

# Clean and rebuild
make clean && make all
```

### Publishing to Arweave
```bash
arx upload aos.lua -w demo.json -t arweave --content-type application/lua --tags Data-Protocol ao
```

### Launching AOS Console
```bash
aos console --module <TX_ID> --mainnet <HYPERBEAM_SERVER> <PROCESS_NAME>
```

## Code Conventions

### Lua Modules
**aos.lua:**
- State persisted in `_G` namespace
- Private functions in local `meta` table
- Binary string keys for message fields
- Return `{ok, State}` from compute function
- Utils module integrated in `_G.utils`

**utils.lua:**
- Functional programming utilities in `_G.utils`
- LUERL-optimized with type safety checks
- No side effects (immutable operations)
- Compatible with AO message processing patterns

### Erlang Tests
- Use EUnit framework with assertions
- Test helpers in `aos_test_helpers.erl`
- Profile-based conditional compilation for authorities tests
- Pattern: `aos_*_test.erl` for test modules

### Message Structure
```lua
message = {
  type = "process" | "message",
  action = "eval" | "send" | ...,
  commitments = {
    [key] = {
      type = "RSA-PSS-512" | "hmac-sha256",
      committer = "43-char-address",
      commit = "hash"
    }
  },
  ["from-process"] = "address",
  authority = "addr1,addr2,addr3"  -- Process messages only
}
```

## Required Validations

These validations MUST be implemented:

1. **Dedupe validation** - Use hyperbeam dedupe device to prevent duplicate messages
2. **Owner validation** - Compare non-HMAC commitment committers with `_G.owner`
3. **Trust validation** - Check authorities and from-process for trusted messages
4. **Assignable validation** - Check if message target differs from message ID

## Test Coverage Areas

- ✅ Security (owner/authorities/trust validation)
- ✅ Message processing and state persistence
- ✅ Color output and table stringify
- ✅ Multi-step evaluation scenarios
- ✅ Process initialization from commitments
- ✅ Message `from` field resolution
- ✅ Utils module functional programming operations (51+ tests)
- ✅ Pattern matching and message specifications
- ✅ Array and object operations with LUERL compatibility

## Performance Considerations

- String concatenation in print() function needs optimization for large outputs
- Consider buffer pooling for output management
- LUERL sandbox adds overhead but provides security
- Utils module optimized for LUERL with `#array` operator instead of `ipairs`
- Type safety checks add minimal overhead but prevent runtime errors
- Function composition and currying have LUERL-specific limitations

## Common Issues and Solutions

1. **Test failures with authorities**: Enable profile with `rebar3 as authorities_test eunit`
2. **State not persisting**: Ensure using `_G` namespace, not local variables
3. **Security validation failing**: Check commitment types (must be non-HMAC for ownership)
4. **Colors not showing**: Terminal must support ANSI escape codes
5. **Utils curry limitation**: LUERL has issues with partial application - use direct function calls
6. **Utils not loading**: Ensure utils.lua is in same directory as aos.lua
7. **Type conversion errors**: Check Erlang map to Lua table conversions with utils functions

## Agent Usage

The project includes specialized Claude agents in `.claude/agents/`:
- **erlang_developer**: For Erlang/OTP implementation following HyperBEAM conventions
- **luerl_lua_dev**: For Lua code targeting LUERL VM with aos patterns

Use these agents when implementing features in their respective domains.

## References

- Hyperbeam: https://github.com/permaweb/hyperbeam
- AOS: https://github.com/permaweb/aos
- LUERL: https://github.com/rvirding/luerl
- AO Cookbook: https://cookbook_ao.ar.io