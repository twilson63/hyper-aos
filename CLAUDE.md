# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a Hyper AOS (AO Operating System) demo project containing a Lua module script (`aos.lua`) designed to run on the Hyperbeam Lua device. The module implements a message processing environment within the AO protocol. AO is a protocol that uses message passing between processes in a decentralized supercomputer. aos is a set of core features that every ao process needs to provide the developer with the tools to create incredible processes that can do many things. This project consists of the aos (hyper-aos) lua script that runs on the lua device in hyperbeam. It is implemented using the luerl sandbox in the BEAM platform. The result is an extremly fast implementation. There are some tradeoffs, but for now, this is a very exiting project. This current implementation needs a lot of the features ported from legacy aos. And what we would like to do is port them over in a test driven approach.

Core state initial properties:

- id - when receiving the first message which MUST be type Process, we need to read the Commitments of the message and find the key that is not type of hmac and assign its value to the State.id variable.
- owner - when receiving the first message which MUST be type Process, we need to read the Commitments of the message and reference the committer and assign its value to State.owner.



Required validation checks:

- is_owner - this validation MUST compare the committer key all of the non hmac committment types of the message and one should match the State.owner
- dedupe validation - uses the hyperbeam dedupe device to make sure no duplicate messages are passed to the process
- is trusted msg - looks at the authorities State key and checks to see if the has a "from-process" key and is signed by one of the authorities in the State.authorities then it is trusted
- is assignable - some messages are provided as assignments, this message checks to see if the message that does not have the message id as the target

## Project Structure

```
hyper-aos-demo/
├── aos.lua                 # Core Lua module for AO compute environment
├── CLAUDE.md              # This file - project documentation
├── demo.json              # Demo configuration
└── aos_test_suite/        # Erlang/LUERL test suite
    ├── Makefile           # Build and test commands
    ├── README_TEST.md     # Test suite documentation
    ├── rebar.config       # Erlang build configuration
    ├── test_runner.sh     # Shell script test runner
    ├── src/               # Original test modules
    │   ├── aos_sandbox_test.erl    # Main manual test suite
    │   ├── aos_require_test.erl    # Require functionality tests
    │   ├── aos_math_test.erl       # Math operations tests
    │   ├── aos_outbox_test.erl     # Send/outbox tests
    │   ├── aos_test_suite_app.erl  # OTP application
    │   └── aos_test_suite_sup.erl  # OTP supervisor
    └── test/              # EUnit test modules
        ├── aos_test.erl             # Main EUnit test suite
        └── aos_math_eunit_test.erl  # Math EUnit tests
```

## Architecture

- **aos.lua** - Core Lua module that processes messages in the AO compute environment
- **aos_test_suite** - Unusual test suite using LUERL (Lua in Erlang) to sandbox and test aos.lua

## Build commands

### AOS Module
test command - `hype run test.lua`

publish command - `arx upload aos.lua -w demo.json -t arweave --content-type application/lua --tags Data-Protocol ao`

### Test Suite
```bash
cd aos_test_suite

# Run EUnit tests (recommended)
make eunit

# Run specific test types
make test-require    # Test require('.process')._version
make test-math      # Test math operations
make test-outbox    # Test send/outbox functionality

# Clean and rebuild
make clean
make all
```


## Security Features

### Commitment-Based Access Control
The `eval` function now includes RSA-PSS-512 commitment validation:
- Only RSA-PSS-512 commitments are valid for ownership verification
- Other commitment types (like hmac-sha256) are explicitly not accepted for ownership
- Messages must include a matching RSA-PSS-512 commitment with committer matching State.owner
- Security validation is implemented via a private `is_owner` function
- The private functions table is local to the module and cannot be accessed from eval'd code
- Backward compatible - if no State.owner is set, access is allowed

### Automatic Owner Initialization
- The process owner is automatically set from the first Process message
- When a message with `type = "process"` is received, the system finds the first non-hmac commitment and sets its committer as State.owner
- The State.initialized flag prevents the owner from being changed after initialization
- This ensures a process is bound to its initial creator

Example commitment structure:
```lua
commitment = {
  type = "RSA-PSS-512",
  commit = "commitment-hash",
  committer = "AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0"  -- 43-char address
}
```

## Critical Security Considerations

1. **Arbitrary Code Execution**: The `eval` function uses Lua's `load()` function to execute code from message inputs. This is now protected by commitment validation.

2. **Input Validation**: Messages with eval actions require matching RSA-PSS-512 commitments for authorization.

3. **Global State Exposure**: Direct manipulation of global `State` variable - consider additional access controls for other sensitive operations.

## Performance Considerations

1. **String Concatenation**: The `print` function uses inefficient string concatenation that could cause performance issues with large outputs.

## Core Functions

### Public Functions
- `compute(state, assignment)` - Main entry point for message processing
- `eval(msg)` - Evaluates Lua expressions (protected by commitment validation)
- `print(txt)` - Appends text to output buffer
- `send(msg)` - Adds messages to outbox
- `prompt()` - Returns formatted prompt string

### Private Functions (not accessible from eval or external code)
- `private.is_owner(state, msg)` - Validates RSA-PSS-512 commitments for security

## Message Processing Flow

1. Messages arrive with an optional `action` field
2. If action matches a global function name, it's executed
3. Otherwise, message is added to Inbox
4. Results are returned via `State.results`

## Safety Improvements Needed

- Implement sandboxed execution environment
- Add input validation and sanitization
- Set resource limits (memory, CPU time)
- Use protected calls (pcall) for error handling
- Implement proper access controls for state modifications:

## Test Suite Details

The aos_test_suite is an unusual testing approach that uses LUERL (Lua in Erlang) to create a sandboxed environment for testing the aos.lua module. Key features:

- **Sandboxed Execution**: Lua code runs inside Erlang's LUERL interpreter
- **Type Conversion**: Automatic conversion between Erlang maps and Lua tables
- **EUnit Integration**: Professional test framework with assertions and fixtures
- **Comprehensive Coverage**: Tests all major functions (compute, eval, send, print, require)

## References

### GitHub Repositories
- Hyperbeam: https://github.com/permaweb/hyperbeam
- AOS: https://github.com/permaweb/aos
- LUERL: https://github.com/rvirding/luerl

### Documentation
- Hyperbeam Documentation: https://hyperbeam.ar.io
- AO Cookbook: https://cookbook_ao.ar.io
