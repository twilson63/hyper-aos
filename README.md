# Hyper AOS Demo

This is a demonstration project for Hyper AOS (Arweave Operating System), implementing a secure Lua module that runs on the Hyperbeam Lua device within the AO protocol.

## Overview

The project consists of:
- **aos.lua** - Core Lua module implementing message processing with ownership-based security
- **aos_test_suite** - Comprehensive Erlang test suite using EUnit

## Security Features

- **Ownership-based access control**: The first non-HMAC commitment during process initialization sets the owner
- **Commitment validation**: All eval operations require valid commitments from the owner
- **Flexible commitment types**: Supports any commitment type (RSA-PSS-512, RSA-PSS-256, etc.) as long as the committer matches the owner

## Building and Testing

### Prerequisites

- Erlang/OTP 24 or later
- Rebar3

### Running Tests

```bash
cd aos_test_suite
rebar3 eunit
```

To run specific test modules:
```bash
rebar3 eunit -m aos_math_test
rebar3 eunit -m simple_security_test
```

## Project Structure

```
.
├── aos.lua                     # Main AOS Lua module
├── CLAUDE.md                   # AI assistant instructions
├── test.lua                    # Hyperbeam test runner
├── demo.json                   # Arweave wallet configuration
└── aos_test_suite/            # Test suite
    ├── src/                   # Test helper modules
    │   ├── aos_test_helpers.erl
    │   ├── aos_math_test.erl
    │   └── aos_security_test.erl
    └── test/                  # Test modules
        ├── aos_test.erl
        ├── aos_eval_security_test.erl
        ├── aos_owner_test.erl
        ├── aos_process_init_test.erl
        └── simple_security_test.erl
```

## Key Functions

### aos.lua

- `compute(state, assignment)` - Main entry point for message processing
- `eval(msg)` - Evaluates Lua expressions with security validation
- `meta.init(msg)` - Initializes owner from first Process message
- `meta.is_owner(msg)` - Validates message commitments against owner
- `send(msg)` - Sends messages to outbox
- `print(txt)` - Appends text to output buffer

## Publishing

To publish the module to Arweave:
```bash
arx upload aos.lua -w demo.json -t arweave --content-type application/lua --tags Data-Protocol ao
```

## Security Considerations

1. **Arbitrary Code Execution**: The `eval` function can execute arbitrary Lua code, but only when called by the process owner
2. **Commitment Validation**: Any commitment type is accepted as long as the committer matches the owner
3. **HMAC Filtering**: HMAC commitments are ignored during owner initialization but accepted for validation

## License

This project is part of the Arweave ecosystem. Please refer to the appropriate licenses.