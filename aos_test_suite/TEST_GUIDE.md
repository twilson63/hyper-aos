# AOS Test Suite Guide

## Overview

This guide provides comprehensive instructions for running and maintaining the AOS test suite, with special attention to the authorities test module that validates the security features of the AO protocol implementation.

## Test Suite Structure

```
aos_test_suite/
├── test/
│   ├── unit/           # Unit tests for individual functions
│   ├── integration/    # Integration tests for module interactions
│   ├── security/       # Security-focused tests (authorities, ownership)
│   ├── legacy/         # Legacy LUERL tests
│   └── helpers/        # Test helper utilities
├── rebar.config        # Build configuration with test profiles
└── Makefile           # Test automation commands
```

## Running Tests

### Quick Start

```bash
# Run all tests
make test

# Run specific test profiles
rebar3 as authorities_test eunit    # Authorities tests only
rebar3 as security_test eunit       # All security tests
rebar3 as unit_test eunit          # Unit tests only
rebar3 as integration_test eunit   # Integration tests only
```

### Running Individual Test Modules

```bash
# Run a specific test module
rebar3 eunit -m aos_authorities_test

# Run with authorities profile enabled
rebar3 as authorities_test eunit -m aos_authorities_test

# Run a specific test function
rebar3 eunit -m aos_authorities_test -t authorities_parsing_test
```

## Authorities Test Module

The `aos_authorities_test` module validates the authority-based trust validation system, a critical security feature of AOS.

### Test Coverage

1. **authorities_parsing_test** - Validates parsing of comma-separated authorities from process messages
2. **authorities_with_spaces_test** - Tests authority parsing with spaces and invalid entries  
3. **trusted_message_test** - Validates trusted message detection (authority in commitments)
4. **untrusted_from_mismatch_test** - Tests untrusted detection when from != from-process
5. **untrusted_no_authority_test** - Tests untrusted detection for non-authority committers
6. **no_from_process_test** - Tests messages without from-process field
7. **no_authorities_test** - Tests process messages without authorities field

### LUERL State Management

The authorities tests use isolated LUERL states to prevent atom table corruption:

- **Fresh State Per Test**: Each test gets a completely fresh LUERL instance
- **Atom Monitoring**: Tests track atom count to detect leaks
- **Binary Keys**: Uses binary strings instead of atoms to prevent accumulation
- **Proper Cleanup**: Explicit garbage collection between tests

## Test Profiles

### authorities_test
Enables the `ENABLE_AUTHORITIES_TESTS` macro for conditional compilation:
```bash
rebar3 as authorities_test eunit
```

### security_test
Runs all security-related tests including authorities, ownership, and trust validation:
```bash
rebar3 as security_test eunit
```

### unit_test
Runs unit tests for individual components:
```bash
rebar3 as unit_test eunit
```

### integration_test
Runs integration tests for module interactions:
```bash
rebar3 as integration_test eunit
```

## Troubleshooting

### Common Issues

#### 1. Atom Table Corruption
**Error**: `beam/beam_load.c(150): Error loading module luerl: corrupt atom table`

**Solution**: 
- Clean build artifacts: `rebar3 clean --all`
- Remove build directory: `rm -rf _build`
- Rebuild: `rebar3 compile`

#### 2. Test State Contamination
**Symptom**: Tests pass individually but fail when run together

**Solution**:
- Ensure tests use fresh LUERL states
- Check for global state modifications
- Use test fixtures with proper setup/teardown

#### 3. Compilation Warnings
**Warning**: `variable 'X' is unused`

**Solution**: 
- These are informational and don't affect test execution
- Can be suppressed with compiler flags if needed

### Debugging Tests

#### Enable Verbose Output
```bash
rebar3 eunit -v
```

#### Check Atom Table Usage
```erlang
%% In Erlang shell
erlang:system_info(atom_count).
erlang:system_info(atom_limit).
```

#### Run Tests in Isolation
```bash
# Run each test individually to identify issues
for test in authorities_parsing authorities_with_spaces trusted_message; do
    rebar3 as authorities_test eunit -m aos_authorities_test -t ${test}_test
done
```

## Test Helper Functions

The `aos_test_helpers` module provides utilities for test setup:

### Key Functions

- `initialize_fresh_aos/0` - Creates fresh LUERL state with AOS loaded
- `cleanup_lua_state/1` - Cleans up LUERL state and runs garbage collection
- `get_atom_count/0` - Returns current atom table count
- `ensure_binary_keys/1` - Converts map keys to binaries
- `call_compute/3` - Safely calls compute function with error handling

### Usage Example

```erlang
%% Test with fresh state
test_function() ->
    %% Setup
    State = aos_test_helpers:initialize_fresh_aos(),
    InitialAtoms = aos_test_helpers:get_atom_count(),
    
    %% Test logic
    {ok, Result} = aos_test_helpers:call_compute(State, Message, Assignment),
    
    %% Cleanup and verification
    aos_test_helpers:cleanup_lua_state(State),
    FinalAtoms = aos_test_helpers:get_atom_count(),
    ?assert(FinalAtoms - InitialAtoms < 100). % Check for atom leaks
```

## Best Practices

### 1. Test Isolation
- Always use fresh LUERL states for each test
- Don't rely on state from previous tests
- Clean up resources in teardown functions

### 2. Atom Management
- Monitor atom table growth in long-running tests
- Use binary strings for dynamic content
- Implement atom count assertions for leak detection

### 3. Error Handling
- Use try-catch blocks for LUERL evaluations
- Provide meaningful error messages
- Log debug information for failed assertions

### 4. Performance
- Keep individual tests under 1 second
- Use setup/teardown for expensive operations
- Consider parallel test execution for large suites

## Continuous Integration

### Running Tests in CI

```yaml
# Example GitHub Actions workflow
- name: Run Tests
  run: |
    cd aos_test_suite
    rebar3 clean --all
    rebar3 compile
    rebar3 as authorities_test eunit
    rebar3 as security_test eunit
```

### Test Reports

Test results are saved in `test_results/` directory:
- `test_results/unit/` - Unit test reports
- `test_results/integration/` - Integration test reports
- `test_results/security/` - Security test reports

## Extending Tests

### Adding New Authority Tests

1. Add test function to `aos_authorities_test.erl`
2. Use the fixture pattern for state isolation:

```erlang
new_authority_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun(State) -> 
         ?_test(begin
             % Test implementation
         end)
     end]}.
```

3. Enable with `ENABLE_AUTHORITIES_TESTS` macro if needed
4. Run with authorities profile: `rebar3 as authorities_test eunit`

## References

- [EUnit Documentation](http://erlang.org/doc/apps/eunit/chapter.html)
- [LUERL GitHub Repository](https://github.com/rvirding/luerl)
- [Rebar3 Documentation](https://rebar3.org/docs/)
- [AOS Protocol Specification](https://github.com/permaweb/aos)