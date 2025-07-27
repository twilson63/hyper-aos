# AOS Metatable Test Module

## Overview
The `aos_metatable_test.erl` module provides comprehensive EUnit tests for the AOS metatable functionality that enables the `_G['processId'].property.path` syntax.

## Test Coverage

### 1. Process ID Detection (`test_process_id_detection`)
- Validates that 43-character base64url strings are recognized as process IDs
- Tests invalid formats (too short, too long, invalid characters)

### 2. Mock ao.resolve Function (`test_ao_resolve_mock`)
- Verifies the mock `ao.resolve` function works correctly
- In production, this would make actual resolve calls to retrieve process data

### 3. Terminal Property Access (`test_terminal_property_access`)
- Tests that terminal properties (supply, balance, owner, etc.) trigger immediate resolution
- Verifies correct path construction for terminal properties

### 4. Nested Property Chains (`test_nested_property_chain`)
- Tests building of nested paths like `processId.now.supply`
- Verifies deep nesting like `processId.token.info.name`

### 5. Error Handling (`test_proxy_error_handling`)
- Ensures that attempts to set properties on proxies fail with appropriate errors
- Validates the read-only nature of process proxies

### 6. Force Resolution (`test_proxy_force_resolution`)
- Tests the ability to force resolution by calling a proxy as a function
- Example: `_G['processId'].process()`

### 7. Multiple Process IDs (`test_multiple_process_ids`)
- Verifies that multiple process IDs can be accessed independently
- Each maintains its own proxy chain

### 8. Integration with Compute (`test_integration_with_compute`)
- Tests that metatable functionality works within the aos compute context
- Verifies eval actions can use the proxy syntax

### 9. Proxy Caching (`test_proxy_cache_behavior`)
- Tests that repeated access to the same path returns cached proxies
- Verifies performance optimization through caching

### 10. Invalid Process ID Handling (`test_invalid_process_id`)
- Ensures non-process-ID keys return nil instead of proxies
- Maintains backward compatibility with regular global access

## Running the Tests

### Run all metatable tests:
```bash
cd aos_test_suite
./test_metatable.sh
```

### Run with other tests:
```bash
cd aos_test_suite
make eunit
```

### Run specific test:
```bash
rebar3 eunit --module=aos_metatable_test --test=test_nested_property_chain
```

## Implementation Details

The test module:
1. Loads the metatable integration code into the Lua state
2. Sets up the global metatable to intercept process ID access
3. Provides a mock `ao.resolve` function that returns "Resolved: {path}"
4. Tests all aspects of the proxy behavior

## Key Components

### Terminal Properties
These properties trigger immediate resolution:
- Token: supply, balance, owner, name, ticker, denomination, logo
- Process: state, inbox, spawns, errors
- Data: value, data, result, output

### Nested Properties
These return another proxy for chaining:
- now, info, balances, state, process, token

### Process ID Format
- Must be exactly 43 characters
- Contains only base64url characters: [A-Za-z0-9_-]
- Example: `AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0`

## Expected Behavior

When accessing `_G['processId'].now.supply`:
1. Global metatable detects 'processId' is a valid process ID
2. Returns a proxy object for 'processId'
3. Accessing '.now' returns another proxy for 'processId/now'
4. Accessing '.supply' (terminal property) calls `ao.resolve('processId/now/supply')`
5. Returns the resolved value

## Future Enhancements

1. Add real ao.resolve integration tests when available
2. Test garbage collection of proxy cache
3. Add performance benchmarks for proxy creation
4. Test concurrent access patterns