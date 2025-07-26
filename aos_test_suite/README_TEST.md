# AOS Test Suite

This is an unusual test suite that uses LUERL (Lua in Erlang) to create a sandboxed environment for testing the aos.lua module.

## Architecture

- **test/aos_test.erl** - EUnit test module with proper assertions and test fixtures
- **test/aos_math_eunit_test.erl** - Focused EUnit tests for math operations
- **src/aos_sandbox_test.erl** - Original manual test module (kept for reference)

## Test Cases

1. **Basic message without action** - Tests default inbox behavior
2. **Message with eval action** - Tests code evaluation (e.g., `return 2 + 2`)
3. **Print function test** - Tests the custom print function
4. **Send function test** - Tests message dispatching
5. **Require process version test** - Tests `require('.process')._version` and verifies it returns "dev"
6. **Math operations test** - Tests `1 + 1` and verifies state.results.output.data equals "2", plus other math operations
7. **Outbox functionality test** - Tests `send({target='ID', data='hello'})` and verifies message appears in state.results.outbox

## Running Tests

### Using EUnit (Recommended)

```bash
# Run all EUnit tests
make eunit

# Run EUnit tests directly with rebar3
rebar3 eunit

# Run specific test module
rebar3 eunit --module=aos_test
```

### Using Original Test Suite

```bash
# Run old test suite
make test-old

# Run specific tests
make test-require
make test-math
make test-outbox

# Using test runner
./test_runner.sh
```

## Benefits of EUnit

- **Assertions**: `?assertEqual`, `?assertMatch` provide clear error messages
- **Test Fixtures**: Automatic setup/teardown for each test
- **Better Organization**: Tests are grouped and named clearly
- **Integration**: Works seamlessly with `rebar3 eunit`
- **Reporting**: Clear pass/fail status with detailed error information

## Dependencies

- Erlang/OTP
- rebar3
- LUERL 1.2.0 (automatically fetched)

## Key Features

- Sandboxed Lua execution environment
- Type conversion between Erlang and Lua data structures
- Isolated testing of the compute function
- Response evaluation and validation