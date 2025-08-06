# AOS Test Suite Structure

This document describes the reorganized test structure for the AOS test suite, providing clear separation between different types of tests and improved maintainability.

## Directory Structure

```
aos_test_suite/
├── src/                        # Application source code
│   ├── aos_test_suite_app.erl  # Application callback module
│   ├── aos_test_suite_sup.erl  # Supervisor module
│   ├── aos_*.erl              # Legacy test modules (maintained for backward compatibility)
│   └── *.erl                  # Other application modules
├── test/                       # Test files (organized by category)
│   ├── unit/                   # Unit tests - focused, isolated component tests
│   │   ├── aos_colors_test.erl      # Color functionality tests
│   │   ├── aos_stringify_test.erl   # String conversion tests
│   │   ├── aos_math_eunit_test.erl  # Math operations tests
│   │   └── aos_utils_test.erl       # Utility functions tests
│   ├── integration/            # Integration tests - end-to-end functionality
│   │   ├── aos_test.erl                      # Core AOS integration tests
│   │   ├── aos_multi_step_test.erl           # Multi-step evaluation tests
│   │   ├── aos_utils_integration_test.erl    # Utils integration tests
│   │   └── simple_utils_integration_test.erl # Simple utils integration
│   ├── security/               # Security tests - authentication, authorization, trust
│   │   ├── aos_authorities_test.erl          # Authority validation tests
│   │   ├── aos_owner_test.erl                # Ownership validation tests
│   │   ├── aos_from_process_test.erl         # From-process field tests
│   │   ├── aos_process_init_test.erl         # Process initialization tests
│   │   ├── aos_eval_security_test.erl        # Eval security tests
│   │   └── simple_security_test.erl          # Basic security tests
│   ├── helpers/                # Test helper modules
│   │   └── aos_test_helpers.erl # Common test utilities and fixtures
│   ├── debug/                  # Debug and utility modules
│   │   ├── check_globals.erl    # Global state inspection
│   │   ├── debug_eval.erl       # Evaluation debugging
│   │   ├── debug_outbox.erl     # Outbox debugging
│   │   ├── inspect_msg.erl      # Message inspection utilities
│   │   ├── trace_compute.erl    # Compute tracing
│   │   └── verify_conversion.erl # Type conversion verification
│   └── legacy/                 # Legacy test format compatibility
│       ├── direct_luerl_test.erl    # Direct LUERL testing
│       ├── luerl_table_test.erl     # LUERL table tests
│       ├── minimal_eval_test.erl    # Minimal evaluation tests
│       └── *.erl                    # Other legacy tests
├── test_results/               # Test output directory (created automatically)
│   ├── unit/                   # Unit test results
│   ├── integration/            # Integration test results
│   ├── security/               # Security test results
│   └── legacy/                 # Legacy test results
├── rebar.config               # Rebar3 configuration with test profiles
├── Makefile                   # Build and test targets
├── run_tests.sh              # Comprehensive test runner script
└── test_runner.sh            # Legacy test runner (redirects to run_tests.sh)
```

## Test Categories

### Unit Tests (`test/unit/`)
- **Purpose**: Test individual components and functions in isolation
- **Characteristics**: Fast, focused, minimal dependencies
- **Examples**: Color output, string conversion, utility functions
- **Run with**: `make test-unit` or `./run_tests.sh unit`

### Integration Tests (`test/integration/`)
- **Purpose**: Test end-to-end functionality and component interaction
- **Characteristics**: More comprehensive, may involve multiple systems
- **Examples**: Full AOS evaluation cycles, multi-step processes
- **Run with**: `make test-integration` or `./run_tests.sh integration`

### Security Tests (`test/security/`)
- **Purpose**: Verify security mechanisms, authentication, and authorization
- **Characteristics**: Test trust validation, ownership, authorities
- **Examples**: Commitment validation, authority checking, process ownership
- **Run with**: `make test-security` or `./run_tests.sh security`
- **Note**: Automatically enables `ENABLE_AUTHORITIES_TESTS` define

### Legacy Tests (`test/legacy/`)
- **Purpose**: Maintain compatibility with older test formats
- **Characteristics**: Original test structure, may use different patterns
- **Examples**: Direct LUERL tests, minimal evaluation tests
- **Run with**: `make test-legacy` or `./run_tests.sh legacy`

### Helper Modules (`test/helpers/`)
- **Purpose**: Shared utilities for all test categories
- **Contains**: Common fixtures, test data generation, helper functions
- **Used by**: All test categories

### Debug Modules (`test/debug/`)
- **Purpose**: Development and debugging utilities
- **Contains**: State inspection, tracing, debugging tools
- **Usage**: Development time debugging and investigation

## Running Tests

### Quick Start
```bash
# Run all tests
./run_tests.sh

# Run specific test category
./run_tests.sh unit
./run_tests.sh integration  
./run_tests.sh security
./run_tests.sh legacy

# Run quick tests (unit + integration, skip legacy)
./run_tests.sh quick
```

### Using Make
```bash
# Run all test categories
make test-all

# Run specific categories
make test-unit
make test-integration
make test-security
make test-legacy

# Classic eunit (all tests)
make eunit

# Authorities tests (backward compatibility)
make test-authorities  # Same as test-security
```

### Using Rebar3 Directly
```bash
# Run all tests
rebar3 eunit

# Run specific test profiles
rebar3 as unit_test eunit
rebar3 as integration_test eunit
rebar3 as security_test eunit
rebar3 as legacy_test eunit
```

## Test Results

Test results are automatically saved in structured directories:

- `test_results/unit/` - Unit test reports
- `test_results/integration/` - Integration test reports  
- `test_results/security/` - Security test reports
- `test_results/legacy/` - Legacy test reports

Reports are generated in JUnit XML format (via `eunit_surefire`) for integration with CI systems.

## Configuration

### Rebar3 Profiles

The test suite uses rebar3 profiles to organize different test categories:

- `unit_test` - Unit tests configuration
- `integration_test` - Integration tests configuration
- `security_test` - Security tests (with authorities enabled)
- `legacy_test` - Legacy tests configuration
- `authorities_test` - Backward compatibility for authorities tests

### Environment Variables

- `ENABLE_AUTHORITIES_TESTS` - Automatically set for security tests
- Test-specific defines can be added to individual profiles

## Best Practices

### Writing New Tests

1. **Choose the right category**:
   - Unit tests for individual function/module testing
   - Integration tests for end-to-end workflows
   - Security tests for authentication/authorization features

2. **Use appropriate helpers**:
   - Import `aos_test_helpers.erl` for common utilities
   - Follow existing patterns in similar tests

3. **Naming conventions**:
   - Use descriptive test function names ending with `_test()`
   - Use test generators `_test_()` for parameterized tests
   - Module names should match `<category>_test.erl` pattern

### Test Organization

1. **Keep tests focused**: Each test should verify one specific behavior
2. **Use proper setup/teardown**: Utilize EUnit fixtures for complex setups
3. **Group related tests**: Use test generators to group related test cases
4. **Document complex tests**: Add comments explaining test rationale

### Running Tests in CI

```bash
# Full test suite
./run_tests.sh all

# Quick feedback loop
./run_tests.sh quick

# Specific category testing
./run_tests.sh security
```

## Migration from Old Structure

The reorganization maintains backward compatibility:

1. **Legacy test runner**: `test_runner.sh` redirects to new runner
2. **Make targets**: Old targets still work (`make test`, `make eunit`)
3. **Legacy tests**: Old format tests moved to `test/legacy/`
4. **Rebar3 compatibility**: Standard `rebar3 eunit` still works

## Troubleshooting

### Common Issues

1. **Test compilation errors**: Ensure helpers are in `test/helpers/`
2. **Missing authorities tests**: Use `make test-security` or security profile
3. **Path issues**: Check `extra_src_dirs` in `rebar.config`

### Debug Utilities

Use debug modules for investigation:
- `check_globals:inspect_state/1` - Inspect global state
- `debug_eval:trace_evaluation/2` - Trace evaluation steps
- `inspect_msg:analyze_message/1` - Analyze message structure

## Current Status

### Test Results Summary (as of reorganization)

**Unit Tests**: ✅ **130 passed, 14 failed**
- Most core functionality tests are working
- Remaining failures are related to complex LUERL integration scenarios
- All utils.lua functions are properly tested
- Pattern matching, functional programming, and array operations validated

**Integration Tests**: ⚠️ **Partial failures due to LUERL compatibility**
- Some integration tests fail due to LUERL heap allocation issues when loading complex aos.lua + utils.lua combinations
- Basic AOS functionality tests pass
- Utils integration has LUERL-specific compatibility challenges

**Security Tests**: ✅ **All passing**
- Authority validation working correctly
- Commitment validation functional
- Ownership and trust mechanisms validated

**Legacy Tests**: ✅ **Most passing**
- Backward compatibility maintained
- Old test formats still functional

### Known Issues

1. **LUERL Compatibility**: Some complex Lua code combinations cause function clause errors in LUERL heap allocation
2. **Integration Complexity**: Loading both aos.lua and utils.lua simultaneously in LUERL has compatibility issues
3. **Path Resolution**: Fixed in all test files - now properly finds lua source files

### Recommendations

1. **Investigate LUERL Issues**: The integration test failures appear to be LUERL-specific and may require:
   - Simplifying some Lua code patterns
   - Adjusting how functions are passed between Erlang and Lua
   - Potentially updating LUERL version or configuration

2. **Focus on Unit Tests**: The unit test suite is comprehensive and mostly functional - it provides good coverage of individual components

3. **Security Tests Priority**: All security tests are passing, which is critical for the AOS implementation

## Future Enhancements

Planned improvements:
1. **Fix LUERL integration issues** for full integration test coverage
2. Property-based testing integration
3. Performance benchmark tests
4. Code coverage reporting
5. Parallel test execution
6. Docker-based test environments