# Project Request Protocol: Fix EUnit simple_utils_integration_test

## Project Overview

**Project Name:** Fix EUnit simple_utils_integration_test  
**Date:** 2025-08-06  
**Priority:** High  
**Impact:** Test suite reliability and CI/CD pipeline

### Problem Statement
The `simple_utils_integration_test.erl` EUnit test is failing because it attempts to load `utils.lua` from the current working directory, but the file is located in `../src/utils.lua`. This path mismatch causes all test functions in the module to fail with file not found errors.

### Current State
- Test file location: `aos_test_suite/test/integration/simple_utils_integration_test.erl`
- Utils source location: `src/utils.lua` (relative to project root)
- Test expects: `utils.lua` in test execution directory
- Result: `{error,enoent}` when attempting to read file

### Root Cause Analysis
1. **Path Issue**: Line 26 uses `file:read_file("utils.lua")` without proper path resolution
2. **Working Directory**: Tests run from `aos_test_suite` directory, not project root
3. **Missing Path Configuration**: No mechanism to locate source files from test context

## Technical Requirements

### Core Issues to Fix
1. **File Path Resolution**
   - Update all `file:read_file()` calls to use correct relative paths
   - Consider using absolute paths or environment variables
   - Ensure paths work from both test suite and project root contexts

2. **Test Isolation**
   - Each test function independently loads utils.lua
   - Need consistent path resolution across all test functions

3. **Cross-Platform Compatibility**
   - Path separators must work on Unix/Linux/macOS
   - Consider using `filename:join/2` for proper path construction

### Affected Test Functions
1. `utils_basic_loading_test/0` - Line 26
2. `utils_basic_functions_test/0` - Line 42
3. `utils_pattern_matching_test/0` - Line 72
4. `utils_curry_test/0` - Line 103
5. `utils_compose_test/0` - Line 136
6. `utils_array_operations_test/0` - Line 165
7. `utils_object_operations_test/0` - Line 200
8. `utils_integration_with_aos_test/0` - Line 237

### Technical Constraints
- Must maintain existing test logic and assertions
- Cannot modify source file locations
- Should work with rebar3 test execution
- Must be compatible with different test profiles (unit, integration, security)

## Implementation Steps

### Phase 1: Immediate Fix (Critical Path)

1. **Update File Paths**
   ```erlang
   % Change from:
   {ok, UtilsCode} = file:read_file("utils.lua"),
   
   % To:
   {ok, UtilsCode} = file:read_file("../src/utils.lua"),
   ```

2. **Apply to All Test Functions**
   - Update all 8 test functions with correct path
   - Ensure consistency across the module

3. **Handle AOS Loading**
   - Line 238: Update `aos.lua` path similarly
   ```erlang
   % Change from:
   {ok, AosCode} = file:read_file("aos.lua"),
   
   % To:
   {ok, AosCode} = file:read_file("../src/aos.lua"),
   ```

### Phase 2: Robust Path Resolution

1. **Create Helper Function**
   ```erlang
   -define(SRC_DIR, "../src/").
   
   get_source_file(Filename) ->
       Path = filename:join(?SRC_DIR, Filename),
       file:read_file(Path).
   ```

2. **Update All Tests to Use Helper**
   ```erlang
   {ok, UtilsCode} = get_source_file("utils.lua"),
   ```

3. **Add Error Handling**
   ```erlang
   get_source_file(Filename) ->
       Path = filename:join(?SRC_DIR, Filename),
       case file:read_file(Path) of
           {ok, Content} -> {ok, Content};
           {error, enoent} ->
               % Try alternative paths
               AltPath = filename:join("src", Filename),
               file:read_file(AltPath)
       end.
   ```

### Phase 3: Long-term Improvements

1. **Environment-Based Configuration**
   ```erlang
   -ifdef(TEST).
   -define(SOURCE_PATH, "../src/").
   -else.
   -define(SOURCE_PATH, "src/").
   -endif.
   ```

2. **Test Setup Function**
   ```erlang
   setup() ->
       % Ensure working directory is correct
       {ok, Cwd} = file:get_cwd(),
       io:format("Test running from: ~s~n", [Cwd]),
       ok.
   
   teardown(_) ->
       ok.
   ```

3. **Integration with Test Helpers**
   - Move to use `aos_test_helpers:load_lua_file/1` if available
   - Centralize path resolution logic

## Success Criteria

### Immediate Success (Phase 1)
- [ ] All 8 test functions in simple_utils_integration_test pass
- [ ] File not found errors eliminated
- [ ] Tests run successfully with `rebar3 eunit -m simple_utils_integration_test`
- [ ] No regression in other test modules

### Complete Success (All Phases)
- [ ] Robust path resolution implemented
- [ ] Tests work from any execution context
- [ ] Clear error messages if files are missing
- [ ] Documentation updated with test execution requirements
- [ ] CI/CD pipeline runs without failures

## Testing Strategy

### Unit Testing
1. Test each function individually
2. Verify file loading succeeds
3. Confirm utils functionality works as expected

### Integration Testing
1. Run full test suite
2. Verify interaction with aos.lua
3. Test from different working directories

### Regression Testing
1. Ensure other test modules still pass
2. Verify no path issues introduced elsewhere
3. Check all test profiles (unit, integration, security)

## Risk Assessment

### Low Risk
- Simple path update to existing code
- No logic changes required
- Straightforward file path correction

### Medium Risk
- Different execution contexts may have different paths
- Rebar3 profiles might affect working directory
- Parallel test execution considerations

### Mitigation Strategies
- Test from multiple directories
- Use defensive path resolution with fallbacks
- Add logging for debugging path issues
- Review rebar3 configuration for test paths

## Implementation Timeline

**Immediate (10 minutes)**
- Update file paths in all test functions
- Test execution
- Verify fixes

**Short-term (30 minutes)**
- Implement helper function
- Add path resolution logic
- Update all tests to use helper

**Long-term (1 hour)**
- Add setup/teardown functions
- Implement environment-based configuration
- Create comprehensive documentation

## Code Changes Required

### File: `test/integration/simple_utils_integration_test.erl`

**Line 26** (and similar):
```erlang
% Before:
{ok, UtilsCode} = file:read_file("utils.lua"),

% After:
{ok, UtilsCode} = file:read_file("../src/utils.lua"),
```

**All affected lines:**
- Line 26: `utils_basic_loading_test/0`
- Line 42: `utils_basic_functions_test/0`
- Line 72: `utils_pattern_matching_test/0`
- Line 103: `utils_curry_test/0`
- Line 136: `utils_compose_test/0`
- Line 165: `utils_array_operations_test/0`
- Line 200: `utils_object_operations_test/0`
- Line 237-238: `utils_integration_with_aos_test/0`

### Optional Enhancement

Add at the top of the module:
```erlang
-define(UTILS_PATH, "../src/utils.lua").
-define(AOS_PATH, "../src/aos.lua").

% Then use throughout:
{ok, UtilsCode} = file:read_file(?UTILS_PATH),
```

## Dependencies

- Erlang/OTP 24+
- Rebar3
- EUnit framework
- LUERL library
- Source files in `src/` directory

## Success Metrics

1. **Test Pass Rate**: 100% of simple_utils_integration_test functions passing
2. **Execution Time**: No significant increase in test runtime
3. **Reliability**: No intermittent failures due to path issues
4. **Maintainability**: Clear path resolution strategy documented

## Validation Steps

1. **Run specific test module**
   ```bash
   cd aos_test_suite
   rebar3 eunit -m simple_utils_integration_test
   ```

2. **Run integration test suite**
   ```bash
   cd aos_test_suite
   ./run_tests.sh integration
   ```

3. **Run full test suite**
   ```bash
   make test
   ```

## Conclusion

This PRP addresses a straightforward but critical issue in the test suite where incorrect file paths prevent the simple_utils_integration_test from executing successfully. The fix involves updating file paths to correctly reference source files from the test execution context. The implementation is low-risk and should restore full test suite functionality.