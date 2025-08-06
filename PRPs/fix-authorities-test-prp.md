# Project Request Protocol: Fix EUnit aos_authorities_test

## Project Overview

The `aos_authorities_test` module in the Hyper-AOS test suite is failing with LUERL atom table corruption errors. All 7 test cases are failing during LUERL initialization, preventing the security tests from running properly. This PRP addresses fixing these critical test failures to ensure the authority-based trust validation system works correctly.

## Current State Analysis

### Error Pattern
All tests fail with the same error:
```
=ERROR REPORT==== 6-Aug-2025::16:25:41.720775 ===
beam/beam_load.c(150): Error loading module luerl:
  corrupt atom table
```

### Test Coverage
The module tests 7 critical security scenarios:
1. **authorities_parsing_test** - Validates parsing of comma-separated authorities from process messages
2. **authorities_with_spaces_test** - Tests authority parsing with spaces and invalid entries
3. **trusted_message_test** - Validates trusted message detection (authority in commitments)
4. **untrusted_from_mismatch_test** - Tests untrusted detection when from != from-process
5. **untrusted_no_authority_test** - Tests untrusted detection for non-authority committers
6. **no_from_process_test** - Tests messages without from-process field
7. **no_authorities_test** - Tests process messages without authorities field

### Root Cause
The atom table corruption suggests:
- LUERL module compilation issues
- Atom table overflow from previous test runs
- Build artifact corruption in `_build` directory
- Potential memory/atom leaks in test helpers

## Technical Requirements

### Primary Requirements
1. **Fix LUERL Initialization**
   - Resolve atom table corruption
   - Ensure clean LUERL state between tests
   - Implement proper cleanup mechanisms

2. **Test Isolation**
   - Each test must have isolated LUERL VM instance
   - No state persistence between test runs
   - Clean teardown after each test

3. **Build System Fixes**
   - Clean build artifacts properly
   - Ensure LUERL dependency compiles correctly
   - Fix rebar3 compilation warnings

### Secondary Requirements
1. **Test Helper Improvements**
   - Add LUERL state cleanup in `aos_test_helpers`
   - Implement atom table monitoring
   - Add error recovery mechanisms

2. **Profile Configuration**
   - Fix `authorities_test` profile configuration
   - Ensure ENABLE_AUTHORITIES_TESTS macro works correctly

## Implementation Steps

### Phase 1: Immediate Fixes (Critical)
1. **Clean Build Environment**
   ```bash
   rm -rf _build
   rm -rf .rebar3
   rebar3 clean --all
   ```

2. **Fix LUERL Initialization**
   - Check `aos_test_helpers:initialize_aos/0` for atom leaks
   - Add explicit LUERL cleanup between tests
   - Implement fresh LUERL instance per test

3. **Add Test Setup/Teardown**
   ```erlang
   setup() ->
       % Clean atom table
       erlang:garbage_collect(),
       % Initialize fresh LUERL
       aos_test_helpers:initialize_aos().
   
   teardown(LuaState) ->
       % Explicit cleanup
       luerl:gc(LuaState),
       ok.
   ```

### Phase 2: Test Isolation (High Priority)
1. **Implement Test Fixtures**
   ```erlang
   authorities_parsing_test_() ->
       {setup,
        fun setup/0,
        fun teardown/1,
        fun(State) -> 
            % Test implementation
        end}.
   ```

2. **Add State Reset Functions**
   - Clear global state between tests
   - Reset atom table usage
   - Monitor memory consumption

### Phase 3: Build System Improvements
1. **Update rebar.config**
   ```erlang
   {profiles, [
       {authorities_test, [
           {erl_opts, [
               {d, 'ENABLE_AUTHORITIES_TESTS'},
               {platform_define, "^2[0-9]", 'NEW_ATOM_TABLE'}
           ]}
       ]}
   ]}.
   ```

2. **Fix Compilation Warnings**
   - Address unused variable warnings
   - Fix pattern matching issues

### Phase 4: Verification
1. **Run Individual Tests**
   ```bash
   rebar3 as authorities_test eunit -m aos_authorities_test -t authorities_parsing_test
   ```

2. **Run Full Test Suite**
   ```bash
   rebar3 as authorities_test eunit
   ```

3. **Monitor Atom Table**
   ```erlang
   erlang:system_info(atom_count)
   ```

## Success Criteria

### Must Have
- [ ] All 7 authorities tests pass consistently
- [ ] No atom table corruption errors
- [ ] Tests run successfully with `rebar3 as authorities_test eunit`
- [ ] Clean build without critical warnings

### Should Have
- [ ] Tests are isolated and don't affect each other
- [ ] Memory/atom usage stays within limits
- [ ] Test execution time < 1 second per test
- [ ] Clear error messages on failure

### Nice to Have
- [ ] Automated atom table monitoring
- [ ] Performance benchmarks for authority validation
- [ ] Extended test coverage for edge cases

## Risk Mitigation

### Known Risks
1. **LUERL Version Incompatibility**
   - Mitigation: Pin to stable LUERL version 1.2.0
   - Fallback: Use older LUERL version if needed

2. **Test State Contamination**
   - Mitigation: Implement strict test isolation
   - Fallback: Run tests in separate OS processes

3. **Build System Complexity**
   - Mitigation: Document all build steps
   - Fallback: Provide manual test scripts

## Testing Strategy

### Unit Tests
- Test each authority validation function independently
- Verify LUERL state management
- Check atom table usage

### Integration Tests
- Test full message flow with authorities
- Verify trust validation in real scenarios
- Test multi-step evaluation with authorities

### Performance Tests
- Measure authority parsing speed
- Check memory consumption
- Monitor atom table growth

## Documentation Updates

### Code Documentation
- Document test isolation requirements
- Add comments explaining atom table management
- Update CLAUDE.md with test running instructions

### Test Documentation
- Create TEST_GUIDE.md for running authorities tests
- Document known issues and workarounds
- Add troubleshooting section

## Definition of Done

- [ ] All authorities tests pass without errors
- [ ] No atom table corruption messages
- [ ] Tests can run multiple times consecutively
- [ ] Build completes without critical warnings
- [ ] Documentation updated with fixes
- [ ] CI/CD pipeline updated if needed
- [ ] Code review completed
- [ ] Performance benchmarks documented