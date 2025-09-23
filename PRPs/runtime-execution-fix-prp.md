# Project Request Protocol: Runtime Execution Test Fix

## Project Overview

**Project Name:** Runtime Execution Test Fix  
**Date:** 2025-08-06  
**Priority:** High  
**Impact:** Build and test pipeline reliability

### Problem Statement
The HyperAOS build system's runtime execution test is failing with a Lua runtime error. The test attempts to load and execute the concatenated `dist/aos.lua` module in a sandboxed environment but encounters an error: `attempt to index a nil value (global 'table')` at line 748.

### Current State
- 8 out of 9 tests passing
- Build process completes successfully
- Module concatenation works correctly
- Syntax validation passes
- LUERL compatibility patterns detected
- Runtime execution fails in sandboxed environment

## Technical Requirements

### Core Issue
The sandboxed test environment in `scripts/test.lua` (lines 228-241) is missing the global `table` library, which is required by the aos.lua module for string concatenation and array operations.

### Required Fixes
1. **Sandboxed Environment Completeness**
   - Add missing Lua standard library components to the test environment
   - Ensure all required globals are available for module execution

2. **Module Dependencies**
   - Identify all standard library dependencies used by aos.lua and utils.lua
   - Document required globals for future reference

3. **Test Robustness**
   - Improve error reporting in the runtime execution test
   - Add validation for sandbox environment completeness

### Technical Constraints
- Must maintain security isolation in the sandbox
- Cannot expose dangerous functions (io.write, os.execute, etc.)
- Must be compatible with LUERL VM execution model
- Should mimic the actual AO compute environment

## Implementation Steps

### Phase 1: Immediate Fix (Critical Path)
1. **Update Sandbox Environment**
   ```lua
   -- Add missing standard library components
   local env = {
       _G = {},
       -- String library
       string = string,
       -- Table library (MISSING - CRITICAL)
       table = table,
       -- Math library
       math = math,
       -- Other existing entries...
   }
   ```

2. **Add Safe Standard Libraries**
   - Include: `table`, `string`, `math`, `coroutine`, `utf8`
   - Exclude: Dangerous I/O operations, network access, file system writes

3. **Update Test Environment Initialization**
   - Ensure `env._G` contains reference to itself
   - Add `package` table with minimal structure for module loading

### Phase 2: Comprehensive Testing
1. **Verify Module Loading**
   - Test utils.lua module registration
   - Test aos.lua module registration
   - Verify inter-module dependencies

2. **Add Diagnostic Output**
   - Log which globals are accessed during execution
   - Report missing dependencies clearly
   - Add verbose mode for debugging

3. **Create Minimal Reproduction Case**
   - Extract the failing code segment
   - Test in isolation
   - Document the exact dependency chain

### Phase 3: Long-term Improvements
1. **Environment Documentation**
   - Create SANDBOX_REQUIREMENTS.md
   - List all required globals for AO modules
   - Document security considerations

2. **Test Suite Enhancement**
   - Add environment validation test
   - Create dependency detection utility
   - Implement progressive sandbox testing

3. **Build System Integration**
   - Add pre-flight checks for module dependencies
   - Generate dependency report during build
   - Validate against known LUERL limitations

## Success Criteria

### Immediate Success (Phase 1)
- [ ] Runtime execution test passes
- [ ] All 9 tests in test suite pass
- [ ] `make test` completes without errors
- [ ] No security vulnerabilities introduced

### Complete Success (All Phases)
- [ ] Comprehensive sandbox environment documented
- [ ] All standard library dependencies identified
- [ ] Test suite provides clear diagnostics for failures
- [ ] Build system validates module compatibility
- [ ] LUERL-specific requirements documented
- [ ] CI/CD pipeline runs successfully

## Testing Strategy

### Unit Testing
1. Test sandbox environment setup
2. Validate each standard library inclusion
3. Test module loading in isolation

### Integration Testing
1. Full build and test cycle
2. Cross-module dependency validation
3. LUERL compatibility verification

### Regression Testing
1. Ensure existing tests still pass
2. Verify no functionality broken
3. Check performance impact

## Risk Assessment

### Low Risk
- Adding standard table library (required, safe)
- Including string, math libraries (commonly needed)
- Improving error messages

### Medium Risk
- Module interdependencies not fully understood
- LUERL-specific behavior differences
- Sandbox escape possibilities

### Mitigation Strategies
- Incremental changes with testing between each
- Review LUERL documentation for gotchas
- Security audit of sandbox environment
- Peer review of changes

## Implementation Timeline

**Immediate (15 minutes)**
- Fix sandbox environment
- Verify test passes
- Commit fix

**Short-term (1 hour)**
- Add comprehensive standard libraries
- Improve error reporting
- Document requirements

**Long-term (2-4 hours)**
- Full test suite enhancement
- Documentation creation
- Build system integration

## Code Changes Required

### File: `scripts/test.lua`

```lua
-- Line 228-241: Update sandbox environment
local env = {
    _G = {},
    print = function() end,  -- Silent print
    io = { open = io.open, close = io.close },
    os = { date = os.date },
    
    -- ADD THESE CRITICAL LIBRARIES
    table = table,           -- Required for table operations
    string = string,         -- Required for string manipulation
    math = math,            -- May be required for calculations
    
    -- Existing safe functions
    pairs = pairs,
    ipairs = ipairs,
    next = next,            -- Add for table iteration
    select = select,        -- Add for varargs handling
    unpack = unpack or table.unpack,  -- Compatibility
    
    -- Type checking
    type = type,
    tostring = tostring,
    tonumber = tonumber,    -- Add for conversions
    
    -- Metatables
    setmetatable = setmetatable,
    getmetatable = getmetatable,
    rawget = rawget,        -- Add for raw access
    rawset = rawset,        -- Add for raw access
    rawequal = rawequal,    -- Add for raw comparison
    
    -- Error handling
    pcall = pcall,
    xpcall = xpcall,        -- Add for better error handling
    error = error,          -- Add for error generation
    assert = assert,        -- Add for assertions
    
    -- Module system
    require = function() end,  -- Stub for safety
    package = { 
        loaded = {} 
    },
    
    -- Loading
    load = load,
    loadstring = loadstring or load,  -- Compatibility
}
```

## Dependencies

- Lua 5.1+ or LuaJIT
- LUERL VM (for production)
- HyperBEAM framework
- Test runner (make, hype)

## Success Metrics

1. **Test Coverage**: 100% of tests passing
2. **Build Reliability**: No intermittent failures
3. **Documentation**: Clear requirements documented
4. **Performance**: No regression in build/test time
5. **Security**: No new vulnerabilities introduced

## Conclusion

This PRP addresses a critical test failure in the HyperAOS build pipeline. The root cause is an incomplete sandbox environment missing the `table` standard library. The fix is straightforward but requires careful consideration of security implications and LUERL compatibility. Implementation should proceed in phases, with immediate focus on fixing the failing test, followed by comprehensive improvements to the test infrastructure.