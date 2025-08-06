# Project Request Protocol: AOS Utils.lua Port to LUERL

## Project Overview

### Title
Port AOS Utils Module to LUERL-Compatible Implementation

### Description
This project involves porting the utility functions from the AOS (Arweave Operating System) `utils.lua` module to ensure full compatibility with the LUERL Lua VM running on the BEAM platform. The ported module will provide essential functional programming utilities while maintaining security and performance characteristics required by the Hyper-AOS implementation.

### Objectives
1. Create a LUERL-compatible version of all utility functions from AOS utils.lua
2. Ensure all functions work correctly within the LUERL sandbox environment
3. Develop comprehensive EUnit tests for each utility function
4. Validate integration with existing aos.lua module
5. Maintain functional programming patterns and immutability principles

## Technical Requirements

### Source Module Analysis
The utils.lua module from AOS contains 16 primary functions providing functional programming utilities:

#### Core Pattern Matching
- `matchesPattern`: Flexible pattern matching with wildcard support
- `matchesSpec`: Message validation against specifications

#### Functional Programming Primitives
- `curry`: Function currying for partial application
- `compose`: Function composition (right-to-left)
- `reduce`: Array reduction with accumulator
- `map`: Array transformation
- `filter`: Array filtering with predicates

#### Array Operations
- `concat`: Array concatenation
- `reverse`: Array reversal
- `find`: First element matching condition
- `includes`: Element presence check
- `isArray` (internal): Array validation

#### Object Operations
- `prop`: Property accessor
- `propEq`: Property equality checker
- `keys`: Extract object keys
- `values`: Extract object values

### LUERL Compatibility Requirements

1. **Type Conversion**
   - Ensure proper handling of Erlang map to Lua table conversions
   - Maintain consistency with LUERL's table implementation
   - Handle binary strings correctly (as per aos.lua patterns)

2. **Sandbox Constraints**
   - No use of restricted Lua functions
   - Respect LUERL's security model
   - Avoid global namespace pollution

3. **Performance Considerations**
   - Optimize for LUERL's execution model
   - Minimize table allocations where possible
   - Use tail recursion where applicable

4. **Integration Requirements**
   - Must work seamlessly with existing aos.lua module
   - Follow established _G namespace conventions
   - Support binary string keys where necessary

## Implementation Steps

### Phase 1: Module Setup and Structure
1. Create `utils.lua` in project root
2. Set up module structure compatible with aos.lua loading pattern
3. Define module export table
4. Implement helper function `isArray`

### Phase 2: Core Functions Implementation
1. **Pattern Matching Functions**
   - Implement `matchesPattern` with wildcard support
   - Implement `matchesSpec` for message validation
   - Test with various pattern types (strings, functions, tables)

2. **Functional Primitives**
   - Implement `curry` with proper closure handling
   - Implement `compose` with right-to-left composition
   - Ensure proper function chaining

3. **Array Operations**
   - Implement `reduce` as foundation for other operations
   - Build `map`, `filter` using reduce
   - Implement `concat`, `reverse`, `find`, `includes`

4. **Object Operations**
   - Implement `prop`, `propEq` for property access
   - Implement `keys`, `values` for table introspection

### Phase 3: Test Suite Development
1. **Test Module Structure**
   - Create `aos_test_suite/test/aos_utils_test.erl`
   - Set up EUnit test framework
   - Create test helpers for Lua evaluation

2. **Individual Function Tests**
   - Pattern matching tests (edge cases, wildcards)
   - Functional programming tests (currying, composition)
   - Array operation tests (empty arrays, large datasets)
   - Object operation tests (nested properties, nil handling)

3. **Integration Tests**
   - Test utils functions with aos.lua message processing
   - Validate state persistence with utils operations
   - Test performance with large data structures

### Phase 4: Integration and Optimization
1. **Integration with aos.lua**
   - Add utils module loading in aos.lua
   - Update compute function to use utils where applicable
   - Ensure proper error handling

2. **Performance Optimization**
   - Profile function execution times
   - Optimize hot paths
   - Minimize memory allocations

3. **Documentation**
   - Add inline documentation for each function
   - Create usage examples
   - Update CLAUDE.md with utils.lua information

## Success Criteria

### Functional Requirements
- [ ] All 16 functions from original utils.lua are implemented
- [ ] Each function passes its individual test suite
- [ ] Functions maintain immutability (no side effects)
- [ ] Proper error handling for edge cases

### Compatibility Requirements
- [ ] All functions work correctly in LUERL sandbox
- [ ] Proper type conversion between Erlang and Lua
- [ ] No global namespace pollution
- [ ] Compatible with aos.lua message processing

### Testing Requirements
- [ ] 100% function coverage in EUnit tests
- [ ] Edge case testing for each function
- [ ] Integration tests with aos.lua
- [ ] Performance benchmarks documented

### Quality Requirements
- [ ] Code follows established Lua conventions
- [ ] Functions are properly documented
- [ ] Error messages are clear and actionable
- [ ] Module can be loaded independently

## Testing Strategy

### Unit Tests
Each function will have dedicated EUnit tests covering:
- Normal operation cases
- Edge cases (empty inputs, nil values)
- Error conditions
- Type coercion scenarios

### Integration Tests
- Message processing with utils functions
- State management with functional operations
- Chained operations (compose, curry combinations)
- Real-world usage patterns from AOS

### Performance Tests
- Benchmark each function with varying input sizes
- Memory usage profiling
- Comparison with native Lua implementations

## Risk Assessment

### Technical Risks
1. **LUERL Limitations**: Some Lua features may not be fully supported
   - Mitigation: Early testing and alternative implementations

2. **Performance Overhead**: Functional operations may be slower in LUERL
   - Mitigation: Optimization and selective usage

3. **Type Conversion Issues**: Erlang-Lua boundary may cause issues
   - Mitigation: Comprehensive type testing

### Integration Risks
1. **Breaking Changes**: May affect existing aos.lua functionality
   - Mitigation: Extensive integration testing

2. **State Management Conflicts**: Utils may interfere with _G namespace
   - Mitigation: Careful namespace management

## Deliverables

1. **utils.lua** - LUERL-compatible utility module
2. **aos_utils_test.erl** - Comprehensive EUnit test suite
3. **Integration updates** - Modified aos.lua with utils integration
4. **Documentation** - Updated CLAUDE.md and inline documentation
5. **Performance report** - Benchmarks and optimization notes

## Timeline Estimate

- Phase 1 (Setup): 2 hours
- Phase 2 (Implementation): 6 hours
- Phase 3 (Testing): 4 hours
- Phase 4 (Integration): 3 hours
- Documentation: 1 hour

**Total Estimated Time**: 16 hours

## Acceptance Criteria

The project will be considered complete when:
1. All utils.lua functions are successfully ported
2. EUnit test suite passes with 100% success rate
3. Integration with aos.lua is verified
4. Performance meets acceptable thresholds
5. Documentation is complete and accurate