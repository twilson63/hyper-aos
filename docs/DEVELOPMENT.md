# Hyper-AOS Development Guide

## Development Environment Setup

### Prerequisites

Before starting development, ensure you have the following installed:

**Required Tools:**
- [Erlang/OTP](https://www.erlang.org/downloads) (version 24+ recommended)
- [Rebar3](https://rebar3.org/) - Erlang build tool
- [Git](https://git-scm.com/) - Version control
- [Node.js](https://nodejs.org/) (for ARX toolkit)
- [ARX](https://github.com/permaweb/arx) - Arweave transaction toolkit

**Optional Tools:**
- [VS Code](https://code.visualstudio.com/) with Lua and Erlang extensions
- [Lua Language Server](https://github.com/luals/lua-language-server)
- [ErlangLS](https://erlang-ls.github.io/) - Erlang language server

### Installation Steps

1. **Clone the repository:**
```bash
git clone https://github.com/permaweb/hyper-aos-demo.git
cd hyper-aos-demo
```

2. **Install ARX toolkit:**
```bash
npm install -g @permaweb/arx
```

3. **Set up Arweave wallet:**
```bash
export WALLET_PATH=/path/to/your/arweave-wallet.json
```

4. **Build the project:**
```bash
make all
```

5. **Run tests to verify setup:**
```bash
cd aos_test_suite
make eunit
```

## Code Structure and Conventions

### Project Layout

```
hyper-aos-demo/
├── src/                    # Source code
│   ├── aos.lua            # Core AO module
│   └── utils.lua          # Functional utilities
├── aos_test_suite/        # Test framework
│   ├── src/               # Erlang test modules
│   ├── test/              # Test data and helpers
│   └── rebar.config       # Test configuration
├── docs/                  # Documentation
├── demos/                 # Example implementations
├── scripts/               # Build and utility scripts
└── config/                # Configuration files
```

### Lua Code Conventions

#### aos.lua Structure

**Global State Management:**
```lua
-- Store all persistent state directly in _G
_G.id = "process_identifier"
_G.owner = "process_owner_address"
_G.authorities = {"addr1", "addr2", "addr3"}
_G.Inbox = {}

-- Private functions in local meta table
local meta = {
  initialized = false,
  init = function(msg) end,
  is_owner = function(msg) end,
  is_trusted = function(msg) end
}
```

**Function Naming:**
- Use snake_case for private functions (`meta.is_owner`)
- Use camelCase for public functions (`compute`, `eval`)
- Prefix boolean functions with `is_` or `has_`
- Use descriptive names that indicate purpose

**Error Handling:**
```lua
-- Always return structured responses
function compute(msg)
  local success, result = pcall(function()
    -- Core logic here
    return process_message(msg)
  end)
  
  if success then
    return {ok, _G}
  else
    return {error, result}
  end
end
```

#### utils.lua Structure

**Module Organization:**
```lua
-- Store in global namespace for AO compatibility
_G.utils = _G.utils or {}

-- Group related functions
_G.utils.map = function(fn, array) end
_G.utils.filter = function(predicate, array) end
_G.utils.reduce = function(fn, initial, array) end
```

**LUERL Optimizations:**
```lua
-- Use # operator for array length (faster than ipairs in LUERL)
local len = #array

-- Explicit nil checks for edge cases
if value == nil then
  return default_value
end

-- Type safety checks
if type(array) ~= "table" then
  error("Expected table, got " .. type(array))
end
```

### Erlang Test Conventions

#### Test Module Structure

```erlang
-module(aos_feature_test).
-include_lib("eunit/include/eunit.hrl").

% Test fixtures
setup() ->
    luerl:init().

cleanup(_State) ->
    ok.

% Test cases
feature_basic_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(State) ->
        [
            ?_test(test_basic_functionality(State)),
            ?_test(test_edge_cases(State)),
            ?_test(test_error_conditions(State))
        ]
    end}.

test_basic_functionality(State) ->
    {ok, Result} = luerl:eval("return 1 + 1", State),
    ?assertEqual([2.0], Result).
```

#### Assertion Patterns

```erlang
% Basic assertions
?assertEqual(Expected, Actual),
?assertNotEqual(NotExpected, Actual),
?assertTrue(BooleanExpression),
?assertFalse(BooleanExpression),

% Exception assertions  
?assertThrow(Pattern, Expression),
?assertError(Reason, Expression),
?assertExit(Reason, Expression),

% Pattern matching assertions
?assertMatch({ok, _}, Result),
?assertMatch([{<<"key">>, <<"value">>}], LuaTable)
```

## Building and Testing Locally

### Build System

The project uses a hybrid build system with Make and Rebar3:

**Makefile targets:**
```bash
make all        # Build all components
make clean      # Clean build artifacts  
make test       # Run all tests
make docs       # Generate documentation
make publish    # Publish to Arweave (requires wallet)
```

**Rebar3 commands:**
```bash
rebar3 compile   # Compile Erlang code
rebar3 eunit     # Run EUnit tests
rebar3 clean     # Clean build files
rebar3 shell     # Start Erlang shell with project loaded
```

### Testing Framework

#### Running Tests

**Full test suite:**
```bash
cd aos_test_suite
make eunit
```

**Specific test modules:**
```bash
rebar3 eunit -m aos_colors_test
rebar3 eunit -m aos_stringify_test  
rebar3 eunit -m aos_security_test
rebar3 eunit -m aos_utils_test
```

**Profile-based testing (for authority features):**
```bash
rebar3 as authorities_test eunit
```

**Verbose output:**
```bash
rebar3 eunit -v
```

#### Test Categories

**Security Tests (`aos_security_test.erl`):**
- Owner validation mechanisms
- Authority-based trust verification
- Commitment signature validation
- Message authentication flows

**Functionality Tests (`aos_colors_test.erl`, `aos_stringify_test.erl`):**
- Output formatting and colorization
- Table stringification with depth limiting
- ANSI escape code generation
- Terminal compatibility

**Utils Tests (`aos_utils_test.erl`):**
- Pattern matching functions
- Functional programming primitives
- Array and object operations
- LUERL compatibility edge cases

**Integration Tests (`integration/`):**
- Multi-step message processing
- State persistence across evaluations
- End-to-end security flows
- Performance benchmarks

#### Writing New Tests

**Test Template:**
```erlang
-module(new_feature_test).
-include_lib("eunit/include/eunit.hrl").

% Import helper functions
-include("../test/helpers/aos_test_helpers.hrl").

new_feature_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(State) ->
        [
            {"Basic functionality", ?_test(test_basic(State))},
            {"Edge cases", ?_test(test_edge_cases(State))},
            {"Error handling", ?_test(test_errors(State))}
        ]
    end}.

setup() ->
    % Load aos.lua and utils.lua
    {ok, State0} = luerl:init(),
    {ok, State1} = load_aos_module(State0),
    {ok, State2} = load_utils_module(State1),
    State2.

test_basic(State) ->
    % Test implementation
    Message = create_test_message(eval, "return 1 + 1"),
    {ok, [Result]} = luerl:call_function([aos, compute], [Message], State),
    ?assertMatch({<<"ok">>, _}, Result).
```

### Local Development Workflow

#### 1. Feature Development

```bash
# Create feature branch
git checkout -b feat/new-feature

# Make changes to src/aos.lua or src/utils.lua
vim src/aos.lua

# Test changes
cd aos_test_suite  
make eunit

# Build and validate
make all
```

#### 2. Testing Changes

```bash
# Run specific tests during development
rebar3 eunit -m aos_feature_test

# Test with different profiles
rebar3 as authorities_test eunit

# Run integration tests  
rebar3 eunit -m aos_integration_test
```

#### 3. Interactive Testing

```bash
# Start LUERL shell for interactive testing
rebar3 shell

% In Erlang shell:
1> {ok, State} = luerl:init().
2> {ok, S1} = luerl:dofile("src/aos.lua", State).
3> {ok, Result} = luerl:eval("return utils.map(function(x) return x*2 end, {1,2,3})", S1).
4> luerl:decode(Result, S1).
```

## Debugging Techniques

### Lua Debugging

#### Print-based Debugging

```lua
-- Enhanced print function for debugging
local function debug_print(label, value)
  print(label .. ": " .. tostring(value))
  if type(value) == "table" then
    for k, v in pairs(value) do
      print("  " .. tostring(k) .. " = " .. tostring(v))
    end
  end
end

-- Usage in aos.lua
debug_print("Incoming message", msg)
debug_print("Current state", _G)
```

#### State Inspection

```lua
-- Inspect global state
function inspect_state()
  local output = {}
  for key, value in pairs(_G) do
    if not string.match(key, "^_") then  -- Skip system keys
      table.insert(output, key .. " = " .. tostring(value))
    end
  end
  return table.concat(output, "\n")
end

-- Usage
eval("print(inspect_state())")
```

#### Error Tracing

```lua
-- Enhanced error handling with stack traces
function safe_eval(code)
  local func, err = load(code)
  if not func then
    return {error = "Syntax error: " .. err}
  end
  
  local success, result = pcall(func)
  if success then
    return {ok = result}
  else
    return {error = "Runtime error: " .. result}
  end
end
```

### Erlang/LUERL Debugging

#### Test Debugging

```erlang
% Add debug output to tests
test_debug_example(State) ->
    Message = create_test_message(eval, "return _G.owner"),
    io:format("Testing with message: ~p~n", [Message]),
    
    {ok, Result} = luerl:call_function([aos, compute], [Message], State),
    io:format("Got result: ~p~n", [Result]),
    
    ?assertMatch({<<"ok">>, _}, Result).
```

#### State Inspection

```erlang
% Helper to inspect LUERL state
inspect_lua_state(State) ->
    {ok, Globals} = luerl:call_function(['_G'], [], State),
    io:format("Global state: ~p~n", [Globals]).

% Check specific variables
check_global_var(VarName, State) ->
    {ok, [Value]} = luerl:get_table([list_to_binary(VarName)], State),
    io:format("~s = ~p~n", [VarName, Value]).
```

#### Memory and Performance Profiling

```erlang
% Profile test execution time
profile_test(TestFun, State) ->
    StartTime = erlang:monotonic_time(),
    Result = TestFun(State),
    EndTime = erlang:monotonic_time(),
    Duration = erlang:convert_time_unit(EndTime - StartTime, native, millisecond),
    io:format("Test completed in ~p ms~n", [Duration]),
    Result.
```

### Common Issues and Solutions

#### Issue: Tests failing with "undefined function"
**Solution:** Ensure modules are loaded correctly in test setup:
```erlang
setup() ->
    {ok, State0} = luerl:init(),
    {ok, State1} = luerl:dofile("../src/aos.lua", State0),
    {ok, State2} = luerl:dofile("../src/utils.lua", State1),
    State2.
```

#### Issue: LUERL type conversion errors
**Solution:** Use proper encoding/decoding:
```erlang
% Encoding Erlang data for Lua
LuaMessage = luerl:encode(Message, State),

% Decoding Lua result for Erlang
ErlangResult = luerl:decode(LuaResult, State).
```

#### Issue: Authority tests failing
**Solution:** Use the correct profile:
```bash
rebar3 as authorities_test eunit
```

#### Issue: State not persisting between calls
**Solution:** Ensure using `_G` namespace in Lua:
```lua
-- Wrong (local variable)
local user_data = "value"

-- Correct (global state)  
_G.user_data = "value"
```

## Contributing Code

### Git Workflow

#### Branch Naming Convention

```bash
feat/feature-name      # New features
fix/bug-description    # Bug fixes
docs/documentation     # Documentation updates
test/test-improvements # Test additions/improvements
refactor/code-cleanup  # Code refactoring
```

#### Commit Message Format

```
type(scope): brief description

Detailed explanation of changes if needed.

- List specific changes
- Include any breaking changes
- Reference issues: Fixes #123
```

**Examples:**
```
feat(security): add authority validation for messages

Implement meta.is_trusted() function to validate message senders
against the process authorities list. Includes comprehensive test
coverage and integration with existing security model.

- Add authority parsing from comma-separated string
- Implement trust verification logic
- Add 15 new test cases for authority validation
- Update security documentation

Fixes #45
```

### Code Review Process

#### Pre-submission Checklist

- [ ] All tests pass: `make eunit`
- [ ] Code follows established conventions
- [ ] New functionality has comprehensive tests
- [ ] Documentation updated for API changes
- [ ] Security implications considered and tested
- [ ] Performance impact evaluated
- [ ] LUERL compatibility verified

#### Pull Request Template

```markdown
## Summary
Brief description of changes made.

## Changes
- List of specific changes
- Include any breaking changes
- Note any new dependencies

## Testing
- How changes were tested
- New test cases added
- Performance impact assessment

## Documentation
- Documentation updates made
- API changes documented
- Examples updated if applicable

## Security
- Security implications considered
- Additional validation added
- Threat model impact assessed
```

### Code Quality Standards

#### Lua Code Quality

**Formatting:**
```lua
-- Consistent indentation (2 spaces)
if condition then
  local result = process_data()
  return result
end

-- Clear variable names
local message_validation_result = validate_message(msg)
local is_authorized_sender = check_authority(sender_address)

-- Function documentation
--- Validates message commitments against process owner
-- @param msg table The message to validate
-- @return boolean True if valid, false otherwise
function meta.is_owner(msg)
```

**Error Handling:**
```lua
-- Always handle edge cases
function safe_operation(input)
  if type(input) ~= "table" then
    error("Expected table input, got " .. type(input))
  end
  
  if #input == 0 then
    return {} -- Handle empty arrays gracefully
  end
  
  -- Process input...
end
```

#### Erlang Code Quality

**Test Organization:**
```erlang
% Group related tests
security_tests_() ->
    [
        {"Owner validation", fun test_owner_validation/0},
        {"Authority checking", fun test_authority_validation/0}, 
        {"Trust verification", fun test_trust_verification/0}
    ].

% Clear test names and descriptions
test_owner_validation() ->
    % Setup
    Message = create_owner_message(),
    State = setup_process_state(),
    
    % Execute
    {ok, Result} = call_aos_compute(Message, State),
    
    % Verify
    ?assertMatch({<<"ok">>, _}, Result).
```

## Performance Considerations

### LUERL Optimizations

#### Array Operations
```lua
-- Efficient: Use # operator
local len = #array
for i = 1, len do
  process(array[i])
end

-- Less efficient: Using pairs
for i, value in pairs(array) do
  process(value)
end
```

#### String Operations
```lua
-- Efficient: Use table.concat for multiple strings
local parts = {"Hello", " ", "world", "!"}
local result = table.concat(parts)

-- Less efficient: String concatenation in loops
local result = ""
for i, part in ipairs(parts) do
  result = result .. part -- Creates new string each time
end
```

#### Memory Management
```lua
-- Limit inbox size to prevent memory growth
if #_G.Inbox > _G.MAX_INBOX_SIZE then
  -- Remove oldest messages
  for i = 1, #_G.Inbox - _G.MAX_INBOX_SIZE do
    table.remove(_G.Inbox, 1)
  end
end
```

### Performance Testing

#### Benchmarking Tests
```erlang
benchmark_message_processing() ->
    NumMessages = 1000,
    Messages = [create_test_message() || _ <- lists:seq(1, NumMessages)],
    
    StartTime = erlang:monotonic_time(),
    
    lists:foreach(fun(Msg) ->
        {ok, _} = call_aos_compute(Msg, State)
    end, Messages),
    
    EndTime = erlang:monotonic_time(),
    Duration = erlang:convert_time_unit(EndTime - StartTime, native, millisecond),
    
    io:format("Processed ~p messages in ~p ms (~p msg/sec)~n", 
              [NumMessages, Duration, NumMessages * 1000 / Duration]).
```

### Memory Profiling

```erlang
% Monitor memory usage during tests
profile_memory_usage(TestFun) ->
    erlang:garbage_collect(),
    MemBefore = erlang:memory(total),
    
    TestFun(),
    
    erlang:garbage_collect(), 
    MemAfter = erlang:memory(total),
    
    io:format("Memory usage: ~p bytes (~p KB)~n", 
              [MemAfter - MemBefore, (MemAfter - MemBefore) div 1024]).
```

This development guide provides the foundation for contributing to Hyper-AOS with proper testing, debugging, and code quality practices.