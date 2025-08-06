# Hyper-AOS Examples

This directory contains comprehensive example applications demonstrating the capabilities and patterns of the Hyper-AOS system. Each example is fully functional and showcases different aspects of AO message processing, state management, and decentralized application development.

## Directory Structure

```
examples/
├── basic/                  # Beginner-friendly examples
│   ├── hello_world.lua    # Simple message handling
│   └── state_management.lua # State persistence patterns
├── advanced/              # Complex examples
│   ├── authority_validation.lua # Security and trust verification
│   └── message_routing.lua     # Advanced message processing
├── utils/                 # Functional programming examples
│   └── functional_programming.lua # Utils module usage
├── integration/           # External system interaction
│   └── arweave_interaction.lua    # Arweave and AO integration
└── README.md             # This file
```

## Getting Started

### Prerequisites

- Hyper-AOS development environment set up
- Basic understanding of Lua programming
- Familiarity with AO message passing concepts
- Access to an AO process for deployment

### Running Examples

1. **Deploy to AOS Process**: Copy any example script to your AOS process
2. **Send Messages**: Use the documented message patterns to interact
3. **View Responses**: Monitor the process console for output and responses

## Examples Overview

### Basic Examples

#### 1. Hello World (`basic/hello_world.lua`)

**Purpose**: Demonstrates the fundamentals of AO message handling.

**Key Features**:
- Simple greeting responses
- State persistence (greeting counter)
- Basic message pattern matching
- Error handling for unknown actions

**Usage**:
```lua
-- Send a greeting
Send({
  Target = "<PROCESS_ID>", 
  Action = "Hello"
})

-- Check process status
Send({
  Target = "<PROCESS_ID>",
  Action = "Status"
})
```

**Learning Outcomes**:
- Understand basic message handling patterns
- Learn state persistence in `_G` namespace
- See response message generation
- Practice error handling

#### 2. State Management (`basic/state_management.lua`)

**Purpose**: Comprehensive demonstration of state persistence and management.

**Key Features**:
- Key-value store with versioning
- Operation history tracking
- Statistics and metadata
- Input validation and error handling

**Usage**:
```lua
-- Store a value
Send({
  Target = "<PROCESS_ID>",
  Action = "Set",
  Key = "user_name", 
  Value = "Alice"
})

-- Retrieve a value
Send({
  Target = "<PROCESS_ID>",
  Action = "Get",
  Key = "user_name"
})

-- View statistics
Send({
  Target = "<PROCESS_ID>",
  Action = "Stats"
})
```

**Learning Outcomes**:
- Master state persistence patterns
- Implement operation history
- Handle concurrent state modifications
- Build robust data validation

### Advanced Examples

#### 3. Authority Validation (`advanced/authority_validation.lua`)

**Purpose**: Demonstrates security and trust verification using AO's commitment system.

**Key Features**:
- RSA-PSS-512 commitment validation
- Role-based access control (Owner/Authority/Public)
- Secure document management
- Access logging and auditing

**Usage**:
```lua
-- Create document (requires authority)
Send({
  Target = "<PROCESS_ID>",
  Action = "CreateDocument",
  DocId = "doc1",
  Content = "Sensitive information"
})

-- Read document (permissions checked)
Send({
  Target = "<PROCESS_ID>",
  Action = "ReadDocument", 
  DocId = "doc1"
})

-- View security info (owner only)
Send({
  Target = "<PROCESS_ID>",
  Action = "SecurityInfo"
})
```

**Learning Outcomes**:
- Implement commitment-based security
- Build role-based access systems
- Create audit trails
- Handle security edge cases

#### 4. Message Routing (`advanced/message_routing.lua`)

**Purpose**: Advanced message processing with routing, forwarding, and inter-process communication.

**Key Features**:
- Pattern-based message routing
- Message transformation and forwarding
- Multi-destination broadcasting
- Request-response coordination
- Process registry and discovery

**Usage**:
```lua
-- Add routing rule
Send({
  Target = "<ROUTER_PROCESS_ID>",
  Action = "AddRoute",
  RouteId = "log_route",
  Pattern = "action=Log",
  Destination = "<LOG_PROCESS_ID>"
})

-- Forward message
Send({
  Target = "<ROUTER_PROCESS_ID>",
  Action = "ForwardMessage",
  Destination = "<TARGET_PROCESS_ID>",
  MessageData = "Hello from sender"
})

-- Broadcast to multiple processes
Send({
  Target = "<ROUTER_PROCESS_ID>",
  Action = "BroadcastMessage",
  Destinations = "<PROC1>,<PROC2>,<PROC3>",
  MessageData = "Announcement"
})
```

**Learning Outcomes**:
- Build message routing systems
- Implement inter-process communication
- Handle distributed coordination
- Create process networks

### Utility Examples

#### 5. Functional Programming (`utils/functional_programming.lua`)

**Purpose**: Comprehensive usage of the utils module for functional programming patterns.

**Key Features**:
- Map, filter, reduce operations
- Data processing pipelines
- Text and numeric analysis
- Dataset management and batch processing

**Usage**:
```lua
-- Process numbers functionally
Send({
  Target = "<PROCESS_ID>",
  Action = "ProcessNumbers",
  Numbers = "1,2,3,4,5,6,7,8,9,10",
  Operation = "analyze"
})

-- Process text with utils
Send({
  Target = "<PROCESS_ID>",
  Action = "ProcessText",
  Text = "Hello world this is a test",
  Operation = "analyze"
})

-- Store and process datasets
Send({
  Target = "<PROCESS_ID>",
  Action = "StoreDataset", 
  DatasetId = "test1",
  Data = "10,20,30,40,50",
  DataType = "numbers"
})

Send({
  Target = "<PROCESS_ID>",
  Action = "ProcessDataset",
  DatasetId = "test1", 
  Operations = "map_square,filter_even,reduce_sum"
})
```

**Learning Outcomes**:
- Master functional programming patterns
- Use utils module effectively
- Build data processing pipelines
- Implement immutable operations

### Integration Examples

#### 6. Arweave Interaction (`integration/arweave_interaction.lua`)

**Purpose**: Advanced patterns for interacting with Arweave and other AO processes.

**Key Features**:
- Arweave transaction reference management
- Distributed AO process coordination
- Query-response patterns with timeouts
- Content publishing workflows
- Process discovery and networking

**Usage**:
```lua
-- Store Arweave reference
Send({
  Target = "<PROCESS_ID>",
  Action = "StoreArweaveRef",
  TxId = "<43-CHAR-ARWEAVE-TX-ID>",
  ContentType = "document",
  Title = "Important Document"
})

-- Query AO process
Send({
  Target = "<PROCESS_ID>",
  Action = "QueryAOProcess",
  ProcessId = "<TARGET_PROCESS_ID>",
  QueryAction = "GetData",
  QueryData = "latest_stats"
})

-- Publish content
Send({
  Target = "<PROCESS_ID>",
  Action = "PublishContent",
  ContentData = "Hello decentralized world!",
  Title = "My First Post",
  NotifyProcesses = "<PROC1>,<PROC2>"
})
```

**Learning Outcomes**:
- Integrate with Arweave ecosystem
- Build distributed applications
- Coordinate multiple AO processes
- Implement content management systems

## Common Patterns and Best Practices

### Message Handling Patterns

1. **Action-Based Routing**:
```lua
if Msg.Action == "SpecificAction" then
  -- Handle specific action
  local param = Msg.Parameter
  -- Process and respond
  Send({
    Target = Msg.From,
    Action = "SpecificActionResponse",
    Data = "Result data"
  })
end
```

2. **Input Validation**:
```lua
if not Msg.RequiredParam then
  Send({
    Target = Msg.From,
    Action = "Error",
    Data = "Missing required parameter",
    ["Error-Type"] = "ValidationError"
  })
  return
end
```

3. **Error Handling**:
```lua
local success, result = pcall(function()
  -- Potentially failing operation
  return process_data(Msg.Data)
end)

if not success then
  Send({
    Target = Msg.From,
    Action = "Error",
    Data = "Processing failed: " .. tostring(result),
    ["Error-Type"] = "ProcessingError"
  })
  return
end
```

### State Management Patterns

1. **Persistent State**:
```lua
-- Initialize in _G namespace
_G.my_data = _G.my_data or {}
_G.counters = _G.counters or { total = 0 }

-- Update state
_G.counters.total = _G.counters.total + 1
_G.my_data[key] = value
```

2. **State Validation**:
```lua
local function validate_state()
  if not _G.required_field then
    error("State corruption: missing required field")
  end
end
```

3. **State History**:
```lua
_G.history = _G.history or {}
table.insert(_G.history, {
  timestamp = os.time(),
  action = action_name,
  data = action_data
})

-- Limit history size
if #_G.history > MAX_HISTORY then
  table.remove(_G.history, 1)
end
```

### Security Patterns

1. **Owner Validation**:
```lua
local function is_owner(msg)
  if not msg.commitments or not _G.owner then
    return false
  end
  
  for _, commitment in pairs(msg.commitments) do
    if commitment.type and 
       string.lower(commitment.type) ~= "hmac-sha256" and
       commitment.committer == _G.owner then
      return true
    end
  end
  return false
end
```

2. **Authority Checking**:
```lua
local function is_trusted_authority(msg)
  if not _G.authorities then return false end
  
  local from_process = msg["from-process"]
  if from_process == msg.from then
    for _, authority in ipairs(_G.authorities) do
      if authority == from_process then
        return true
      end
    end
  end
  return false
end
```

### Utils Module Patterns

1. **Functional Data Processing**:
```lua
-- Transform data
local squared = utils.map(function(x) return x * x end, numbers)

-- Filter data  
local evens = utils.filter(function(x) return x % 2 == 0 end, numbers)

-- Aggregate data
local sum = utils.reduce(function(acc, x) return acc + x end, 0, numbers)
```

2. **Safe Operations**:
```lua
if utils.isArray(data) then
  local result = utils.map(transform_func, data)
else
  -- Handle non-array data
end
```

## Development Tips

### Debugging

1. **Use Print Statements**:
```lua
print("DEBUG: Processing action " .. (Msg.Action or "unknown"))
print("DEBUG: State keys: " .. table.concat(utils.keys(_G), ", "))
```

2. **Message Logging**:
```lua
_G.message_log = _G.message_log or {}
table.insert(_G.message_log, {
  timestamp = os.time(),
  from = Msg.From,
  action = Msg.Action,
  data_length = #(Msg.Data or "")
})
```

3. **State Inspection**:
```lua
if Msg.Action == "Debug" then
  Send({
    Target = Msg.From,
    Action = "DebugResponse",
    Data = "Current state",
    ["State-Keys"] = table.concat(utils.keys(_G), ", "),
    ["Message-Count"] = tostring(#(_G.Inbox or {}))
  })
end
```

### Performance Optimization

1. **Batch Operations**:
```lua
-- Instead of multiple individual operations
local results = utils.map(function(item)
  return process_item(item)
end, batch_data)
```

2. **State Cleanup**:
```lua
-- Periodically clean up old data
if #_G.history > MAX_HISTORY then
  -- Remove oldest entries
  for i = 1, #_G.history - MAX_HISTORY do
    table.remove(_G.history, 1)
  end
end
```

3. **Lazy Initialization**:
```lua
_G.expensive_data = _G.expensive_data or function()
  -- Only compute when first needed
  return compute_expensive_data()
end
```

## Testing Your Examples

### Manual Testing

1. **Deploy** the example to an AOS process
2. **Send test messages** using the documented patterns
3. **Verify responses** match expected behavior
4. **Check state persistence** across multiple messages

### Automated Testing

Create test scripts that send sequences of messages:

```lua
-- Test script example
local test_messages = {
  {Action = "Hello", expected_response = "HelloResponse"},
  {Action = "Status", expected_response = "StatusResponse"},
  {Action = "Help", expected_response = "HelpResponse"}
}

for _, test in ipairs(test_messages) do
  Send({Target = TARGET_PROCESS, Action = test.Action})
  -- Verify response...
end
```

## Contributing

To contribute new examples:

1. Follow the established patterns and documentation style
2. Include comprehensive inline comments
3. Provide usage examples and learning outcomes
4. Test thoroughly with various input scenarios
5. Update this README with your new example

## Support and Resources

- **AO Documentation**: [cookbook_ao.ar.io](https://cookbook_ao.ar.io)
- **Hyper-AOS Repository**: See main project README
- **Community**: Join AO community discussions for help and feedback

## License

These examples are provided under the same license as the Hyper-AOS project. See the main LICENSE file for details.