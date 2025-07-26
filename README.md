# Hyper-AOS

A next-generation console for working with AO (Arweave Operating System) processes on Hyperbeam using native Lua device. This enhanced version provides advanced security features, colorized output, and improved developer experience.

## Features

- 🔐 **Enhanced Security**: Message authentication with `meta.authorities` and trust verification
- 🎨 **Colorized Terminal Output**: Beautiful syntax highlighting for tables and prompts
- 📝 **Smart Message Handling**: Automatic `from` field resolution and message validation
- 🚀 **Native Lua Performance**: Runs directly on Hyperbeam's Lua device
- 🧪 **Comprehensive Test Suite**: Full test coverage using Erlang/LUERL

## Prerequisites

- [Permaweb/ARX](https://github.com/permaweb/arx) - Arweave transaction toolkit
- Arweave wallet keyfile (JSON format)
- [Hyperbeam](https://github.com/permaweb/hyperbeam) node access
- [AOS Console](https://github.com/permaweb/aos) installed

## Installation

### 1. Install ARX

```bash
npm install -g @permaweb/arx
```

### 2. Set up your Arweave wallet

```bash
export WALLET_PATH=/path/to/your/arweave-wallet.json
```

### 3. Publish aos.lua to Arweave

```bash
arx upload aos.lua -w $WALLET_PATH -t arweave --content-type application/lua --tags Data-Protocol ao
```

Save the returned transaction ID (TX_ID) - you'll need this to launch your AOS instance.

## Usage

### Launching Hyper-AOS

> Install preview version of aos (npm i -g https://preview_ao.arweave.net)

Connect to your Hyperbeam node and launch AOS with your module:

```bash
aos console --module <TX_ID> --mainnet <YOUR_HYPERBEAM_SERVER> <PROCESS_NAME>
```

Example:
```bash
aos console --module abc123def456... --mainnet https://hyperbeam.example.com my-hyper-aos
```

### Core Features

#### 1. Enhanced Security

The module includes built-in security features for message validation:

```lua
-- Messages are automatically validated for trust
-- Trusted messages have msg.trusted = true
-- Based on authorities defined in process initialization
```

#### 2. Colorized Output

Tables and data structures are automatically formatted with colors:

```lua
-- Print any table with beautiful colors
print({
  name = "Alice",
  data = {1, 2, 3},
  active = true
})
-- Output will show with:
-- - Red keys
-- - Green strings  
-- - Blue numbers/booleans
```

#### 3. Colored Prompt

The interactive prompt displays with syntax highlighting:
```
hyper~aos@dev[3]> 
```
- "hyper" in cyan bold
- "aos" in bright green
- Version in yellow
- Inbox count in bright magenta
- Prompt symbol in bright blue

## API Reference

### Meta Table

The `meta` table provides core security and utility functions:

- `meta.owner` - Process owner address
- `meta.authorities` - List of trusted authorities
- `meta.colors` - Terminal color codes
- `meta.is_owner(msg)` - Check if message is from owner
- `meta.is_trusted(msg)` - Verify message trust status
- `meta.ensure_message(msg)` - Process message to set `from` field and trust status

### Enhanced Functions

- `print(...)` - Enhanced print with automatic table colorization
- `stringify(table)` - Convert tables to colored string format
- `prompt()` - Returns colorized prompt string

### Standard AOS Functions

- `compute(state, assignment)` - Main entry point for message processing
- `eval(msg)` - Evaluate Lua code (owner-only)
- `send(msg)` - Send messages to other processes

## Development

### Running Tests

The project includes a comprehensive test suite using Erlang and LUERL:

```bash
cd aos_test_suite
rebar3 eunit
```

Run specific test modules:
```bash
rebar3 eunit -m aos_colors_test
rebar3 eunit -m aos_stringify_test
rebar3 eunit -m aos_authorities_test  # Run with authorities_test profile
```

### Test Coverage

- ✅ Security features (owner validation, authorities)
- ✅ Message trust verification  
- ✅ Color output formatting
- ✅ Table stringify functionality
- ✅ Multi-step evaluation scenarios
- ✅ State persistence
- ✅ Message `from` field resolution

## Architecture

```
hyper-aos-demo/
├── aos.lua                     # Enhanced AOS module
├── CLAUDE.md                   # Development notes
├── demo.json                   # Example wallet
└── aos_test_suite/            
    ├── src/                    # Test helpers
    │   └── aos_test_helpers.erl
    └── test/                   # Test cases
        ├── aos_colors_test.erl
        ├── aos_stringify_test.erl
        ├── aos_authorities_test.erl
        ├── aos_multi_step_test.erl
        └── ...
```

## Security Model

### 1. Owner Validation
- Process owner set from first non-HMAC commitment
- Only owner can execute `eval` commands
- Flexible commitment type support

### 2. Authority System  
- Define trusted authorities in process message
- Authorities parsed from comma-separated string
- Each authority must be exactly 43 characters

### 3. Message Trust
- Messages trusted when `from == from-process` AND committer is an authority
- Trust status stored in `msg.trusted`
- Automatic trust verification via `meta.ensure_message()`

### 4. Message From Field
Priority resolution:
1. Existing `from` field
2. `from-process` field  
3. First non-HMAC commitment committer

## Advanced Usage

### Setting Authorities

During process initialization:
```lua
{
  type = "process",
  authority = "Auth1234...43chars,Auth5678...43chars",
  commitments = { ... }
}
```

### Checking Trust Status

```lua
-- In your message handler
if msg.trusted then
  -- Message is from a trusted source
  processPrivilegedCommand(msg)
else
  -- Handle untrusted message
  processPublicCommand(msg)
end
```

## Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Run tests to ensure compatibility
4. Commit your changes (`git commit -m 'Add amazing feature'`)
5. Push to the branch (`git push origin feature/amazing-feature`)
6. Open a Pull Request

## License

This project is part of the Arweave ecosystem. Please refer to the appropriate licenses.

## Acknowledgments

- [Permaweb](https://github.com/permaweb) team for AOS and Hyperbeam
- [LUERL](https://github.com/rvirding/luerl) for Lua in Erlang
- Original [AOS stringify](https://github.com/permaweb/aos/blob/main/process/stringify.lua) implementation

## Support

For issues and questions:
- Open an issue in this repository
- Join the Arweave Discord server
- Check the [AO Cookbook](https://cookbook_ao.ar.io) for documentation
