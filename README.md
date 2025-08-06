# 🚀 Hyper-AOS

[![Build Status](https://img.shields.io/badge/build-passing-brightgreen)](https://github.com/twilson63/hyper-aos)
[![Tests](https://img.shields.io/badge/tests-151%20passing-brightgreen)](./aos_test_suite)
[![Version](https://img.shields.io/badge/version-dev-blue)](./CHANGELOG.md)
[![License](https://img.shields.io/badge/license-MIT-blue)](./LICENSE)
[![AO Compatible](https://img.shields.io/badge/AO-compatible-orange)](https://ao.arweave.dev)

> Next-generation console for AO (Arweave Operating System) processes on Hyperbeam with native Lua device, enhanced security, and professional build system.

## 📖 Table of Contents

- [Overview](#overview)
- [✨ Features](#-features)
- [🚀 Quick Start](#-quick-start)
- [📦 Installation](#-installation)
- [🏗️ Project Structure](#️-project-structure)
- [🛠️ Build System](#️-build-system)
- [💻 Development](#-development)
- [🧪 Testing](#-testing)
- [📤 Deployment](#-deployment)
- [📚 Documentation](#-documentation)
- [🔧 API Reference](#-api-reference)
- [🤝 Contributing](#-contributing)
- [📞 Support](#-support)

## Overview

Hyper-AOS is an enhanced implementation of the AO protocol using Hyperbeam's native Lua device with LUERL sandboxing on the BEAM platform. It provides a secure, colorized console for interacting with AO processes through message passing in a decentralized supercomputer.

### Why Hyper-AOS?

- **🔐 Enhanced Security**: Authority-based trust validation and message authentication
- **🎨 Beautiful Output**: Colorized terminal with syntax highlighting
- **⚡ Native Performance**: Runs directly on Hyperbeam's Lua device
- **🧪 Battle-Tested**: Comprehensive test suite with 151+ passing tests
- **🔧 Professional Tooling**: Unified build system with make commands
- **📦 Modular Design**: Clean separation of concerns with utils module

## ✨ Features

- 🔐 **Enhanced Security**: Message authentication with `meta.authorities` and trust verification
- 🎨 **Colorized Terminal Output**: Beautiful syntax highlighting for tables and prompts
- 📝 **Smart Message Handling**: Automatic `from` field resolution and message validation
- 🚀 **Native Lua Performance**: Optimized for Hyperbeam's Lua device
- 🧪 **Comprehensive Test Suite**: Full coverage with unit, integration, and security tests
- 🛠️ **Professional Build System**: Automated concatenation and deployment
- 📚 **Utils Module**: Functional programming utilities for message processing

## 🚀 Quick Start

Get up and running in under a minute:

```bash
# Clone the repository
git clone https://github.com/twilson63/hyper-aos.git
cd hyper-aos

# Install all dependencies
make install-deps

# Build the module
make build

# Run tests
make test

# Deploy to Arweave
make deploy
```

That's it! You now have a fully functional Hyper-AOS installation.

## 📦 Installation

### Prerequisites

- **Git**: Version control system
- **Lua**: 5.1+ or LuaJIT
- **Node.js**: 16+ for deployment tools
- **Erlang/OTP**: 24+ (optional, for running tests)

### Automatic Installation

Use our unified build system for one-command setup:

```bash
# Install all dependencies automatically
make install-deps
```

This will install:
- ✅ Hype framework (build tool)
- ✅ Rebar3 (Erlang build tool)
- ✅ ARX (Arweave deployment tool)

### Manual Installation

If you prefer manual setup:

```bash
# Install Hype
npm install -g @twilson63/hype

# Install ARX
npm install -g @permaweb/arx

# Install Rebar3 (for tests)
# macOS
brew install rebar3

# Linux
wget https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
sudo mv rebar3 /usr/local/bin/
```

### Configuration

Set up your Arweave wallet for deployment:

```bash
# Set wallet path
export WALLET_PATH=/path/to/your/arweave-wallet.json

# Or copy to config directory
cp your-wallet.json config/wallet.json
```

## 🏗️ Project Structure

```
hyper-aos/
├── 📁 src/                      # Source modules
│   ├── aos.lua                  # Core AOS implementation
│   └── utils.lua                # Functional utilities
├── 📁 dist/                     # Build output
│   └── aos.lua                  # Concatenated module
├── 📁 scripts/                  # Build & automation
│   ├── build.lua                # Build script
│   ├── deploy.lua               # Deployment script
│   └── test.lua                 # Test runner
├── 📁 config/                   # Configuration files
│   ├── wallet.json              # Arweave wallet
│   └── *.json                   # Environment configs
├── 📁 docs/                     # Documentation
│   ├── ARCHITECTURE.md          # System design
│   ├── API.md                   # API reference
│   ├── DEVELOPMENT.md           # Developer guide
│   ├── DEPLOYMENT.md            # Deployment guide
│   └── SECURITY.md              # Security docs
├── 📁 aos_test_suite/           # Test suite
│   ├── test/                    # Test categories
│   │   ├── unit/                # Unit tests
│   │   ├── integration/         # Integration tests
│   │   ├── security/            # Security tests
│   │   └── helpers/             # Test utilities
│   └── TEST_GUIDE.md            # Testing documentation
├── 📁 examples/                 # Example code
│   ├── basic/                   # Basic examples
│   ├── advanced/                # Advanced patterns
│   └── utils/                   # Utils examples
├── 📄 Makefile                  # Build system
├── 📄 README.md                 # This file
└── 📄 CLAUDE.md                 # AI assistant guide
```

## 🛠️ Build System

Our professional Makefile provides comprehensive automation:

### Primary Commands

| Command | Description |
|---------|-------------|
| `make help` | Show all available commands with descriptions |
| `make build` | Build concatenated Lua module |
| `make test` | Run all tests (unit + integration) |
| `make deploy` | Deploy to Arweave network |
| `make clean` | Remove build artifacts |

### Development Commands

| Command | Description |
|---------|-------------|
| `make dev` | Development build with hot reload |
| `make watch` | Watch files for changes |
| `make lint` | Run code linters |
| `make format` | Format code files |
| `make check` | Run all checks (lint + test) |

### Testing Commands

| Command | Description |
|---------|-------------|
| `make test-quick` | Run quick smoke tests |
| `make test-unit` | Run unit tests only |
| `make test-integration` | Run integration tests |
| `make test-security` | Run security tests |
| `make test-coverage` | Generate coverage report |

### Utility Commands

| Command | Description |
|---------|-------------|
| `make install-deps` | Install all dependencies |
| `make status` | Show project status |
| `make info` | Show build information |
| `make release` | Create release package |
| `make docs` | Generate documentation |

### Example Usage

```bash
# Full development cycle
make clean         # Clean previous builds
make build         # Build the module
make test          # Run all tests
make deploy        # Deploy to Arweave

# Development workflow
make dev           # Start development mode
# (In another terminal)
make watch         # Auto-rebuild on changes

# Quality checks
make lint          # Check code style
make format        # Auto-format code
make check         # Lint + test
```

## 💻 Development

### Development Workflow

1. **Setup Environment**
   ```bash
   make install-deps
   make status        # Verify installation
   ```

2. **Development Mode**
   ```bash
   make dev           # Build and watch
   ```

3. **Make Changes**
   - Edit files in `src/`
   - Auto-rebuild on save
   - See changes in `dist/`

4. **Test Your Changes**
   ```bash
   make test-quick    # Quick validation
   make test          # Full test suite
   ```

5. **Code Quality**
   ```bash
   make lint          # Check style
   make format        # Auto-format
   ```

### Build Process

The build system concatenates `src/utils.lua` and `src/aos.lua` into `dist/aos.lua`:

```bash
# Manual build
make build

# Output
✓ Module concatenated
  Output: dist/aos.lua
  Size: 45K
  Lines: 1337
```

### Hot Reload

Enable automatic rebuilding during development:

```bash
# Terminal 1: Build watch
make watch

# Terminal 2: Run tests on change
watch -n 2 'make test-quick'
```

## 🧪 Testing

### Test Suite Overview

Our comprehensive test suite ensures reliability and security:

- **151+ Tests**: Covering all critical functionality
- **4 Test Categories**: Unit, Integration, Security, Legacy
- **EUnit Framework**: Professional Erlang testing
- **LUERL Sandbox**: Secure Lua execution

### Running Tests

```bash
# Run all tests
make test

# Run specific categories
make test-unit         # Unit tests only
make test-integration  # Integration tests
make test-security     # Security tests

# Run with profiles
cd aos_test_suite
rebar3 as authorities_test eunit  # Authorities tests
rebar3 as security_test eunit     # All security tests
```

### Test Categories

#### Unit Tests (test/unit/)
- Color output formatting
- String manipulation
- Math operations
- Utils functions

#### Integration Tests (test/integration/)
- Multi-step evaluation
- State persistence
- Message processing
- Utils integration

#### Security Tests (test/security/)
- Authority validation
- Owner verification
- Trust checking
- Message authentication

### Test Coverage

```bash
# Generate coverage report
make test-coverage

# View results
open aos_test_suite/cover/index.html
```

### Writing Tests

See [TEST_GUIDE.md](./aos_test_suite/TEST_GUIDE.md) for detailed testing documentation.

## 📤 Deployment

### Quick Deploy

```bash
# Deploy with environment variable
export WALLET_PATH=/path/to/wallet.json
make deploy

# Deploy with config file
cp wallet.json config/wallet.json
make deploy
```

### Manual Deployment

```bash
# Build first
make build

# Deploy with ARX
arx upload dist/aos.lua \
  -w $WALLET_PATH \
  -t arweave \
  --content-type application/lua \
  --tags Data-Protocol ao \
  --tags Module-Format concatenated \
  --tags App-Name HyperAOS
```

### Launch AOS Console

After deployment, launch your AOS instance:

```bash
# Install AOS CLI (if needed)
npm i -g https://preview_ao.arweave.net

# Launch with your module
aos console \
  --module <TX_ID> \
  --mainnet <HYPERBEAM_SERVER> \
  <PROCESS_NAME>

# Example
aos console \
  --module abc123def456... \
  --mainnet https://hyperbeam.example.com \
  my-hyper-aos
```

## 📚 Documentation

Comprehensive documentation is available in the `docs/` directory:

### Core Documentation

- 📘 [ARCHITECTURE.md](./docs/ARCHITECTURE.md) - System design and components
- 📗 [API.md](./docs/API.md) - Complete API reference
- 📙 [DEVELOPMENT.md](./docs/DEVELOPMENT.md) - Developer guide
- 📕 [DEPLOYMENT.md](./docs/DEPLOYMENT.md) - Deployment strategies
- 📔 [SECURITY.md](./docs/SECURITY.md) - Security model and best practices
- 📓 [BUILD_SYSTEM.md](./docs/BUILD_SYSTEM.md) - Build system details

### Guides & Tutorials

- 🧪 [TEST_GUIDE.md](./aos_test_suite/TEST_GUIDE.md) - Testing documentation
- 🎯 [UTILS_README.md](./docs/UTILS_README.md) - Utils module guide
- 🔧 [CLAUDE.md](./CLAUDE.md) - AI assistant integration

### Examples

Browse the `examples/` directory for practical code samples:

```lua
-- examples/basic/hello_world.lua
return compute(function(state, msg)
  print("Hello from Hyper-AOS!")
  return {ok = state}
end)
```

## 🔧 API Reference

### Core Functions

#### `compute(state, assignment)`
Main entry point for message processing.

```lua
function compute(state, assignment)
  -- Process assignment
  return {ok, updatedState}
end
```

#### `meta` Table
Security and utility functions:

```lua
meta.owner              -- Process owner address
meta.authorities        -- List of trusted authorities
meta.is_owner(msg)      -- Check if message from owner
meta.is_trusted(msg)    -- Verify message trust
meta.ensure_message(msg) -- Process message fields
```

#### Enhanced Functions

```lua
print(...)              -- Colorized output
stringify(table)        -- Convert tables to colored strings
prompt()               -- Return colorized prompt
```

### Utils Module

Functional programming utilities:

```lua
utils.curry(fn)         -- Function currying
utils.compose(...)      -- Function composition
utils.map(fn, array)    -- Array mapping
utils.filter(fn, array) -- Array filtering
utils.reduce(fn, init, array) -- Array reduction
```

See [API.md](./docs/API.md) for complete reference.

## 🤝 Contributing

We welcome contributions! Please see our guidelines:

- 📋 [CONTRIBUTING.md](./CONTRIBUTING.md) - Contribution guidelines
- 📜 [CODE_OF_CONDUCT.md](./CODE_OF_CONDUCT.md) - Community standards
- 🐛 [Issues](https://github.com/twilson63/hyper-aos/issues) - Report bugs
- 💡 [Discussions](https://github.com/twilson63/hyper-aos/discussions) - Ideas & questions

### Development Process

1. Fork the repository
2. Create feature branch (`git checkout -b feature/amazing`)
3. Make changes and test (`make test`)
4. Commit changes (`git commit -m 'Add amazing feature'`)
5. Push branch (`git push origin feature/amazing`)
6. Open Pull Request

### Code Quality Standards

- ✅ All tests must pass
- ✅ Follow existing code style
- ✅ Add tests for new features
- ✅ Update documentation
- ✅ Run `make check` before committing

## 📞 Support

### Getting Help

- 📖 Check the [documentation](./docs/)
- 🔍 Search [existing issues](https://github.com/twilson63/hyper-aos/issues)
- 💬 Join [Arweave Discord](https://discord.gg/arweave)
- 📚 Visit [AO Cookbook](https://cookbook_ao.ar.io)

### Reporting Issues

When reporting issues, please include:
- Hyper-AOS version (`make info`)
- System information
- Steps to reproduce
- Error messages
- Expected behavior

### Quick Troubleshooting

```bash
# Check installation
make status

# Verify dependencies
make install-deps

# Clean and rebuild
make clean && make build

# Run diagnostics
make info
```

## 📄 License

This project is part of the Arweave ecosystem. See [LICENSE](./LICENSE) for details.

## 🙏 Acknowledgments

- [Permaweb](https://github.com/permaweb) team for AOS and Hyperbeam
- [LUERL](https://github.com/rvirding/luerl) for Lua in Erlang
- [AO Community](https://ao.arweave.dev) for feedback and support
- Original [AOS stringify](https://github.com/permaweb/aos/blob/main/process/stringify.lua) implementation

---

<div align="center">
  <strong>Built with ❤️ for the Arweave ecosystem</strong>
  <br>
  <sub>⭐ Star us on GitHub!</sub>
</div>