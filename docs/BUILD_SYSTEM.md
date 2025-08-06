# Lua Script Concatenation Build System

## Overview

This build system automates the concatenation of multiple Lua modules into a single distributable file for deployment to the HyperBEAM/LUERL environment. It uses the Hype framework for building and deploying to Arweave.

## Features

- 🔨 **Automated Module Concatenation**: Combines multiple Lua files into a single bundle
- 📦 **Module Registration**: Properly registers modules in `_G.package.loaded` for LUERL
- 🚀 **Arweave Deployment**: Direct deployment to Arweave mainnet with ANS-104
- ✅ **Comprehensive Testing**: Validates syntax, structure, and LUERL compatibility
- 🎨 **Colorized Output**: Clear, visual feedback during build process
- 🔄 **Auto-Installation**: Automatically installs Hype framework if not present

## Quick Start

```bash
# Build the concatenated module
make build

# Run tests to validate
make test

# Deploy to Arweave (requires wallet)
make deploy

# Show all available commands
make help
```

## Installation

### Prerequisites

- **macOS** or **Linux** operating system
- **Lua** modules to concatenate (aos.lua, utils.lua)
- **Wallet** for Arweave deployment (wallet.json or demo.json)

### Optional Tools

- **Lua interpreter**: For syntax validation
- **arx or arkb**: For Arweave deployment
- **fswatch**: For file watching in development

## Project Structure

```
hyper-aos-demo/
├── Makefile                 # Build automation
├── BUILD_SYSTEM.md          # This documentation
├── .hype/
│   └── config.json          # Hype configuration
├── scripts/
│   ├── build.lua            # Concatenation script
│   ├── deploy.lua           # Deployment script
│   └── test.lua             # Validation tests
├── src/                     # Source modules
│   ├── aos.lua
│   └── utils.lua
└── dist/                    # Build output
    └── aos.lua              # Concatenated bundle
```

## Build Process

### 1. Module Concatenation

The build script (`scripts/build.lua`) performs the following:

1. **Reads source modules** from `src/` directory
2. **Maintains dependency order** (utils before aos)
3. **Wraps each module** in `_G.package.loaded["modulename"]`
4. **Processes require() statements** for LUERL compatibility
5. **Adds build metadata** (timestamp, module list)
6. **Outputs to `dist/aos.lua`**

Example output structure:
```lua
-- HyperAOS Concatenated Module Bundle
-- Generated: 2024-01-15 10:30:00
-- Modules: utils, aos

_G.package = _G.package or {}
_G.package.loaded = _G.package.loaded or {}

-- Module: utils
_G.package.loaded["utils"] = (function()
  -- utils.lua content
end)()

-- Module: aos  
_G.package.loaded["aos"] = (function()
  -- aos.lua content with require("utils") working
end)()
```

### 2. Testing

Run comprehensive tests with:
```bash
make test
```

Tests validate:
- ✅ Output file exists
- ✅ File size is reasonable
- ✅ Module registration present
- ✅ Both required modules included
- ✅ Correct module load order
- ✅ Valid Lua syntax
- ✅ LUERL compatibility patterns
- ✅ Build metadata present
- ✅ Runtime execution succeeds

### 3. Deployment

Deploy to Arweave with:
```bash
make deploy
```

The deployment script:
1. **Locates your wallet** (wallet.json, demo.json, or WALLET_PATH)
2. **Confirms deployment** with user
3. **Uploads to Arweave** with proper tags
4. **Returns transaction ID** for reference

## Makefile Targets

| Command | Description |
|---------|-------------|
| `make build` | Build concatenated Lua file |
| `make test` | Run validation tests |
| `make deploy` | Deploy to Arweave |
| `make clean` | Remove build artifacts |
| `make install-hype` | Install Hype framework |
| `make status` | Show project status |
| `make dev-build` | Build with preview output |
| `make watch` | Watch files for changes |
| `make clean-all` | Deep clean all generated files |
| `make help` | Show available commands |

## Configuration

### Hype Configuration (`.hype/config.json`)

```json
{
  "name": "hyper-aos-build",
  "version": "1.0.0",
  "plugins": ["@twilson63/hype-plugin-ans104"],
  "wallet": "wallet.json",
  "network": "mainnet",
  "tags": {
    "Data-Protocol": "ao",
    "Content-Type": "application/lua",
    "Module-Format": "concatenated",
    "App-Name": "HyperAOS"
  }
}
```

### Environment Variables

- `WALLET_PATH`: Path to Arweave wallet (overrides default locations)

## Adding New Modules

To add a new module to the build:

1. **Place the module** in `src/` directory
2. **Update `scripts/build.lua`** to include it:
```lua
local modules = {
    {name = "utils", path = "src/utils.lua", optional = false},
    {name = "newmodule", path = "src/newmodule.lua", optional = false},
    {name = "aos", path = "src/aos.lua", optional = false}
}
```
3. **Rebuild** with `make build`

## Development Workflow

### Interactive Development

```bash
# Watch for changes and auto-rebuild
make watch

# Build and preview output
make dev-build

# Check project status
make status
```

### CI/CD Integration

Add to your GitHub Actions workflow:

```yaml
- name: Build HyperAOS Module
  run: |
    make build
    make test

- name: Deploy to Arweave
  if: github.ref == 'refs/heads/main'
  env:
    WALLET_PATH: ${{ secrets.ARWEAVE_WALLET }}
  run: make deploy
```

## Troubleshooting

### Build Fails

- **Missing source files**: Ensure aos.lua and utils.lua exist in src/ or root
- **Syntax errors**: Check Lua syntax with `lua -c src/*.lua`
- **Module order issues**: Verify dependencies in `scripts/build.lua`

### Deployment Fails

- **No wallet found**: Create wallet.json or set WALLET_PATH
- **Insufficient funds**: Ensure wallet has AR tokens
- **Network issues**: Check internet connectivity
- **Tool not installed**: Install arx with `npm install -g @permaweb/arx`

### Test Failures

- **File not found**: Run `make build` before testing
- **Module registration**: Check _G.package.loaded pattern in output
- **LUERL compatibility**: Verify _G namespace usage

## Performance

- **Build time**: < 5 seconds
- **Deployment time**: < 30 seconds  
- **Bundle size**: ~35-40 KB for aos + utils
- **Zero runtime overhead** from concatenation

## Security Considerations

- **Wallet security**: Never commit wallet.json to version control
- **Use .gitignore**: Add wallet files and secrets
- **Environment variables**: Use WALLET_PATH for CI/CD
- **Validate modules**: Test thoroughly before deployment

## License

This build system is part of the HyperAOS project and follows the same license terms.

## Support

For issues or questions:
- Check the [troubleshooting section](#troubleshooting)
- Review the [CLAUDE.md](CLAUDE.md) for project conventions
- Open an issue in the project repository