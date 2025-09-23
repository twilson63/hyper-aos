# Changelog

All notable changes to the Hyper-AOS project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Configuration management files for different environments
- Project governance and contribution guidelines
- Comprehensive development, production, and test configurations

## [1.0.0] - 2024-12-XX

### Added
- Initial release of Hyper-AOS
- Enhanced AOS module with security features and colorized output
- Functional programming utilities module (utils.lua)
- Comprehensive test suite with 51+ test cases
- LUERL sandbox integration for secure Lua execution
- Backwards compatibility alias for Utils module

### Security
- RSA-PSS-512 commitment validation for process ownership
- Authority-based trust verification system
- Message normalization and `from` field resolution
- Secure sandboxed execution environment

### Features
- **Enhanced Security**: Message authentication with authorities and trust verification
- **Colorized Terminal Output**: Beautiful syntax highlighting for tables and prompts
- **Smart Message Handling**: Automatic `from` field resolution and message validation
- **Native Lua Performance**: Runs directly on Hyperbeam's Lua device
- **Functional Programming**: Comprehensive utils module with pattern matching, currying, and array operations

## [0.3.0] - 2024-11-XX

### Added
- Utils module integration with comprehensive functional programming support
- Pattern matching functions (matchesPattern, matchesSpec)
- Functional primitives (curry, compose, reduce, map, filter)
- Array operations (concat, reverse, find, includes)
- Object operations (prop, propEq, keys, values)
- LUERL-optimized implementations with type safety

### Fixed
- LUERL loading conflicts with utils helper functions
- Utils curry limitation workarounds for LUERL compatibility
- Type conversion errors between Erlang maps and Lua tables

### Changed
- Moved utils helper functions out of aos.lua to avoid loading conflicts
- Added backwards compatibility alias `_G.Utils -> _G.utils`
- Optimized utils module for LUERL with `#array` operator instead of `ipairs`

## [0.2.0] - 2024-10-XX

### Added
- Multi-step evaluation scenario testing
- Authority-based message trust system
- Enhanced message processing with `from` field resolution
- Process initialization from commitment analysis
- Comprehensive security validation tests

### Security
- **Process Initialization**: Owner and ID set from first non-HMAC commitment
- **Authority Parsing**: Support for comma-separated 43-character addresses
- **Trust Validation**: Checks `from-process` field and authority membership
- **Message Validation**: Ensures proper `from` field with priority resolution

### Testing
- Added security test suite with authorities profile
- Integration tests for multi-step scenarios
- Validation tests for ownership and trust mechanisms
- Test helpers for common validation scenarios

### Changed
- State management now uses `_G` namespace directly instead of State table
- Private functions moved to local `meta` table for security
- System keys excluded from state serialization

## [0.1.0] - 2024-09-XX

### Added
- Initial Hyper-AOS implementation
- Core aos.lua module with AO compute environment
- LUERL sandbox integration for secure Lua execution
- Basic colorized output system
- EUnit-based test framework
- Message processing and state persistence

### Features
- **Core Security**: Basic owner validation and message processing
- **Color System**: Terminal color codes and table formatting
- **State Persistence**: Global state management with `_G` namespace
- **Test Framework**: Erlang/EUnit test infrastructure

### Security
- Basic RSA-PSS-512 commitment validation
- Message structure validation
- Sandbox environment protection

### Testing
- Unit tests for color output and table stringify
- Basic security tests for owner validation
- Test helpers and utilities for common scenarios

---

## Version History Summary

- **v1.0.0**: Full production release with comprehensive security and utils integration
- **v0.3.0**: Utils module integration and LUERL optimizations
- **v0.2.0**: Enhanced security with authorities and trust verification
- **v0.1.0**: Initial implementation with basic features and testing

## Migration Guides

### Upgrading from v0.2.x to v0.3.x

1. **Utils Module**: The utils module is now automatically available as `_G.utils`
2. **Backwards Compatibility**: Old `_G.Utils` references are still supported via alias
3. **LUERL Optimizations**: Consider using direct function calls instead of curry for better performance

### Upgrading from v0.1.x to v0.2.x

1. **Authority System**: Configure authorities in process messages if using trust features
2. **Message Structure**: Ensure messages include proper commitment structures
3. **State Management**: Update any direct State table references to use `_G`

## Breaking Changes

### v0.3.0
- Utils helper functions removed from aos.lua (use utils.lua module instead)
- Curry function has LUERL-specific limitations

### v0.2.0
- State table replaced with `_G` namespace usage
- Message validation now requires proper commitment structures
- Trust system requires authority configuration

### v0.1.0
- Initial implementation, no breaking changes from previous versions

## Security Advisories

### SA-2024-001: State Isolation
- **Affected**: v0.1.x
- **Impact**: Potential state leakage between processes
- **Fixed in**: v0.2.0
- **Action**: Upgrade to v0.2.0+ and verify state isolation in tests

### SA-2024-002: Authority Validation
- **Affected**: v0.1.x
- **Impact**: Missing authority validation in trust system
- **Fixed in**: v0.2.0
- **Action**: Configure authorities properly and test trust validation

## Development Process

This changelog follows these principles:

- **Semantic Versioning**: MAJOR.MINOR.PATCH format
- **Keep a Changelog**: Standard changelog format
- **Security First**: Security changes are prominently documented
- **Breaking Changes**: Clearly marked and explained
- **Migration Guides**: Provided for major version changes

## Contributing to Changelog

When contributing changes:

1. Add entries to `[Unreleased]` section
2. Use categories: Added, Changed, Deprecated, Removed, Fixed, Security
3. Include issue/PR references where applicable
4. Follow existing format and style
5. Move items to versioned sections on release

For more information, see [CONTRIBUTING.md](CONTRIBUTING.md).