# Contributing to Hyper-AOS

Welcome to the Hyper-AOS project! We're excited that you're interested in contributing to this high-performance implementation of the AO (Arweave Operating System) protocol using Hyperbeam's native Lua device with LUERL sandboxing.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Setup](#development-setup)
- [Contributing Guidelines](#contributing-guidelines)
- [Pull Request Process](#pull-request-process)
- [Testing](#testing)
- [Code Style](#code-style)
- [Documentation](#documentation)
- [Security](#security)
- [Community](#community)

## Code of Conduct

This project and everyone participating in it is governed by our [Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code. Please report unacceptable behavior to the project maintainers.

## Getting Started

### Prerequisites

Before contributing, ensure you have the following installed:

- [Erlang/OTP 24+](https://www.erlang.org/downloads)
- [Rebar3](https://rebar3.org/)
- [ARX](https://github.com/permaweb/arx) - Arweave transaction toolkit
- [AOS Console](https://github.com/permaweb/aos)
- Git
- A text editor with Lua and Erlang syntax highlighting

### Project Architecture

Hyper-AOS consists of four main components:

1. **aos.lua** - Core Lua module implementing AO compute environment
2. **utils.lua** - Functional programming utilities module
3. **LUERL Sandbox** - Erlang-based Lua interpreter
4. **Test Suite** - Comprehensive EUnit-based testing framework

## Development Setup

1. **Fork and Clone the Repository**
   ```bash
   git clone https://github.com/your-username/hyper-aos-demo.git
   cd hyper-aos-demo
   ```

2. **Set Up Development Environment**
   ```bash
   # Install dependencies
   cd aos_test_suite
   rebar3 get-deps
   
   # Run initial test to verify setup
   rebar3 eunit
   ```

3. **Configure Your Wallet**
   ```bash
   export WALLET_PATH=/path/to/your/arweave-wallet.json
   ```

4. **Verify Installation**
   ```bash
   # Run Lua tests
   hype run test.lua
   
   # Run comprehensive test suite
   cd aos_test_suite
   make eunit
   ```

## Contributing Guidelines

### Types of Contributions

We welcome various types of contributions:

- **Bug Reports** - Help us identify and fix issues
- **Feature Requests** - Suggest new functionality
- **Code Contributions** - Submit bug fixes or new features
- **Documentation** - Improve docs, examples, or tutorials
- **Testing** - Add test cases or improve test coverage
- **Performance** - Optimize existing code

### Before You Start

1. **Check Existing Issues** - Look for existing issues or discussions
2. **Create an Issue** - For significant changes, create an issue first to discuss
3. **Understand the Codebase** - Read through [CLAUDE.md](CLAUDE.md) and existing code
4. **Review Security Model** - Understand our security requirements and validation rules

### Development Workflow

1. **Create a Feature Branch**
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Make Your Changes**
   - Follow our code style guidelines
   - Write comprehensive tests
   - Update documentation as needed
   - Ensure all tests pass

3. **Commit Your Changes**
   ```bash
   git add .
   git commit -m "feat: add your feature description"
   ```
   
   Use conventional commit messages:
   - `feat:` - New features
   - `fix:` - Bug fixes
   - `docs:` - Documentation changes
   - `test:` - Test additions or modifications
   - `refactor:` - Code refactoring
   - `perf:` - Performance improvements
   - `chore:` - Maintenance tasks

## Pull Request Process

1. **Run Full Test Suite**
   ```bash
   cd aos_test_suite
   make clean && make all
   rebar3 eunit
   rebar3 as authorities_test eunit  # For security tests
   ```

2. **Update Documentation**
   - Update README.md if needed
   - Add or update code comments
   - Update API documentation

3. **Create Pull Request**
   - Use a descriptive title
   - Fill out the PR template completely
   - Link to related issues
   - Include screenshots for UI changes

4. **Address Review Feedback**
   - Respond to reviewer comments
   - Make requested changes
   - Keep the PR up to date with main branch

### PR Requirements

- [ ] All tests pass
- [ ] Code follows style guidelines
- [ ] Documentation is updated
- [ ] Commit messages follow convention
- [ ] No breaking changes (or clearly documented)
- [ ] Security considerations addressed

## Testing

### Running Tests

```bash
# Full test suite
cd aos_test_suite
rebar3 eunit

# Specific test modules
rebar3 eunit -m aos_colors_test
rebar3 eunit -m aos_stringify_test
rebar3 eunit -m aos_authorities_test

# With authorities profile
rebar3 as authorities_test eunit

# Lua tests
hype run test.lua
```

### Writing Tests

#### Erlang/EUnit Tests

- Place tests in `aos_test_suite/test/`
- Follow naming pattern: `*_test.erl`
- Use test helpers from `aos_test_helpers.erl`
- Include both positive and negative test cases

Example:
```erlang
-module(my_feature_test).
-include_lib("eunit/include/eunit.hrl").

my_feature_test_() ->
    [
        ?_test(test_basic_functionality()),
        ?_test(test_error_conditions())
    ].

test_basic_functionality() ->
    % Your test implementation
    ?assertEqual(expected, actual).
```

#### Lua Tests

- Place Lua tests in appropriate directories
- Test both normal and edge cases
- Include security validation tests

### Test Coverage Areas

Ensure your contributions include tests for:

- ✅ Security (owner/authorities/trust validation)
- ✅ Message processing and state persistence
- ✅ Error handling and edge cases
- ✅ Performance characteristics
- ✅ Integration scenarios

## Code Style

### Lua Code Style

- Use 2-space indentation
- Follow existing naming conventions
- Add comprehensive comments
- Validate inputs and handle errors gracefully
- Use `_G` namespace for state persistence

Example:
```lua
-- Good: Clear function with validation
function meta.is_owner(msg)
    -- Validate message structure
    if not msg or not msg.commitments then
        return false
    end
    
    -- Implementation...
    return result
end
```

### Erlang Code Style

- Follow OTP conventions
- Use descriptive function and variable names
- Add type specifications where appropriate
- Handle errors with proper error tuples

Example:
```erlang
%% Good: Well-documented function
-spec process_message(map()) -> {ok, term()} | {error, term()}.
process_message(Message) when is_map(Message) ->
    try
        % Process the message
        {ok, Result}
    catch
        _:Error ->
            {error, Error}
    end.
```

### General Guidelines

- **Comments**: Write clear, concise comments explaining the why, not just the what
- **Error Handling**: Always handle edge cases and error conditions
- **Performance**: Consider LUERL-specific optimizations
- **Security**: Validate all inputs and maintain security boundaries

## Documentation

### Code Documentation

- Add docstrings to all public functions
- Include parameter descriptions and return values
- Document security implications
- Provide usage examples

### Project Documentation

- Update README.md for user-facing changes
- Update CLAUDE.md for development guidance
- Create or update architecture diagrams
- Add examples for new features

## Security

Security is paramount in Hyper-AOS. When contributing:

### Security Requirements

1. **Input Validation** - Validate all inputs rigorously
2. **Commitment Verification** - Ensure RSA-PSS-512 validation works correctly
3. **Authority Checks** - Maintain proper authority verification
4. **Sandbox Integrity** - Don't break LUERL sandbox protections
5. **State Isolation** - Maintain proper state boundaries

### Security Review Process

1. Security-related PRs require additional review
2. All security tests must pass
3. Consider attack vectors and edge cases
4. Document security implications of changes

### Reporting Security Issues

For security vulnerabilities, please:
1. **DO NOT** open a public issue
2. Email security concerns privately to maintainers
3. Allow time for investigation and fixes
4. Follow responsible disclosure practices

## Community

### Getting Help

- **GitHub Issues** - For bugs and feature requests
- **Discussions** - For questions and ideas
- **Discord** - Join the Arweave Discord server
- **Documentation** - Check the [AO Cookbook](https://cookbook_ao.ar.io)

### Recognition

Contributors are recognized in:
- GitHub contributors list
- Release notes for significant contributions
- Project README.md acknowledgments

### Communication Guidelines

- Be respectful and inclusive
- Provide constructive feedback
- Help newcomers get started
- Share knowledge and learnings

## Additional Resources

- [CLAUDE.md](CLAUDE.md) - Development notes and guidelines
- [API Documentation](docs/API.md) - Detailed API reference
- [Architecture Overview](docs/ARCHITECTURE.md) - System architecture
- [Security Model](docs/SECURITY.md) - Security implementation details

## Questions?

If you have questions about contributing, please:
1. Check existing documentation
2. Search through issues and discussions
3. Create a discussion post
4. Reach out to maintainers

Thank you for contributing to Hyper-AOS! Together we're building the future of decentralized computing on Arweave.