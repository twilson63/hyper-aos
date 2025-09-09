.PHONY: test test-verbose all clean build

# Check if hype is installed
HYPE_CHECK := $(shell command -v hype 2> /dev/null)

# Build the aos.lua module using hype
build:
	@echo "Building aos.lua module..."
	@if [ -z "$(HYPE_CHECK)" ]; then \
		echo "Error: hype is not installed."; \
		echo "Please install hype from: https://twilson63.github.io/hype"; \
		exit 1; \
	fi
	@hype run build.lua

# Run all EUnit tests in aos_test_suite
test:
	@echo "Running EUnit tests..."
	@cd aos_test_suite && make eunit && cd -

# Run verbose EUnit tests in aos_test_suite
test-verbose:
	@echo "Running verbose EUnit tests..."
	@cd aos_test_suite && make eunit-verbose && cd -

# Run all tests (alias for test)
all: test

# Clean build artifacts
clean:
	@echo "Cleaning..."
	@cd aos_test_suite && make clean && cd -