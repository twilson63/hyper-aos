.PHONY: all compile test eunit test-unit test-integration test-security test-legacy test-authorities test-all test-old test-require test-math test-outbox clean deps shell

all: deps compile test-all

deps:
	@echo "Fetching dependencies..."
	@rebar3 get-deps

compile:
	@echo "Compiling..."
	@rebar3 compile

# Test categories
test-unit: compile
	@echo "Running unit tests..."
	@mkdir -p test_results/unit
	@rebar3 as unit_test eunit

test-integration: compile
	@echo "Running integration tests..."
	@mkdir -p test_results/integration
	@rebar3 as integration_test eunit

test-security: compile
	@echo "Running security tests..."
	@mkdir -p test_results/security
	@rebar3 as security_test eunit

test-legacy: compile
	@echo "Running legacy tests..."
	@mkdir -p test_results/legacy
	@rebar3 as legacy_test eunit

# Authorities tests (backward compatibility)
test-authorities: test-security

# Run all test categories
test-all: test-unit test-integration test-security test-legacy
	@echo "All test categories completed"

# Standard eunit (runs all tests)
eunit: compile
	@echo "Running all EUnit tests..."
	@mkdir -p test_results
	@rebar3 eunit

# Old manual tests (kept for reference)
test-old: compile
	@echo "Running old AOS sandbox tests..."
	@rebar3 shell --eval "aos_sandbox_test:run_tests(), init:stop()."

test-require: compile
	@echo "Running require('.process')._version test..."
	@rebar3 shell --eval "aos_require_test:test_require_process_version(), init:stop()."

test-math: compile
	@echo "Running math operations tests..."
	@rebar3 shell --eval "aos_math_test:test_math_operations(), init:stop()."

test-outbox: compile
	@echo "Running outbox/send functionality tests..."
	@rebar3 shell --eval "aos_outbox_test:test_outbox_functionality(), init:stop()."

clean:
	@echo "Cleaning..."
	@rebar3 clean
	@rm -rf _build test_results

shell: compile
	@rebar3 shell

# Aliases for backward compatibility
test: test-all