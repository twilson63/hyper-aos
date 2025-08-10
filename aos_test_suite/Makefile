.PHONY: all compile test eunit test-old test-require test-math test-outbox clean deps

all: deps compile eunit

deps:
	@echo "Fetching dependencies..."
	@rebar3 get-deps

compile:
	@echo "Compiling..."
	@rebar3 compile

# New EUnit tests (recommended)
eunit: compile
	@echo "Running EUnit tests..."
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

test-utils: compile
	@echo "Running utils module tests..."
	@rebar3 eunit --module=aos_utils_eunit_test

test-handlers-utils: compile
	@echo "Running handlers-utils module tests..."
	@rebar3 eunit --module=aos_handlers_utils_eunit_test

clean:
	@echo "Cleaning..."
	@rebar3 clean
	@rm -rf _build

shell: compile
	@rebar3 shell

# Alias for backward compatibility
test: eunit