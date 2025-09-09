.PHONY: test test-verbose all clean

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