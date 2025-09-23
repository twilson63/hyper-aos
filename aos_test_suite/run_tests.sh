#!/bin/bash

# AOS Test Suite Runner
# Comprehensive test runner for the reorganized AOS test suite

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Helper functions
print_banner() {
    echo -e "${CYAN}=================================================${NC}"
    echo -e "${CYAN}$1${NC}"
    echo -e "${CYAN}=================================================${NC}"
}

print_section() {
    echo -e "\n${BLUE}--- $1 ---${NC}"
}

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_error() {
    echo -e "${RED}✗ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠ $1${NC}"
}

# Test categories
run_unit_tests() {
    print_section "Running Unit Tests"
    echo "Unit tests focus on individual components and functions"
    if make test-unit; then
        print_success "Unit tests passed"
        return 0
    else
        print_error "Unit tests failed"
        return 1
    fi
}

run_integration_tests() {
    print_section "Running Integration Tests"
    echo "Integration tests verify end-to-end functionality"
    if make test-integration; then
        print_success "Integration tests passed"
        return 0
    else
        print_error "Integration tests failed"
        return 1
    fi
}

run_security_tests() {
    print_section "Running Security Tests"
    echo "Security tests verify authentication, authorization, and trust mechanisms"
    if make test-security; then
        print_success "Security tests passed"
        return 0
    else
        print_error "Security tests failed"
        return 1
    fi
}

run_legacy_tests() {
    print_section "Running Legacy Tests"
    echo "Legacy tests maintain compatibility with older test formats"
    if make test-legacy; then
        print_success "Legacy tests passed"
        return 0
    else
        print_error "Legacy tests failed"
        return 1
    fi
}

# Show usage information
show_usage() {
    cat << EOF
Usage: $0 [COMMAND]

Commands:
    all                 Run all test categories (default)
    unit               Run unit tests only
    integration        Run integration tests only  
    security           Run security tests only
    legacy             Run legacy tests only
    quick              Run unit and integration tests (skip legacy)
    clean              Clean build artifacts and test results
    deps               Fetch dependencies
    compile            Compile code only
    help               Show this help message

Test Categories:
    unit/              Tests for individual components (colors, stringify, utils)
    integration/       End-to-end functionality tests
    security/          Authentication, authorization, and trust tests  
    legacy/            Older test format compatibility tests

Examples:
    $0                 # Run all tests
    $0 unit            # Run only unit tests
    $0 security        # Run only security tests
    $0 quick           # Run unit + integration (fastest)
    $0 clean           # Clean everything

Test results are saved in test_results/ directory with separate subdirectories
for each test category.
EOF
}

# Parse command line arguments
case "${1:-all}" in
    "all")
        print_banner "AOS Test Suite - Running All Tests"
        
        failed_tests=()
        
        if ! run_unit_tests; then
            failed_tests+=("unit")
        fi
        
        if ! run_integration_tests; then
            failed_tests+=("integration")
        fi
        
        if ! run_security_tests; then
            failed_tests+=("security")
        fi
        
        if ! run_legacy_tests; then
            failed_tests+=("legacy")
        fi
        
        # Summary
        echo -e "\n${CYAN}Test Summary:${NC}"
        if [ ${#failed_tests[@]} -eq 0 ]; then
            print_success "All test categories passed!"
            echo "Test results available in test_results/ directory"
            exit 0
        else
            print_error "Failed test categories: ${failed_tests[*]}"
            echo "Check test_results/ directory for detailed reports"
            exit 1
        fi
        ;;
        
    "unit")
        print_banner "AOS Test Suite - Unit Tests"
        run_unit_tests
        ;;
        
    "integration")
        print_banner "AOS Test Suite - Integration Tests"
        run_integration_tests
        ;;
        
    "security")
        print_banner "AOS Test Suite - Security Tests"
        run_security_tests
        ;;
        
    "legacy")
        print_banner "AOS Test Suite - Legacy Tests"
        run_legacy_tests
        ;;
        
    "quick")
        print_banner "AOS Test Suite - Quick Tests (Unit + Integration)"
        
        failed_tests=()
        
        if ! run_unit_tests; then
            failed_tests+=("unit")
        fi
        
        if ! run_integration_tests; then
            failed_tests+=("integration")
        fi
        
        # Summary
        echo -e "\n${CYAN}Quick Test Summary:${NC}"
        if [ ${#failed_tests[@]} -eq 0 ]; then
            print_success "Quick tests passed!"
            exit 0
        else
            print_error "Failed test categories: ${failed_tests[*]}"
            exit 1
        fi
        ;;
        
    "clean")
        print_banner "AOS Test Suite - Cleaning"
        print_section "Cleaning build artifacts and test results"
        make clean
        print_success "Cleaned successfully"
        ;;
        
    "deps")
        print_banner "AOS Test Suite - Fetching Dependencies"
        make deps
        print_success "Dependencies fetched"
        ;;
        
    "compile")
        print_banner "AOS Test Suite - Compiling"
        make compile
        print_success "Compilation completed"
        ;;
        
    "help"|"-h"|"--help")
        show_usage
        ;;
        
    *)
        print_error "Unknown command: $1"
        echo ""
        show_usage
        exit 1
        ;;
esac