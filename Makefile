# Hyper-AOS Unified Build System
# Master Makefile for building, testing, and deploying Hyper-AOS modules

# =====================================================================
# Configuration
# =====================================================================
SHELL := /bin/bash
.SHELLFLAGS := -o pipefail -c

# Project directories
SRC_DIR := src
DIST_DIR := dist
TEST_DIR := aos_test_suite
DOCS_DIR := docs
DEMOS_DIR := demos
CONFIG_DIR := config
SCRIPTS_DIR := scripts

# Source files
AOS_SRC := $(SRC_DIR)/aos.lua
UTILS_SRC := $(SRC_DIR)/utils.lua
BUILD_OUTPUT := $(DIST_DIR)/aos.lua

# Colors for output (using tput for maximum compatibility)
ifneq ($(shell command -v tput 2>/dev/null),)
    GREEN := $(shell tput setaf 2)
    YELLOW := $(shell tput setaf 3)
    RED := $(shell tput setaf 1)
    BLUE := $(shell tput setaf 4)
    CYAN := $(shell tput setaf 6)
    MAGENTA := $(shell tput setaf 5)
    BOLD := $(shell tput bold)
    NC := $(shell tput sgr0)
else
    # Fallback to ANSI codes
    GREEN := \033[0;32m
    YELLOW := \033[0;33m
    RED := \033[0;31m
    BLUE := \033[0;34m
    CYAN := \033[0;36m
    MAGENTA := \033[0;35m
    BOLD := \033[1m
    NC := \033[0m
endif

# Export for shell commands
export GREEN YELLOW RED BLUE CYAN MAGENTA BOLD NC

# Version information
VERSION := $(shell git describe --tags --always 2>/dev/null || echo "dev")
BUILD_DATE := $(shell date +%Y-%m-%d)
BUILD_TIME := $(shell date +%H:%M:%S)

# =====================================================================
# Phony Targets
# =====================================================================
.PHONY: all help build test deploy clean install-deps status \
        test-unit test-integration test-security test-all test-quick \
        docs lint format check dev watch release info \
        clean-all install-hype install-rebar3 install-arx

# Default target
all: build test

# =====================================================================
# Help Target
# =====================================================================
help:
	@printf "\n"
	@printf "$(CYAN)╔══════════════════════════════════════════════════════════════╗$(NC)\n"
	@printf "$(CYAN)║$(NC)  $(GREEN)Hyper-AOS Build System$(NC) - Version $(YELLOW)$(VERSION)$(NC)\n"
	@printf "$(CYAN)╚══════════════════════════════════════════════════════════════╝$(NC)\n"
	@printf "\n"
	@printf "$(BLUE)▶ Primary Commands:$(NC)\n"
	@printf "  $(YELLOW)make build$(NC)         Build concatenated Lua module\n"
	@printf "  $(YELLOW)make test$(NC)          Run all tests (unit + integration)\n"
	@printf "  $(YELLOW)make deploy$(NC)        Deploy to Arweave network\n"
	@printf "  $(YELLOW)make clean$(NC)         Remove all build artifacts\n"
	@printf "\n"
	@printf "$(BLUE)▶ Testing Commands:$(NC)\n"
	@printf "  $(YELLOW)make test-quick$(NC)    Run quick smoke tests\n"
	@printf "  $(YELLOW)make test-unit$(NC)     Run unit tests only\n"
	@printf "  $(YELLOW)make test-integration$(NC) Run integration tests\n"
	@printf "  $(YELLOW)make test-security$(NC) Run security tests\n"
	@printf "  $(YELLOW)make test-coverage$(NC) Generate test coverage report\n"
	@printf "\n"
	@printf "$(BLUE)▶ Development Commands:$(NC)\n"
	@printf "  $(YELLOW)make dev$(NC)           Development build with hot reload\n"
	@printf "  $(YELLOW)make watch$(NC)         Watch files for changes\n"
	@printf "  $(YELLOW)make lint$(NC)          Run code linters\n"
	@printf "  $(YELLOW)make format$(NC)        Format code files\n"
	@printf "  $(YELLOW)make check$(NC)         Run all checks (lint + test)\n"
	@printf "\n"
	@printf "$(BLUE)▶ Documentation Commands:$(NC)\n"
	@printf "  $(YELLOW)make docs$(NC)          Generate documentation\n"
	@printf "  $(YELLOW)make docs-serve$(NC)    Serve documentation locally\n"
	@printf "\n"
	@printf "$(BLUE)▶ Utility Commands:$(NC)\n"
	@printf "  $(YELLOW)make install-deps$(NC)  Install all dependencies\n"
	@printf "  $(YELLOW)make status$(NC)        Show project status\n"
	@printf "  $(YELLOW)make info$(NC)          Show build information\n"
	@printf "  $(YELLOW)make release$(NC)       Create release package\n"
	@printf "  $(YELLOW)make clean-all$(NC)     Deep clean (including deps)\n"
	@printf "\n"
	@printf "$(GREEN)Quick Start:$(NC) make install-deps && make build && make test\n"
	@printf "\n"

# =====================================================================
# Installation Targets
# =====================================================================

# Install all dependencies
install-deps: install-hype install-rebar3 install-arx
	@echo "$(GREEN)✓ All dependencies installed$(NC)"

# Install Hype framework
install-hype:
	@echo "$(YELLOW)Checking for Hype framework...$(NC)"
	@if command -v hype >/dev/null 2>&1; then \
		echo "$(GREEN)✓ Hype is already installed$(NC)"; \
	else \
		echo "$(YELLOW)Installing Hype framework...$(NC)"; \
		if [[ "$$(uname)" == "Darwin" ]]; then \
			curl -sSL https://raw.githubusercontent.com/twilson63/hype/main/install-mac.sh | bash || \
			npm install -g @twilson63/hype; \
		else \
			curl -sSL https://raw.githubusercontent.com/twilson63/hype/main/install.sh | bash || \
			npm install -g @twilson63/hype; \
		fi; \
		if command -v hype >/dev/null 2>&1; then \
			echo "$(GREEN)✓ Hype installed successfully$(NC)"; \
		else \
			echo "$(RED)✗ Failed to install Hype$(NC)"; \
			exit 1; \
		fi; \
	fi

# Install Rebar3 for Erlang tests
install-rebar3:
	@echo "$(YELLOW)Checking for Rebar3...$(NC)"
	@if command -v rebar3 >/dev/null 2>&1; then \
		echo "$(GREEN)✓ Rebar3 is already installed$(NC)"; \
	else \
		echo "$(YELLOW)Installing Rebar3...$(NC)"; \
		if [[ "$$(uname)" == "Darwin" ]]; then \
			brew install rebar3 2>/dev/null || \
			(wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3 && sudo mv rebar3 /usr/local/bin/); \
		else \
			wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3 && sudo mv rebar3 /usr/local/bin/; \
		fi; \
	fi

# Install ARX for Arweave deployment
install-arx:
	@echo "$(YELLOW)Checking for ARX...$(NC)"
	@if command -v arx >/dev/null 2>&1; then \
		echo "$(GREEN)✓ ARX is already installed$(NC)"; \
	else \
		echo "$(YELLOW)Installing ARX...$(NC)"; \
		npm install -g @permaweb/arx || \
		(echo "$(RED)Failed to install ARX$(NC)" && exit 1); \
	fi

# =====================================================================
# Build Targets
# =====================================================================

# Create build directory
$(DIST_DIR):
	@mkdir -p $(DIST_DIR)

# Build concatenated Lua file
build: $(DIST_DIR)
	@echo "$(GREEN)════════════════════════════════════════════════════════$(NC)"
	@echo "$(GREEN)Building Hyper-AOS Module$(NC)"
	@echo "$(GREEN)════════════════════════════════════════════════════════$(NC)"
	@echo "$(BLUE)▶ Checking source files...$(NC)"
	@if [ ! -f $(AOS_SRC) ]; then \
		echo "$(RED)Error: $(AOS_SRC) not found$(NC)"; \
		exit 1; \
	fi
	@if [ ! -f $(UTILS_SRC) ]; then \
		echo "$(RED)Error: $(UTILS_SRC) not found$(NC)"; \
		exit 1; \
	fi
	@echo "$(GREEN)  ✓ aos.lua found$(NC)"
	@echo "$(GREEN)  ✓ utils.lua found$(NC)"
	@echo ""
	@echo "$(BLUE)▶ Building concatenated module...$(NC)"
	@if [ -f $(SCRIPTS_DIR)/build.lua ]; then \
		lua $(SCRIPTS_DIR)/build.lua || hype run $(SCRIPTS_DIR)/build.lua 2>/dev/null; \
	else \
		echo "-- Hyper-AOS Concatenated Module" > $(BUILD_OUTPUT); \
		echo "-- Version: $(VERSION)" >> $(BUILD_OUTPUT); \
		echo "-- Built: $(BUILD_DATE) $(BUILD_TIME)" >> $(BUILD_OUTPUT); \
		echo "" >> $(BUILD_OUTPUT); \
		cat $(UTILS_SRC) >> $(BUILD_OUTPUT); \
		echo "" >> $(BUILD_OUTPUT); \
		cat $(AOS_SRC) >> $(BUILD_OUTPUT); \
	fi
	@echo "$(GREEN)  ✓ Module concatenated$(NC)"
	@echo ""
	@echo "$(BLUE)▶ Build Summary:$(NC)"
	@echo "  Output: $(BUILD_OUTPUT)"
	@echo "  Size: $$(ls -lh $(BUILD_OUTPUT) | awk '{print $$5}')"
	@echo "  Lines: $$(wc -l < $(BUILD_OUTPUT))"
	@echo ""
	@echo "$(GREEN)✓ Build completed successfully!$(NC)"

# Development build
dev: build
	@echo "$(GREEN)Development build complete$(NC)"
	@echo "$(YELLOW)Starting development mode...$(NC)"
	@$(MAKE) watch

# Watch for changes
watch:
	@if command -v fswatch >/dev/null 2>&1; then \
		echo "$(GREEN)Watching for changes... (Ctrl+C to stop)$(NC)"; \
		fswatch -o $(SRC_DIR)/*.lua | xargs -n1 -I{} $(MAKE) build; \
	else \
		echo "$(RED)fswatch not found. Install with: brew install fswatch$(NC)"; \
		exit 1; \
	fi

# =====================================================================
# Testing Targets
# =====================================================================

# Run all tests
test: test-lua test-erlang
	@echo ""
	@echo "$(GREEN)════════════════════════════════════════════════════════$(NC)"
	@echo "$(GREEN)✓ All tests completed successfully!$(NC)"
	@echo "$(GREEN)════════════════════════════════════════════════════════$(NC)"

# Quick smoke tests
test-quick: build
	@echo "$(BLUE)Running quick smoke tests...$(NC)"
	@lua -c $(BUILD_OUTPUT) 2>/dev/null && echo "$(GREEN)✓ Lua syntax valid$(NC)" || echo "$(RED)✗ Lua syntax error$(NC)"
	@grep -q "_G.package.loaded" $(BUILD_OUTPUT) && echo "$(GREEN)✓ Module registration found$(NC)" || echo "$(RED)✗ Module registration missing$(NC)"
	@echo "$(GREEN)✓ Quick tests passed$(NC)"

# Run Lua tests
test-lua: build
	@echo "$(BLUE)Running Lua tests...$(NC)"
	@if [ -f $(SCRIPTS_DIR)/test.lua ]; then \
		lua $(SCRIPTS_DIR)/test.lua || hype run $(SCRIPTS_DIR)/test.lua 2>/dev/null; \
	else \
		$(MAKE) test-quick; \
	fi

# Run Erlang/EUnit tests
test-erlang:
	@echo "$(BLUE)Running Erlang test suite...$(NC)"
	@if [ -d $(TEST_DIR) ] && command -v rebar3 >/dev/null 2>&1; then \
		cd $(TEST_DIR) && $(MAKE) test; \
	else \
		echo "$(YELLOW)Skipping Erlang tests (rebar3 not found or test suite missing)$(NC)"; \
	fi

# Run unit tests
test-unit:
	@echo "$(BLUE)Running unit tests...$(NC)"
	@if [ -x $(TEST_DIR)/run_tests.sh ]; then \
		cd $(TEST_DIR) && ./run_tests.sh unit; \
	elif [ -d $(TEST_DIR) ]; then \
		cd $(TEST_DIR) && rebar3 eunit; \
	else \
		echo "$(RED)Test suite not found$(NC)"; \
	fi

# Run integration tests
test-integration:
	@echo "$(BLUE)Running integration tests...$(NC)"
	@if [ -x $(TEST_DIR)/run_tests.sh ]; then \
		cd $(TEST_DIR) && ./run_tests.sh integration; \
	else \
		echo "$(RED)Test runner not found$(NC)"; \
	fi

# Run security tests
test-security:
	@echo "$(BLUE)Running security tests...$(NC)"
	@if [ -x $(TEST_DIR)/run_tests.sh ]; then \
		cd $(TEST_DIR) && ./run_tests.sh security; \
	else \
		echo "$(RED)Test runner not found$(NC)"; \
	fi

# Generate test coverage
test-coverage:
	@echo "$(BLUE)Generating test coverage report...$(NC)"
	@if [ -d $(TEST_DIR) ] && command -v rebar3 >/dev/null 2>&1; then \
		cd $(TEST_DIR) && rebar3 cover; \
	else \
		echo "$(RED)Rebar3 or test suite not found$(NC)"; \
	fi

# =====================================================================
# Deployment Targets
# =====================================================================

# Deploy to Arweave
deploy: build
	@echo "$(GREEN)════════════════════════════════════════════════════════$(NC)"
	@echo "$(GREEN)Deploying to Arweave$(NC)"
	@echo "$(GREEN)════════════════════════════════════════════════════════$(NC)"
	@if [ -f $(SCRIPTS_DIR)/deploy.lua ]; then \
		hype run $(SCRIPTS_DIR)/deploy.lua 2>/dev/null || lua $(SCRIPTS_DIR)/deploy.lua; \
	else \
		$(MAKE) deploy-arx; \
	fi

# Deploy with ARX
deploy-arx: build
	@echo "$(BLUE)Deploying with ARX...$(NC)"
	@if [ -z "$${WALLET_PATH}" ]; then \
		if [ -f $(CONFIG_DIR)/wallet.json ]; then \
			export WALLET_PATH=$(CONFIG_DIR)/wallet.json; \
		else \
			echo "$(RED)Error: WALLET_PATH not set and no wallet found$(NC)"; \
			echo "Run: export WALLET_PATH=/path/to/wallet.json"; \
			exit 1; \
		fi; \
	fi; \
	arx upload $(BUILD_OUTPUT) -w "$${WALLET_PATH}" -t arweave \
		--content-type application/lua \
		--tags Data-Protocol ao \
		--tags Module-Format concatenated \
		--tags App-Name HyperAOS \
		--tags Version $(VERSION) \
		--tags Build-Date $(BUILD_DATE)

# =====================================================================
# Code Quality Targets
# =====================================================================

# Run linters
lint:
	@echo "$(BLUE)Running code linters...$(NC)"
	@if command -v luacheck >/dev/null 2>&1; then \
		luacheck $(SRC_DIR)/*.lua --globals _G meta utils; \
	else \
		echo "$(YELLOW)luacheck not installed, skipping Lua linting$(NC)"; \
		echo "Install with: luarocks install luacheck"; \
	fi

# Format code
format:
	@echo "$(BLUE)Formatting code...$(NC)"
	@if command -v lua-format >/dev/null 2>&1; then \
		lua-format -i $(SRC_DIR)/*.lua; \
		echo "$(GREEN)✓ Lua files formatted$(NC)"; \
	else \
		echo "$(YELLOW)lua-format not installed$(NC)"; \
		echo "Install with: luarocks install --server=https://luarocks.org/dev luaformatter"; \
	fi

# Run all checks
check: lint test
	@echo "$(GREEN)✓ All checks passed$(NC)"

# =====================================================================
# Documentation Targets
# =====================================================================

# Generate documentation
docs:
	@echo "$(BLUE)Generating documentation...$(NC)"
	@echo "$(GREEN)✓ Documentation available in $(DOCS_DIR)/$(NC)"
	@echo "  - ARCHITECTURE.md: System design"
	@echo "  - API.md: API reference"
	@echo "  - DEVELOPMENT.md: Developer guide"
	@echo "  - DEPLOYMENT.md: Deployment guide"
	@echo "  - SECURITY.md: Security documentation"

# Serve documentation
docs-serve:
	@echo "$(BLUE)Serving documentation on http://localhost:8000$(NC)"
	@cd $(DOCS_DIR) && python3 -m http.server 8000 2>/dev/null || python -m SimpleHTTPServer 8000

# =====================================================================
# Release Targets
# =====================================================================

# Create release package
release: clean build test
	@echo "$(BLUE)Creating release package...$(NC)"
	@mkdir -p releases
	@tar -czf releases/hyper-aos-$(VERSION).tar.gz \
		$(BUILD_OUTPUT) \
		$(SRC_DIR) \
		$(DOCS_DIR) \
		$(DEMOS_DIR) \
		README.md \
		CHANGELOG.md \
		LICENSE 2>/dev/null || true
	@echo "$(GREEN)✓ Release created: releases/hyper-aos-$(VERSION).tar.gz$(NC)"

# =====================================================================
# Cleanup Targets
# =====================================================================

# Clean build artifacts
clean:
	@echo "$(YELLOW)Cleaning build artifacts...$(NC)"
	@rm -rf $(DIST_DIR)
	@rm -rf $(TEST_DIR)/_build
	@rm -f $(TEST_DIR)/rebar3.crashdump
	@rm -f $(TEST_DIR)/src/*.beam
	@find . -name "*.beam" -type f -delete 2>/dev/null || true
	@find . -name "erl_crash.dump" -type f -delete 2>/dev/null || true
	@find . -name "*.crashdump" -type f -delete 2>/dev/null || true
	@echo "$(GREEN)✓ Clean complete$(NC)"

# Deep clean
clean-all: clean
	@echo "$(YELLOW)Deep cleaning all generated files...$(NC)"
	@rm -rf .hype/
	@rm -rf $(TEST_DIR)/_checkouts
	@rm -rf $(TEST_DIR)/_vendor
	@rm -rf releases/
	@rm -rf coverage/
	@echo "$(GREEN)✓ Deep clean complete$(NC)"

# =====================================================================
# Utility Targets
# =====================================================================

# Show project status
status:
	@echo ""
	@echo "$(CYAN)╔══════════════════════════════════════════════════════════════╗$(NC)"
	@echo "$(CYAN)║$(NC)  $(GREEN)Hyper-AOS Project Status$(NC)"
	@echo "$(CYAN)╚══════════════════════════════════════════════════════════════╝$(NC)"
	@echo ""
	@echo "$(YELLOW)▶ Source Files:$(NC)"
	@ls -la $(SRC_DIR)/*.lua 2>/dev/null || echo "  No source files found"
	@echo ""
	@echo "$(YELLOW)▶ Build Artifacts:$(NC)"
	@ls -la $(DIST_DIR)/*.lua 2>/dev/null || echo "  No build artifacts"
	@echo ""
	@echo "$(YELLOW)▶ Configuration:$(NC)"
	@ls -la $(CONFIG_DIR)/*.json 2>/dev/null | head -3 || echo "  No config files"
	@echo ""
	@echo "$(YELLOW)▶ Tools Status:$(NC)"
	@command -v hype >/dev/null 2>&1 && echo "  $(GREEN)✓$(NC) Hype installed" || echo "  $(RED)✗$(NC) Hype not installed"
	@command -v lua >/dev/null 2>&1 && echo "  $(GREEN)✓$(NC) Lua installed" || echo "  $(RED)✗$(NC) Lua not installed"
	@command -v arx >/dev/null 2>&1 && echo "  $(GREEN)✓$(NC) ARX installed" || echo "  $(RED)✗$(NC) ARX not installed"
	@command -v rebar3 >/dev/null 2>&1 && echo "  $(GREEN)✓$(NC) Rebar3 installed" || echo "  $(RED)✗$(NC) Rebar3 not installed"
	@command -v luacheck >/dev/null 2>&1 && echo "  $(GREEN)✓$(NC) Luacheck installed" || echo "  $(RED)✗$(NC) Luacheck not installed"
	@echo ""
	@echo "$(YELLOW)▶ Repository Statistics:$(NC)"
	@echo "  Lua files: $$(find $(SRC_DIR) -name '*.lua' 2>/dev/null | wc -l)"
	@echo "  Test files: $$(find $(TEST_DIR)/test -name '*.erl' 2>/dev/null | wc -l || echo 0)"
	@echo "  Documentation: $$(find $(DOCS_DIR) -name '*.md' 2>/dev/null | wc -l) files"
	@echo "  Total LOC: $$(find $(SRC_DIR) -name '*.lua' -exec wc -l {} + 2>/dev/null | tail -1 | awk '{print $$1}' || echo 0)"
	@echo ""

# Show build information
info:
	@echo "$(MAGENTA)Build Information:$(NC)"
	@echo "  Version: $(VERSION)"
	@echo "  Date: $(BUILD_DATE)"
	@echo "  Time: $(BUILD_TIME)"
	@echo "  System: $$(uname -s) $$(uname -r)"
	@echo "  Lua: $$(lua -v 2>&1 | head -1)"

# =====================================================================
# Special Targets
# =====================================================================

# Initialize project (first-time setup)
init: install-deps
	@echo "$(GREEN)Initializing Hyper-AOS project...$(NC)"
	@mkdir -p $(SRC_DIR) $(DIST_DIR) $(DOCS_DIR) $(DEMOS_DIR) $(CONFIG_DIR) $(SCRIPTS_DIR)
	@echo "$(GREEN)✓ Project initialized$(NC)"
	@$(MAKE) status

# Validate project structure
validate:
	@echo "$(BLUE)Validating project structure...$(NC)"
	@test -d $(SRC_DIR) && echo "$(GREEN)✓ src/ directory exists$(NC)" || echo "$(RED)✗ src/ directory missing$(NC)"
	@test -f $(AOS_SRC) && echo "$(GREEN)✓ aos.lua exists$(NC)" || echo "$(RED)✗ aos.lua missing$(NC)"
	@test -f $(UTILS_SRC) && echo "$(GREEN)✓ utils.lua exists$(NC)" || echo "$(RED)✗ utils.lua missing$(NC)"
	@test -d $(TEST_DIR) && echo "$(GREEN)✓ Test suite exists$(NC)" || echo "$(RED)✗ Test suite missing$(NC)"
	@test -d $(DOCS_DIR) && echo "$(GREEN)✓ Documentation exists$(NC)" || echo "$(RED)✗ Documentation missing$(NC)"

# =====================================================================
# End of Makefile
# =====================================================================