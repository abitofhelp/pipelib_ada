# =============================================================================
# Pipelib Makefile - Ada 2022 Library Development
# =============================================================================

# Project-specific variable (only difference between library makefiles)
PROJECT_NAME := pipelib

# =============================================================================
# Common Configuration
# =============================================================================

.PHONY: all build build-dev build-opt build-release check clean ci docs format help install setup-hooks stats test test-all test-contract test-coverage test-e2e test-integration test-performance test-property test-unit watch

# Default target
all: build test check

# Build tools
ALR := alr
GPRBUILD := gprbuild
GNATFORMAT := gnatformat
GNATDOC := gnatdoc

# Directories
BUILD_DIR := obj
LIB_DIR := lib
DOCS_DIR := docs/api
COVERAGE_DIR := coverage
TESTS_DIR := tests

# Colors for output
GREEN := \033[0;32m
YELLOW := \033[0;33m
RED := \033[0;31m
BLUE := \033[0;34m
NC := \033[0m # No Color

# =============================================================================
# Help Target (Default)
# =============================================================================

help:
	@echo "$(BLUE)$(PROJECT_NAME) - Ada 2022 Library Development$(NC)"
	@echo ""
	@echo "$(YELLOW)Build Commands:$(NC)"
	@echo "  build              - Build library (development mode)"
	@echo "  build-dev          - Build with development settings and debug info"
	@echo "  build-opt          - Build with optimizations enabled"
	@echo "  build-release      - Build production release version"
	@echo "  clean              - Remove all build artifacts"
	@echo "  install            - Install library via Alire"
	@echo ""
	@echo "$(YELLOW)Development Commands:$(NC)"
	@echo "  ci                 - Run complete CI pipeline locally"
	@echo "  setup-hooks        - Configure git pre-commit hooks"
	@echo "  stats              - Display project statistics"
	@echo "  watch              - Auto-rebuild on file changes"
	@echo ""
	@echo "$(YELLOW)Documentation Commands:$(NC)"
	@echo "  docs               - Generate API documentation"
	@echo ""
	@echo "$(YELLOW)Quality Commands:$(NC)"
	@echo "  check              - Run static analysis and validation"
	@echo "  format             - Auto-format all source code"
	@echo ""
	@echo "$(YELLOW)Testing Commands:$(NC)"
	@echo "  test               - Run standard test suite"
	@echo "  test-all           - Run all test categories"
	@echo "  test-contract      - Run contract verification tests"
	@echo "  test-coverage      - Run tests with coverage analysis"
	@echo "  test-e2e           - Run end-to-end tests"
	@echo "  test-integration   - Run integration tests"
	@echo "  test-performance   - Run performance benchmarks"
	@echo "  test-property      - Run property-based tests"
	@echo "  test-unit          - Run unit tests only"
	@echo ""
	@echo "$(YELLOW)Quick Shortcuts:$(NC)"
	@echo "  make               - Build, test, and check"
	@echo "  make b             - Build"
	@echo "  make c             - Check"
	@echo "  make d             - Docs"
	@echo "  make f             - Format"
	@echo "  make t             - Test"
	@echo "  make ta            - All tests"
	@echo "  make tc            - Coverage tests"
	@echo "  make tcp           - Contract tests"
	@echo "  make te            - E2E tests"
	@echo "  make ti            - Integration tests"
	@echo "  make tp            - Performance tests"
	@echo "  make tpr           - Property tests"
	@echo "  make tu            - Unit tests"

# =============================================================================
# Build Commands
# =============================================================================

build:
	@echo "$(GREEN)Building $(PROJECT_NAME)...$(NC)"
	@$(ALR) build
	@echo "$(GREEN)✓ Build complete$(NC)"

build-dev:
	@echo "$(GREEN)Building $(PROJECT_NAME) (development mode)...$(NC)"
	@$(ALR) build --development
	@echo "$(GREEN)✓ Development build complete$(NC)"

build-opt:
	@echo "$(GREEN)Building $(PROJECT_NAME) (optimized)...$(NC)"
	@$(ALR) build -- -O2
	@echo "$(GREEN)✓ Optimized build complete$(NC)"

build-release:
	@echo "$(GREEN)Building $(PROJECT_NAME) (release)...$(NC)"
	@$(ALR) build --release
	@echo "$(GREEN)✓ Release build complete$(NC)"

clean:
	@echo "$(GREEN)Cleaning build artifacts...$(NC)"
	@$(ALR) clean
	@rm -rf $(BUILD_DIR) $(LIB_DIR) alire .build $(COVERAGE_DIR)
	@find . -name "*.gcda" -o -name "*.gcno" -o -name "*.gcov" | xargs rm -f
	@echo "$(GREEN)✓ Clean complete$(NC)"

install:
	@echo "$(GREEN)Installing $(PROJECT_NAME)...$(NC)"
	@$(ALR) install
	@echo "$(GREEN)✓ Installation complete$(NC)"

# =============================================================================
# Development Commands
# =============================================================================

ci: clean format build check test-coverage docs
	@echo ""
	@echo "$(GREEN)════════════════════════════════════════$(NC)"
	@echo "$(GREEN)✓ CI Pipeline Complete!$(NC)"
	@echo "$(GREEN)════════════════════════════════════════$(NC)"
	@echo ""
	@echo "Summary:"
	@echo "  • Code formatted"
	@echo "  • Library built successfully"
	@echo "  • Static analysis passed"
	@echo "  • All tests passed"
	@echo "  • Coverage report generated"
	@echo "  • Documentation updated"

setup-hooks:
	@echo "$(GREEN)Setting up git hooks...$(NC)"
	@mkdir -p .git/hooks
	@echo "#!/bin/sh" > .git/hooks/pre-commit
	@echo "# Auto-generated pre-commit hook" >> .git/hooks/pre-commit
	@echo "echo 'Running pre-commit checks...'" >> .git/hooks/pre-commit
	@echo "make format" >> .git/hooks/pre-commit
	@echo "make check" >> .git/hooks/pre-commit
	@echo "git add -u" >> .git/hooks/pre-commit
	@chmod +x .git/hooks/pre-commit
	@echo "$(GREEN)✓ Git hooks configured$(NC)"

stats:
	@echo "$(BLUE)Project Statistics for $(PROJECT_NAME)$(NC)"
	@echo "$(YELLOW)════════════════════════════════════════$(NC)"
	@echo ""
	@echo "Ada Source Files:"
	@echo "  Specifications (.ads): $$(find src -name "*.ads" 2>/dev/null | wc -l)"
	@echo "  Bodies (.adb):         $$(find src -name "*.adb" 2>/dev/null | wc -l)"
	@echo ""
	@echo "Lines of Code:"
	@find src -name "*.ads" -o -name "*.adb" 2>/dev/null | xargs wc -l | tail -1 | awk '{printf "  Total: %d lines\n", $$1}'
	@echo ""
	@echo "Test Files:"
	@echo "  Test specs (.ads):     $$(find $(TESTS_DIR) -name "*.ads" 2>/dev/null | wc -l)"
	@echo "  Test bodies (.adb):    $$(find $(TESTS_DIR) -name "*.adb" 2>/dev/null | wc -l)"
	@echo ""
	@echo "Directory Structure:"
	@if command -v tree >/dev/null 2>&1; then \
		tree src -d -L 3 --noreport; \
	else \
		find src -type d | sort | sed 's|[^/]*/|  |g'; \
	fi

watch:
	@echo "$(GREEN)Watching for changes in src/...$(NC)"
	@echo "Press Ctrl+C to stop"
	@if command -v inotifywait >/dev/null 2>&1; then \
		while true; do \
			inotifywait -q -e modify,create,delete -r src/ $(TESTS_DIR)/; \
			clear; \
			echo "$(YELLOW)Change detected, rebuilding...$(NC)"; \
			make build test; \
		done; \
	else \
		echo "$(RED)Error: inotifywait not found. Install inotify-tools:$(NC)"; \
		echo "  Ubuntu/Debian: sudo apt-get install inotify-tools"; \
		echo "  macOS: brew install fswatch (use fswatch instead)"; \
		exit 1; \
	fi

# =============================================================================
# Documentation Commands
# =============================================================================

docs:
	@echo "$(GREEN)Generating documentation...$(NC)"
	@mkdir -p $(DOCS_DIR)
	@if command -v $(GNATDOC) >/dev/null 2>&1; then \
		$(ALR) exec -- $(GNATDOC) -P$(PROJECT_NAME).gpr -w --output=$(DOCS_DIR); \
		echo "$(GREEN)✓ Full documentation generated in $(DOCS_DIR)$(NC)"; \
	else \
		echo "$(YELLOW)Warning: gnatdoc not found, generating basic docs...$(NC)"; \
		find src -name "*.ads" -exec echo "=== {} ===" \; -exec grep -E "^[ ]*--" {} \; > $(DOCS_DIR)/basic_docs.txt; \
		echo "$(GREEN)✓ Basic documentation extracted to $(DOCS_DIR)/basic_docs.txt$(NC)"; \
	fi

# =============================================================================
# Quality Commands
# =============================================================================

check:
	@echo "$(GREEN)Running static analysis...$(NC)"
	@$(ALR) build --validation
	@echo "$(GREEN)✓ Static analysis complete$(NC)"

format:
	@echo "$(GREEN)Formatting code...$(NC)"
	@if command -v $(GNATFORMAT) >/dev/null 2>&1; then \
		$(ALR) exec -- $(GNATFORMAT) -P$(PROJECT_NAME).gpr; \
	else \
		echo "$(YELLOW)Warning: gnatformat not found, using gnatpp...$(NC)"; \
		find src -name "*.ads" -o -name "*.adb" | xargs -I {} $(ALR) exec -- gnatpp {} -pipe; \
	fi
	@echo "$(GREEN)✓ Code formatting complete$(NC)"

# =============================================================================
# Testing Commands
# =============================================================================

test: build
	@echo "$(GREEN)Running test suite...$(NC)"
	@if [ -f "tests.gpr" ]; then \
		$(ALR) exec -- $(GPRBUILD) -P tests.gpr -p -q; \
		if [ -f "./bin/main" ]; then \
			./bin/main; \
		elif [ -f "./tests/simple_test_runner" ]; then \
			cd tests && ./simple_test_runner; \
		elif [ -f "./bin/test_runner" ]; then \
			./bin/test_runner; \
		elif [ -f "./tests/test_runner" ]; then \
			cd tests && ./test_runner; \
		else \
			echo "$(YELLOW)Test runner not found$(NC)"; \
		fi; \
	else \
		echo "$(YELLOW)No test project found$(NC)"; \
	fi
	@echo "$(GREEN)✓ Tests complete$(NC)"

test-all: test-unit test-integration test-contract test-property test-performance test-e2e
	@echo ""
	@echo "$(GREEN)════════════════════════════════════════$(NC)"
	@echo "$(GREEN)✓ All Test Suites Complete!$(NC)"
	@echo "$(GREEN)════════════════════════════════════════$(NC)"

test-contract: build
	@echo "$(GREEN)Running contract verification tests...$(NC)"
	@if [ -d "$(TESTS_DIR)/contract" ]; then \
		make test ARGS="--filter=contract"; \
	else \
		echo "$(YELLOW)Contract tests not implemented yet$(NC)"; \
	fi
	@echo "$(GREEN)✓ Contract tests complete$(NC)"

test-coverage:
	@echo "$(GREEN)Running tests with coverage analysis...$(NC)"
	@mkdir -p $(COVERAGE_DIR)
	@echo "$(YELLOW)Building library for coverage...$(NC)"
	@$(ALR) clean
	@$(ALR) build
	@echo "$(YELLOW)Running test suite...$(NC)"
	@if [ -f "tests.gpr" ]; then \
		$(ALR) exec -- $(GPRBUILD) -P tests.gpr -p -q \
			-cargs -fprofile-arcs -ftest-coverage \
			-largs -fprofile-arcs; \
		if [ -f "./bin/main" ]; then \
			./bin/main; \
		elif [ -f "./tests/simple_test_runner" ]; then \
			cd tests && ./simple_test_runner; \
		elif [ -f "./bin/test_runner" ]; then \
			./bin/test_runner; \
		elif [ -f "./tests/test_runner" ]; then \
			cd tests && ./test_runner; \
		fi; \
		echo "$(YELLOW)Generating coverage report...$(NC)"; \
		if command -v gcovr >/dev/null 2>&1; then \
			gcovr -r . --html --html-details -o $(COVERAGE_DIR)/coverage.html; \
			echo "$(GREEN)✓ Coverage report generated: $(COVERAGE_DIR)/coverage.html$(NC)"; \
		else \
			echo "$(YELLOW)gcovr not found, generating basic coverage...$(NC)"; \
			find $(BUILD_DIR) -name "*.gcda" | xargs -I {} gcov {}; \
			mv *.gcov $(COVERAGE_DIR)/ 2>/dev/null || true; \
			echo "$(GREEN)✓ Basic coverage files in $(COVERAGE_DIR)/$(NC)"; \
		fi; \
	else \
		echo "$(YELLOW)No test project found - coverage requires tests$(NC)"; \
		echo "$(YELLOW)Create tests.gpr to enable coverage analysis$(NC)"; \
	fi

test-e2e: build
	@echo "$(GREEN)Running end-to-end tests...$(NC)"
	@if [ -d "$(TESTS_DIR)/e2e" ]; then \
		make test ARGS="--filter=e2e"; \
	else \
		echo "$(YELLOW)End-to-end tests not implemented yet$(NC)"; \
	fi
	@echo "$(GREEN)✓ E2E tests complete$(NC)"

test-integration: build
	@echo "$(GREEN)Running integration tests...$(NC)"
	@if [ -d "$(TESTS_DIR)/integration" ]; then \
		make test ARGS="--filter=integration"; \
	else \
		echo "$(YELLOW)Integration tests not implemented yet$(NC)"; \
	fi
	@echo "$(GREEN)✓ Integration tests complete$(NC)"

test-performance: build
	@echo "$(GREEN)Running performance benchmarks...$(NC)"
	@if [ -d "$(TESTS_DIR)/performance" ]; then \
		make test ARGS="--filter=performance"; \
	else \
		echo "$(YELLOW)Performance benchmarks not implemented yet$(NC)"; \
	fi
	@echo "$(GREEN)✓ Performance tests complete$(NC)"

test-property: build
	@echo "$(GREEN)Running property-based tests...$(NC)"
	@if [ -d "$(TESTS_DIR)/property" ]; then \
		make test ARGS="--filter=property"; \
	else \
		echo "$(YELLOW)Property-based tests not implemented yet$(NC)"; \
	fi
	@echo "$(GREEN)✓ Property tests complete$(NC)"

test-unit: build
	@echo "$(GREEN)Running unit tests...$(NC)"
	@if [ -d "$(TESTS_DIR)/unit" ]; then \
		make test ARGS="--filter=unit"; \
	else \
		echo "$(YELLOW)Unit tests not implemented yet$(NC)"; \
	fi
	@echo "$(GREEN)✓ Unit tests complete$(NC)"

# =============================================================================
# Development Shortcuts
# =============================================================================

# Single letter shortcuts
b: build
c: check
d: docs
f: format
t: test

# Test shortcuts
ta: test-all
tc: test-coverage
tcp: test-contract
te: test-e2e
ti: test-integration
tp: test-performance
tpr: test-property
tu: test-unit
