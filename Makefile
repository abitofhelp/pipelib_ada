# =============================================================================
# Pipelib Makefile - Developer Workflow Automation
# =============================================================================

.PHONY: all build test check format docs clean help ci install

# Default target
all: build test check

# Variables
PROJECT_NAME := pipelib
ALR := alr
GNATFORMAT := gnatformat
GNATPP := gnatpp

# Colors for output
GREEN := \033[0;32m
YELLOW := \033[0;33m
RED := \033[0;31m
NC := \033[0m # No Color

# Help target - default when just typing 'make'
help:
	@echo "$(GREEN)Pipelib Development Makefile$(NC)"
	@echo ""
	@echo "Available targets:"
	@echo "  $(YELLOW)build$(NC)      - Build the library"
	@echo "  $(YELLOW)test$(NC)       - Run all tests"
	@echo "  $(YELLOW)check$(NC)      - Run static analysis (GNAT warnings)"
	@echo "  $(YELLOW)format$(NC)     - Format all source code"
	@echo "  $(YELLOW)docs$(NC)       - Generate documentation"
	@echo "  $(YELLOW)clean$(NC)      - Clean build artifacts"
	@echo "  $(YELLOW)install$(NC)    - Install the library (via Alire)"
	@echo "  $(YELLOW)ci$(NC)         - Run full CI pipeline locally"
	@echo "  $(YELLOW)help$(NC)       - Show this help message"
	@echo ""
	@echo "Quick commands:"
	@echo "  $(YELLOW)make$(NC)        - Build, test, and check"
	@echo "  $(YELLOW)make ci$(NC)     - Run everything (as in CI)"

# Build the library
build:
	@echo "$(GREEN)Building $(PROJECT_NAME)...$(NC)"
	@$(ALR) build
	@echo "$(GREEN)✓ Build complete$(NC)"

# Run tests
test: build
	@echo "$(GREEN)Running tests...$(NC)"
	@if [ -d "tests" ] && [ -n "$$(find tests -name '*.adb' -o -name '*.ads' 2>/dev/null)" ]; then \
		$(ALR) test; \
	else \
		echo "$(YELLOW)No tests found yet$(NC)"; \
	fi
	@echo "$(GREEN)✓ Tests complete$(NC)"

# Static analysis
check:
	@echo "$(GREEN)Running static analysis...$(NC)"
	@$(ALR) build --validation
	@echo "$(GREEN)✓ Static analysis complete$(NC)"

# Format code
format:
	@echo "$(GREEN)Formatting code...$(NC)"
	@alr exec -- gnatformat
	@echo "$(GREEN)✓ Formatting complete$(NC)"

# Generate documentation
docs:
	@echo "$(GREEN)Generating documentation...$(NC)"
	@mkdir -p docs/api
	@if command -v gnatdoc >/dev/null 2>&1; then \
		gnatdoc -P$(PROJECT_NAME).gpr --output=docs/api; \
	else \
		echo "$(YELLOW)Warning: gnatdoc not found, using basic extraction$(NC)"; \
		find src -name "*.ads" -exec grep -H "^--" {} \; > docs/api/extracted_docs.txt; \
	fi
	@echo "$(GREEN)✓ Documentation generated in docs/api/$(NC)"

# Clean build artifacts
clean:
	@echo "$(GREEN)Cleaning build artifacts...$(NC)"
	@$(ALR) clean
	@rm -rf obj lib alire .build
	@echo "$(GREEN)✓ Clean complete$(NC)"

# Install library
install:
	@echo "$(GREEN)Installing $(PROJECT_NAME)...$(NC)"
	@$(ALR) install
	@echo "$(GREEN)✓ Installation complete$(NC)"

# Local CI pipeline - runs everything
ci: clean format build check test docs
	@echo ""
	@echo "$(GREEN)════════════════════════════════════════$(NC)"
	@echo "$(GREEN)✓ CI Pipeline Complete!$(NC)"
	@echo "$(GREEN)════════════════════════════════════════$(NC)"
	@echo ""
	@echo "Summary:"
	@echo "  • Code formatted"
	@echo "  • Library built"
	@echo "  • Static analysis passed"
	@echo "  • Tests passed"
	@echo "  • Documentation generated"

# Development shortcuts
.PHONY: b t c f d

b: build
t: test
c: check
f: format
d: docs

# Watch for changes and rebuild (requires inotify-tools)
watch:
	@echo "$(GREEN)Watching for changes...$(NC)"
	@while true; do \
		inotifywait -q -e modify,create,delete -r src/; \
		clear; \
		make build test; \
	done

# Pre-commit hook setup
setup-hooks:
	@echo "$(GREEN)Setting up git hooks...$(NC)"
	@mkdir -p .git/hooks
	@echo "#!/bin/sh" > .git/hooks/pre-commit
	@echo "make format check" >> .git/hooks/pre-commit
	@chmod +x .git/hooks/pre-commit
	@echo "$(GREEN)✓ Git hooks configured$(NC)"

# Show current project statistics
stats:
	@echo "$(GREEN)Project Statistics:$(NC)"
	@echo "Lines of Ada code:"
	@find src -name "*.ads" -o -name "*.adb" | xargs wc -l | tail -1
	@echo ""
	@echo "File count by type:"
	@echo "  .ads files: $$(find src -name "*.ads" | wc -l)"
	@echo "  .adb files: $$(find src -name "*.adb" | wc -l)"
	@echo ""
	@echo "Directory structure:"
	@tree src -d -L 3 2>/dev/null || find src -type d | sed 's|[^/]*/|- |g'
