# Claude Code Generation and Maintenance Criteria

## Always Apply These Criteria

### 1. Architecture and Design Patterns
- Follow hybrid architecture (DDD/Clean/Hexagonal)
- Apply Dependency Inversion Principle (DIP)
- Apply Repository pattern for data access
- Use factory patterns for complex object creation
- Create value objects for domain concepts (port from Rust where applicable)
- Use aggregates to maintain consistency boundaries
- Implement ACID compliant transactions for database mutations
- Use saga pattern for distributed transactions
- Look for opportunities to apply generic patterns

### 2. Ada 2022 Language Features
- Always use pragma Ada_2022
- Use ownership features for memory management
- Apply contract aspects (Pre, Post, Type_Invariant, Predicate)
- Utilize expression functions where appropriate
- Use case expressions and if expressions
- Apply parallel blocks and loops where beneficial
- Use 'Image attribute enhancements
- Apply delta aggregates for record updates

### 3. Error Handling
- Always use Result pattern from Core.Domain.Errors
- Never propagate exceptions across architectural boundaries
- All code must have comprehensive error detection and handling
- All exceptions must be caught locally and procedures/functions will return a Result
- Catch exceptions locally and return Result
- Provide meaningful error messages with context
- Use type-safe error types
- Handle all error cases explicitly

### 4. Memory Management
- Prefer stack allocation over heap
- Use controlled types for RAII pattern
- Apply Ada 2022 ownership features for dynamic memory
- Use subpools for related allocations
- Ensure proper finalization
- Avoid memory leaks through proper cleanup

### 5. Coding Standards
- Apply preelaboration pragmas where possible
- Use meaningful names following Ada conventions
- Document with purpose and usage (target junior developers implicitly)
- Use constants from Abohlib where available (especially SI units)
- Create local constants initialized from Abohlib when needed
- Replace hardcoded values with named constants

### 6. Testing and Validation
- Design for testability from the start
- Use dependency injection
- Create interfaces for external dependencies
- Validate inputs at architectural boundaries
- Use type constraints for validation
- Implement unit, integration, and e2e tests (90% coverage target)
- Use mocking where appropriate

### 7. Build and Project Management
- Always create a Makefile with required and best practice commands
- Group Makefile help by function area in ascending order
- Order commands within groups alphabetically
- Use `tests.gpr` not `<project>_tests.gpr` for test project files

### 8. Performance Optimization
- Prefer compile-time checks over runtime
- Use expression functions for simple calculations
- Apply pragma Inline for small functions
- Consider concurrent/parallel operations for independent work
- Use appropriate data structures (vectors, maps, etc.)

### 9. Security
- Validate all external inputs
- Use type safety to prevent errors
- Apply least privilege principle
- Sanitize data at boundaries
- Use cryptographically secure random when needed

### 10. Concurrency
- Use protected types for shared data
- Apply Ada 2022 parallel features
- Avoid race conditions through proper synchronization
- Use task-safe data structures
- Design for concurrent execution

### 11. Domain Modeling
- Create rich domain models with behavior
- Use value objects for concepts without identity
- Apply entity pattern for objects with identity
- Generate domain events for significant state changes
- Use generic packages to reduce code duplication

## Development Workflow Reminders
- Check for compilation errors from core outward
- Run lint and typecheck after implementation
- Use generic packages to reduce code duplication
- Port patterns from Rust where applicable
- Review code against these criteria before completion
