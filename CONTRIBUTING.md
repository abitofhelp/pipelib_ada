# Contributing to Abohlib

Thank you for your interest in contributing to Abohlib! This guide will help you get started.

## Code of Conduct

This project adheres to a code of conduct. By participating, you are expected to uphold this code. Please be respectful and constructive in all interactions.

## How to Contribute

### Reporting Issues

- Check if the issue already exists in the [issue tracker](https://github.com/abitofhelp/abohlib/issues)
- Include a clear description of the problem
- Provide steps to reproduce the issue
- Include your environment details (OS, Ada compiler version)

### Submitting Pull Requests

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/your-feature-name`)
3. Make your changes following our coding standards
4. Add tests for your changes
5. Ensure all tests pass (`make test`)
6. Commit with clear, descriptive messages
7. Push to your fork and submit a pull request

## Development Standards

### Code Style

- Follow the coding standards defined in [CLAUDE.md](CLAUDE.md)
- Use `pragma Ada_2022` in all new files
- Apply strong typing principles
- Use the Result pattern for error handling
- Add comprehensive contracts (Pre, Post, Type_Invariant)

### Testing Requirements

- Maintain 90%+ test coverage for domain layer
- Write unit tests for all new functionality
- Include integration tests where appropriate
- Follow the Result-based testing pattern

### Documentation

- Document all public APIs
- Include examples for complex functionality
- Update relevant documentation files
- Keep README.md current with your changes

## Development Setup

1. Install Ada toolchain (GNAT 12.0+)
2. Install Alire package manager
3. Clone the repository
4. Run `alr build` to build the project
5. Run `make test` to run all tests

## Making Changes

### Before Starting

- Discuss major changes in an issue first
- Check our project board for ongoing work
- Ensure your fork is up to date

### Quality Checklist

Before submitting your pull request, ensure:

- [ ] Code follows project standards
- [ ] All tests pass (`make test`)
- [ ] Coverage remains at 90%+ (`make coverage`)
- [ ] No compiler warnings (`alr build`)
- [ ] Documentation is updated
- [ ] Commit messages are clear

### Commit Messages

Follow conventional commit format:
```
type(scope): description

Longer explanation if needed

Fixes #123
```

Types: feat, fix, docs, style, refactor, test, chore

## Questions?

- Check existing documentation in the `docs/` directory
- Ask in [GitHub Discussions](https://github.com/abitofhelp/abohlib/discussions)
- Open an issue for clarification

Thank you for contributing to Abohlib!
