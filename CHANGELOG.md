# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2025-08-03

### Added
- Initial stable release of Pipelib
- Core domain model with Chunk entity and value objects
- Parallel chunk processing with configurable worker threads
- Memory-mapped file I/O for efficient large file handling
- Random write file support for concurrent output
- Comprehensive Ada 2022 contract system
- Result pattern for error handling without exceptions
- Progress tracking with thread-safe operations
- Generic pipeline stage framework
- Support for multiple algorithms (SHA-256, MD5, compression, encoding)
- Extensive test suite with 92% code coverage
- Complete documentation including SRS, SDD, and STP
- Architecture diagrams and guides

### Performance
- Sequential processing: 100+ MB/s
- Parallel processing (8 cores): 700+ MB/s
- Memory-mapped I/O: 1+ GB/s for large files

### Technical Details
- Full Ada 2022 feature support
- Zero-copy operations for memory efficiency
- RAII patterns with controlled types
- Thread-safe design with protected types
- Comprehensive contract validation (43 tests)
- Clean/Hexagonal/DDD architecture

### Dependencies
- GNAT 12.0+ with Ada 2022 support
- Abohlib utility library
- SHA2 library for cryptographic hashing

---

For details on unreleased changes, see the [development branch](https://github.com/abitofhelp/pipelib/tree/develop).
