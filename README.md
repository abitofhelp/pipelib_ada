# Pipelib - Reusable Pipeline Components Library

A library of reusable components for building data processing pipelines in Ada 2022.

## Overview

Pipelib provides generic, type-safe components for building various types of data processing pipelines. It follows Domain-Driven Design principles and the hybrid architecture pattern (DDD/Clean/Hexagonal).

## Architecture

```
pipelib/
├── src/
│   └── pipelib/
│       ├── core/
│       │   └── domain/
│       │       ├── value_objects/    # Pipeline-specific value objects
│       │       │   ├── chunk_size    # Type-safe chunk size representation
│       │       │   └── file_chunk    # Immutable data chunk with checksums
│       │       └── services/
│       │           └── stages/       # Generic pipeline stages
│       │               ├── stage_interface      # Common stage interface
│       │               └── generic_hasher_stage # SHA256 hashing stage
│       └── infrastructure/           # Infrastructure implementations
```

## Key Components

### Value Objects
- **Chunk_Size**: Type-safe representation of data chunk sizes with validation
- **File_Chunk**: Immutable data chunks with optional SHA256 checksums

### Generic Stages
- **Stage_Interface**: Generic interface that all pipeline stages must implement
- **Generic_Hasher_Stage**: Generic SHA256 hasher stage for data integrity

## Usage

Add pipelib as a dependency in your `alire.toml`:

```toml
[[depends-on]]
pipelib = "*"

[[pins]]
pipelib = { path='../pipelib' }  # Or use a git URL
```

## Features

- Ada 2022 with full contract support
- Type-safe generic components
- Result-based error handling (no exceptions across boundaries)
- SHA256 hashing support
- Immutable value objects
- Memory-safe with proper cleanup
- Designed for concurrent/parallel processing

## License

MIT License - Copyright (c) 2025 A Bit of Help, Inc.