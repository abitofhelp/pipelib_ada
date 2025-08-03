# Pipelib Multi-Language Porting Guide

## Overview

This guide provides comprehensive instructions for porting the Ada pipelib architecture and design patterns to Go and Rust. The pipelib implements a high-performance, zero-copy chunk processing pipeline using Domain-Driven Design (DDD), Clean Architecture, and Hexagonal Architecture principles.

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Core Concepts Mapping](#core-concepts-mapping)
3. [Go Implementation Guide](#go-implementation-guide)
4. [Rust Implementation Guide](#rust-implementation-guide)
5. [Contract System Translation](#contract-system-translation)
6. [Error Handling Patterns](#error-handling-patterns)
7. [Concurrency Models](#concurrency-models)
8. [Memory Management](#memory-management)
9. [Testing Strategies](#testing-strategies)
10. [Performance Considerations](#performance-considerations)

## Architecture Overview

The pipelib follows a hybrid architecture combining DDD, Clean, and Hexagonal patterns:

```
┌─────────────────────────────────────────────────────────┐
│                    Presentation Layer                    │
├─────────────────────────────────────────────────────────┤
│                   Application Layer                      │
│  ┌─────────────┐  ┌──────────────┐  ┌──────────────┐  │
│  │  Use Cases  │  │   Services   │  │     DTOs     │  │
│  └─────────────┘  └──────────────┘  └──────────────┘  │
├─────────────────────────────────────────────────────────┤
│                     Domain Layer                         │
│  ┌──────────┐  ┌────────────┐  ┌──────────────────┐   │
│  │ Entities │  │   Value    │  │     Domain       │   │
│  │          │  │  Objects   │  │    Services      │   │
│  └──────────┘  └────────────┘  └──────────────────┘   │
├─────────────────────────────────────────────────────────┤
│                 Infrastructure Layer                     │
│  ┌──────────┐  ┌────────────┐  ┌──────────────────┐   │
│  │    IO    │  │  Database  │  │   External APIs  │   │
│  └──────────┘  └────────────┘  └──────────────────┘   │
└─────────────────────────────────────────────────────────┘
```

### Key Architectural Principles

1. **Dependency Inversion**: All dependencies point inward toward the domain
2. **Separation of Concerns**: Each layer has distinct responsibilities
3. **Interface Segregation**: Small, focused interfaces
4. **Single Responsibility**: Each component has one reason to change

## Core Concepts Mapping

### Ada to Go/Rust Type Mapping

| Ada Concept | Go Equivalent | Rust Equivalent |
|------------|---------------|-----------------|
| Package | Package | Module |
| Generic Package | Interface + Factory | Generic Trait |
| Tagged Type | Struct with methods | Struct with impl |
| Interface Type | Interface | Trait |
| Access Type | Pointer (*T) | Box<T>, Rc<T>, Arc<T> |
| Protected Type | Mutex/Channel | Mutex<T>/RwLock<T> |
| Task Type | Goroutine | Thread/async Task |
| Discriminated Record | Interface + Type assertion | Enum |
| Array | Array/Slice | Array/Vec |
| Controlled Type | Defer/Finalizer | Drop trait |

### Domain Model Mapping

#### Value Objects

**Ada Pattern:**
```ada
type File_Chunk is tagged private;
--  Invariant: Data is immutable after creation
```

**Go Pattern:**
```go
type FileChunk struct {
    id           uuid.UUID
    sequenceNum  uint32
    offset       int64
    data         []byte // Make copy on creation
    isFinal      bool
}

// NewFileChunk creates an immutable file chunk
func NewFileChunk(sequenceNum uint32, offset int64, data []byte, isFinal bool) (*FileChunk, error) {
    // Validate inputs
    if sequenceNum > MaxSequenceNumber {
        return nil, ErrInvalidSequenceNumber
    }

    // Copy data to ensure immutability
    dataCopy := make([]byte, len(data))
    copy(dataCopy, data)

    return &FileChunk{
        id:          uuid.New(),
        sequenceNum: sequenceNum,
        offset:      offset,
        data:        dataCopy,
        isFinal:     isFinal,
    }, nil
}

// Data returns a copy of the chunk data
func (fc *FileChunk) Data() []byte {
    result := make([]byte, len(fc.data))
    copy(result, fc.data)
    return result
}
```

**Rust Pattern:**
```rust
#[derive(Clone, Debug)]
pub struct FileChunk {
    id: Uuid,
    sequence_num: u32,
    offset: i64,
    data: Vec<u8>,
    is_final: bool,
}

impl FileChunk {
    /// Creates a new immutable file chunk
    pub fn new(sequence_num: u32, offset: i64, data: Vec<u8>, is_final: bool) -> Result<Self, ChunkError> {
        if sequence_num > MAX_SEQUENCE_NUMBER {
            return Err(ChunkError::InvalidSequenceNumber);
        }

        Ok(FileChunk {
            id: Uuid::new_v4(),
            sequence_num,
            offset,
            data,
            is_final,
        })
    }

    /// Returns a reference to the chunk data
    pub fn data(&self) -> &[u8] {
        &self.data
    }
}
```

#### Entities

**Ada Pattern:**
```ada
type Chunk is new Ada.Finalization.Controlled with private;
--  State machine with lifecycle management
```

**Go Pattern:**
```go
type ChunkState int

const (
    ChunkStatePending ChunkState = iota
    ChunkStateProcessing
    ChunkStateCompleted
    ChunkStateFailed
)

type Chunk struct {
    mu           sync.RWMutex
    id           uuid.UUID
    state        ChunkState
    fileChunk    *FileChunk
    result       []byte
    errorInfo    error
    transitions  int
}

func (c *Chunk) StartProcessing() error {
    c.mu.Lock()
    defer c.mu.Unlock()

    if c.state != ChunkStatePending {
        return ErrInvalidStateTransition
    }

    c.state = ChunkStateProcessing
    c.transitions++
    return nil
}
```

**Rust Pattern:**
```rust
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ChunkState {
    Pending,
    Processing,
    Completed,
    Failed,
}

pub struct Chunk {
    id: Uuid,
    state: RwLock<ChunkState>,
    file_chunk: FileChunk,
    result: RwLock<Option<Vec<u8>>>,
    error_info: RwLock<Option<String>>,
    transitions: AtomicU32,
}

impl Chunk {
    pub fn start_processing(&self) -> Result<(), ChunkError> {
        let mut state = self.state.write().unwrap();

        match *state {
            ChunkState::Pending => {
                *state = ChunkState::Processing;
                self.transitions.fetch_add(1, Ordering::SeqCst);
                Ok(())
            }
            _ => Err(ChunkError::InvalidStateTransition),
        }
    }
}
```

## Go Implementation Guide

### Project Structure

```
pipelib-go/
├── cmd/
│   └── example/
│       └── main.go
├── internal/
│   ├── core/
│   │   ├── domain/
│   │   │   ├── entities/
│   │   │   │   └── chunk.go
│   │   │   ├── valueobjects/
│   │   │   │   ├── filechunk.go
│   │   │   │   └── algorithm.go
│   │   │   ├── services/
│   │   │   │   └── progresstracker.go
│   │   │   └── errors/
│   │   │       └── result.go
│   │   └── application/
│   │       ├── services/
│   │       │   └── chunkprocessor.go
│   │       └── ports/
│   │           ├── input.go
│   │           └── output.go
│   └── infrastructure/
│       ├── io/
│       │   ├── mmap.go
│       │   └── randomwrite.go
│       └── adapters/
│           └── mmapadapter.go
├── pkg/
│   └── pipelib/
│       └── api.go
├── go.mod
├── go.sum
└── Makefile
```

### Key Implementation Patterns

#### Result Pattern for Error Handling

```go
// pkg/result/result.go
type Result[T any] struct {
    value *T
    err   error
}

func Ok[T any](value T) Result[T] {
    return Result[T]{value: &value}
}

func Err[T any](err error) Result[T] {
    return Result[T]{err: err}
}

func (r Result[T]) IsOk() bool {
    return r.err == nil
}

func (r Result[T]) Unwrap() T {
    if r.err != nil {
        panic("called Unwrap on error result")
    }
    return *r.value
}

func (r Result[T]) UnwrapOr(defaultValue T) T {
    if r.err != nil {
        return defaultValue
    }
    return *r.value
}
```

#### Interface-Based Dependency Injection

```go
// core/application/ports/input.go
type ChunkReader interface {
    ReadChunk(offset int64, size int) (Result[FileChunk], error)
    Close() error
}

// core/application/ports/output.go
type ChunkWriter interface {
    WriteChunk(chunk ProcessedChunk) error
    Flush() error
    Close() error
}

// core/application/services/processor.go
type ChunkProcessor struct {
    reader    ChunkReader
    writer    ChunkWriter
    algorithm Algorithm
    workers   int
}

func NewChunkProcessor(reader ChunkReader, writer ChunkWriter, algorithm Algorithm, workers int) *ChunkProcessor {
    return &ChunkProcessor{
        reader:    reader,
        writer:    writer,
        algorithm: algorithm,
        workers:   workers,
    }
}
```

#### Concurrent Processing with Channels

```go
func (p *ChunkProcessor) Process(ctx context.Context) error {
    chunkChan := make(chan *Chunk, p.workers*2)
    resultChan := make(chan *ProcessedChunk, p.workers*2)
    errChan := make(chan error, p.workers)

    // Start workers
    var wg sync.WaitGroup
    for i := 0; i < p.workers; i++ {
        wg.Add(1)
        go p.worker(ctx, &wg, chunkChan, resultChan, errChan)
    }

    // Start result writer
    go p.writeResults(ctx, resultChan, errChan)

    // Read and send chunks
    offset := int64(0)
    for {
        result := p.reader.ReadChunk(offset, DefaultChunkSize)
        if !result.IsOk() {
            break
        }

        chunk := NewChunk(result.Unwrap())
        select {
        case chunkChan <- chunk:
            offset += int64(DefaultChunkSize)
        case <-ctx.Done():
            return ctx.Err()
        }
    }

    close(chunkChan)
    wg.Wait()
    close(resultChan)

    return nil
}
```

## Rust Implementation Guide

### Project Structure

```
pipelib-rust/
├── Cargo.toml
├── Cargo.lock
├── src/
│   ├── lib.rs
│   ├── core/
│   │   ├── mod.rs
│   │   ├── domain/
│   │   │   ├── mod.rs
│   │   │   ├── entities/
│   │   │   │   ├── mod.rs
│   │   │   │   └── chunk.rs
│   │   │   ├── value_objects/
│   │   │   │   ├── mod.rs
│   │   │   │   ├── file_chunk.rs
│   │   │   │   └── algorithm.rs
│   │   │   ├── services/
│   │   │   │   ├── mod.rs
│   │   │   │   └── progress_tracker.rs
│   │   │   └── errors/
│   │   │       ├── mod.rs
│   │   │       └── result.rs
│   │   └── application/
│   │       ├── mod.rs
│   │       ├── services/
│   │       │   ├── mod.rs
│   │       │   └── chunk_processor.rs
│   │       └── ports/
│   │           ├── mod.rs
│   │           ├── input.rs
│   │           └── output.rs
│   └── infrastructure/
│       ├── mod.rs
│       ├── io/
│       │   ├── mod.rs
│       │   ├── mmap.rs
│       │   └── random_write.rs
│       └── adapters/
│           ├── mod.rs
│           └── mmap_adapter.rs
├── examples/
│   └── basic_pipeline.rs
├── tests/
│   └── integration_test.rs
└── benches/
    └── performance.rs
```

### Key Implementation Patterns

#### Type-Safe Error Handling

```rust
// core/domain/errors/mod.rs
use thiserror::Error;

#[derive(Error, Debug)]
pub enum PipelibError {
    #[error("Invalid sequence number: {0}")]
    InvalidSequenceNumber(u32),

    #[error("Invalid state transition from {from:?} to {to:?}")]
    InvalidStateTransition {
        from: ChunkState,
        to: ChunkState,
    },

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Processing failed: {0}")]
    ProcessingFailed(String),
}

pub type Result<T> = std::result::Result<T, PipelibError>;
```

#### Trait-Based Abstraction

```rust
// core/application/ports/input.rs
use async_trait::async_trait;

#[async_trait]
pub trait ChunkReader: Send + Sync {
    async fn read_chunk(&mut self, offset: i64, size: usize) -> Result<Option<FileChunk>>;
    async fn close(&mut self) -> Result<()>;
}

// core/application/ports/output.rs
#[async_trait]
pub trait ChunkWriter: Send + Sync {
    async fn write_chunk(&mut self, chunk: ProcessedChunk) -> Result<()>;
    async fn flush(&mut self) -> Result<()>;
    async fn close(&mut self) -> Result<()>;
}
```

#### Zero-Copy Operations with Ownership

```rust
// infrastructure/io/mmap.rs
use memmap2::{Mmap, MmapOptions};
use std::fs::File;
use std::sync::Arc;

pub struct MemoryMappedFile {
    mmap: Arc<Mmap>,
    file: File,
}

impl MemoryMappedFile {
    pub fn new(path: &Path) -> Result<Self> {
        let file = File::open(path)?;
        let mmap = unsafe { MmapOptions::new().map(&file)? };

        Ok(MemoryMappedFile {
            mmap: Arc::new(mmap),
            file,
        })
    }

    /// Zero-copy slice of the mapped memory
    pub fn slice(&self, offset: usize, len: usize) -> Option<&[u8]> {
        if offset + len <= self.mmap.len() {
            Some(&self.mmap[offset..offset + len])
        } else {
            None
        }
    }
}
```

#### Async Concurrent Processing

```rust
use tokio::sync::mpsc;
use tokio::task::JoinSet;

pub struct ParallelChunkProcessor {
    reader: Box<dyn ChunkReader>,
    writer: Box<dyn ChunkWriter>,
    algorithm: Box<dyn Algorithm>,
    workers: usize,
}

impl ParallelChunkProcessor {
    pub async fn process(&mut self) -> Result<()> {
        let (tx, mut rx) = mpsc::channel::<Chunk>(self.workers * 2);
        let (result_tx, mut result_rx) = mpsc::channel::<ProcessedChunk>(self.workers * 2);

        // Spawn workers
        let mut workers = JoinSet::new();
        for _ in 0..self.workers {
            let rx_clone = rx.clone();
            let tx_clone = result_tx.clone();
            let algo = self.algorithm.clone();

            workers.spawn(async move {
                while let Some(chunk) = rx_clone.recv().await {
                    match chunk.process(&algo).await {
                        Ok(processed) => {
                            if tx_clone.send(processed).await.is_err() {
                                break;
                            }
                        }
                        Err(e) => eprintln!("Processing error: {}", e),
                    }
                }
            });
        }

        // Spawn result writer
        let mut writer = self.writer.clone();
        let writer_task = tokio::spawn(async move {
            while let Some(result) = result_rx.recv().await {
                writer.write_chunk(result).await?;
            }
            writer.flush().await
        });

        // Read and send chunks
        let mut offset = 0i64;
        while let Some(file_chunk) = self.reader.read_chunk(offset, CHUNK_SIZE).await? {
            let chunk = Chunk::new(file_chunk)?;
            tx.send(chunk).await.map_err(|_| PipelibError::ChannelClosed)?;
            offset += CHUNK_SIZE as i64;
        }

        // Cleanup
        drop(tx);
        workers.join_all().await;
        drop(result_tx);
        writer_task.await??;

        Ok(())
    }
}
```

## Contract System Translation

### Ada Contracts to Go

Ada's contract system doesn't have a direct equivalent in Go, but we can implement similar guarantees:

```go
// Precondition checking
func (fc *FileChunk) SetData(data []byte) error {
    // Simulate Pre => data'Length > 0
    if len(data) == 0 {
        return errors.New("precondition failed: data must not be empty")
    }

    // Simulate Pre => data'Length <= Max_Chunk_Size
    if len(data) > MaxChunkSize {
        return errors.New("precondition failed: data exceeds maximum chunk size")
    }

    // Implementation
    fc.data = make([]byte, len(data))
    copy(fc.data, data)

    // Simulate Post => fc.data'Length == data'Length
    if len(fc.data) != len(data) {
        panic("postcondition failed: data length mismatch")
    }

    return nil
}

// Type invariant checking
func (c *Chunk) checkInvariant() {
    // Simulate Type_Invariant
    if c.state == ChunkStateCompleted && c.result == nil {
        panic("invariant violation: completed chunk must have result")
    }

    if c.state == ChunkStateFailed && c.errorInfo == nil {
        panic("invariant violation: failed chunk must have error info")
    }
}
```

### Ada Contracts to Rust

Rust provides better support for contracts through its type system:

```rust
// Use NewType pattern for constrained types
#[derive(Debug, Clone, Copy)]
pub struct SequenceNumber(u32);

impl SequenceNumber {
    pub fn new(value: u32) -> Result<Self> {
        // Simulate subtype constraint
        if value > MAX_SEQUENCE_NUMBER {
            return Err(PipelibError::InvalidSequenceNumber(value));
        }
        Ok(SequenceNumber(value))
    }
}

// Use builder pattern with validation
pub struct ChunkBuilder {
    sequence_num: Option<SequenceNumber>,
    offset: Option<i64>,
    data: Option<Vec<u8>>,
}

impl ChunkBuilder {
    pub fn new() -> Self {
        ChunkBuilder {
            sequence_num: None,
            offset: None,
            data: None,
        }
    }

    pub fn sequence_number(mut self, num: u32) -> Result<Self> {
        self.sequence_num = Some(SequenceNumber::new(num)?);
        Ok(self)
    }

    pub fn build(self) -> Result<FileChunk> {
        // Enforce all required fields
        let sequence_num = self.sequence_num.ok_or(PipelibError::MissingField("sequence_num"))?;
        let offset = self.offset.ok_or(PipelibError::MissingField("offset"))?;
        let data = self.data.ok_or(PipelibError::MissingField("data"))?;

        // Additional validation
        if data.is_empty() {
            return Err(PipelibError::EmptyData);
        }

        Ok(FileChunk {
            id: Uuid::new_v4(),
            sequence_num: sequence_num.0,
            offset,
            data,
            is_final: false,
        })
    }
}

// Use phantom types for state machine
pub struct Pending;
pub struct Processing;
pub struct Completed;

pub struct TypedChunk<S> {
    inner: Chunk,
    _state: PhantomData<S>,
}

impl TypedChunk<Pending> {
    pub fn start_processing(self) -> TypedChunk<Processing> {
        // State transition is type-safe
        TypedChunk {
            inner: self.inner,
            _state: PhantomData,
        }
    }
}
```

## Error Handling Patterns

### Go Error Handling

```go
// Domain-specific errors
type ChunkError struct {
    Code    string
    Message string
    Cause   error
}

func (e ChunkError) Error() string {
    if e.Cause != nil {
        return fmt.Sprintf("%s: %s (caused by: %v)", e.Code, e.Message, e.Cause)
    }
    return fmt.Sprintf("%s: %s", e.Code, e.Message)
}

// Error codes
const (
    ErrCodeInvalidState     = "INVALID_STATE"
    ErrCodeProcessingFailed = "PROCESSING_FAILED"
    ErrCodeIOError          = "IO_ERROR"
)

// Constructor functions for common errors
func NewInvalidStateError(from, to string) error {
    return ChunkError{
        Code:    ErrCodeInvalidState,
        Message: fmt.Sprintf("invalid state transition from %s to %s", from, to),
    }
}

// Usage with Result pattern
func ProcessChunk(chunk *Chunk) Result[ProcessedChunk] {
    if err := chunk.StartProcessing(); err != nil {
        return Err[ProcessedChunk](err)
    }

    // Process...

    return Ok(ProcessedChunk{...})
}
```

### Rust Error Handling

```rust
// Custom error types with thiserror
#[derive(Error, Debug)]
pub enum ChunkError {
    #[error("Invalid state transition")]
    InvalidState {
        from: ChunkState,
        to: ChunkState,
    },

    #[error("Processing failed")]
    ProcessingFailed(#[source] Box<dyn Error + Send + Sync>),

    #[error("IO operation failed")]
    Io(#[from] io::Error),
}

// Result type alias
pub type ChunkResult<T> = Result<T, ChunkError>;

// Error propagation with ?
pub async fn process_file(path: &Path) -> ChunkResult<Stats> {
    let file = MemoryMappedFile::new(path)?;
    let processor = ParallelChunkProcessor::new(file)?;
    let stats = processor.process().await?;
    Ok(stats)
}

// Error context with anyhow
use anyhow::{Context, Result};

pub fn read_chunk_with_context(path: &Path, offset: u64) -> Result<FileChunk> {
    let file = File::open(path)
        .with_context(|| format!("Failed to open file: {}", path.display()))?;

    // Read chunk...

    Ok(chunk)
}
```

## Concurrency Models

### Go Concurrency

```go
// Worker pool pattern
type WorkerPool struct {
    workers   int
    taskQueue chan Task
    wg        sync.WaitGroup
}

func NewWorkerPool(workers int) *WorkerPool {
    return &WorkerPool{
        workers:   workers,
        taskQueue: make(chan Task, workers*2),
    }
}

func (p *WorkerPool) Start(ctx context.Context) {
    for i := 0; i < p.workers; i++ {
        p.wg.Add(1)
        go p.worker(ctx, i)
    }
}

func (p *WorkerPool) worker(ctx context.Context, id int) {
    defer p.wg.Done()

    for {
        select {
        case task, ok := <-p.taskQueue:
            if !ok {
                return
            }
            task.Execute()
        case <-ctx.Done():
            return
        }
    }
}

// Progress tracking with channels
type ProgressTracker struct {
    total     atomic.Int64
    processed atomic.Int64
    updates   chan ProgressUpdate
}

func (pt *ProgressTracker) Track(ctx context.Context) {
    ticker := time.NewTicker(time.Second)
    defer ticker.Stop()

    for {
        select {
        case <-ticker.C:
            processed := pt.processed.Load()
            total := pt.total.Load()

            if total > 0 {
                percentage := float64(processed) / float64(total) * 100
                pt.updates <- ProgressUpdate{
                    Processed:  processed,
                    Total:      total,
                    Percentage: percentage,
                }
            }
        case <-ctx.Done():
            return
        }
    }
}
```

### Rust Concurrency

```rust
// Actor pattern with tokio
use tokio::sync::{mpsc, oneshot};

pub struct ChunkProcessor {
    sender: mpsc::Sender<ProcessorCommand>,
}

pub enum ProcessorCommand {
    Process {
        chunk: Chunk,
        respond_to: oneshot::Sender<ChunkResult<ProcessedChunk>>,
    },
    GetStats {
        respond_to: oneshot::Sender<ProcessorStats>,
    },
    Shutdown,
}

impl ChunkProcessor {
    pub fn new(algorithm: Box<dyn Algorithm>) -> Self {
        let (sender, mut receiver) = mpsc::channel(100);

        tokio::spawn(async move {
            let mut stats = ProcessorStats::default();

            while let Some(cmd) = receiver.recv().await {
                match cmd {
                    ProcessorCommand::Process { chunk, respond_to } => {
                        let result = chunk.process(&algorithm).await;
                        stats.processed += 1;
                        let _ = respond_to.send(result);
                    }
                    ProcessorCommand::GetStats { respond_to } => {
                        let _ = respond_to.send(stats.clone());
                    }
                    ProcessorCommand::Shutdown => break,
                }
            }
        });

        ChunkProcessor { sender }
    }

    pub async fn process(&self, chunk: Chunk) -> ChunkResult<ProcessedChunk> {
        let (respond_to, response) = oneshot::channel();

        self.sender
            .send(ProcessorCommand::Process { chunk, respond_to })
            .await
            .map_err(|_| ChunkError::ProcessorShutdown)?;

        response.await.map_err(|_| ChunkError::ProcessorShutdown)?
    }
}

// Shared state with Arc<RwLock<T>>
pub struct SharedProgressTracker {
    inner: Arc<RwLock<ProgressTrackerInner>>,
}

struct ProgressTrackerInner {
    total: u64,
    processed: u64,
    start_time: Instant,
}

impl SharedProgressTracker {
    pub fn new(total: u64) -> Self {
        SharedProgressTracker {
            inner: Arc::new(RwLock::new(ProgressTrackerInner {
                total,
                processed: 0,
                start_time: Instant::now(),
            })),
        }
    }

    pub async fn increment(&self) {
        let mut inner = self.inner.write().await;
        inner.processed += 1;
    }

    pub async fn get_progress(&self) -> Progress {
        let inner = self.inner.read().await;
        Progress {
            percentage: (inner.processed as f64 / inner.total as f64) * 100.0,
            elapsed: inner.start_time.elapsed(),
            eta: calculate_eta(&inner),
        }
    }
}
```

## Memory Management

### Go Memory Management

```go
// Object pooling for frequent allocations
var chunkPool = sync.Pool{
    New: func() interface{} {
        return &Chunk{
            data: make([]byte, 0, DefaultChunkSize),
        }
    },
}

func GetChunk() *Chunk {
    chunk := chunkPool.Get().(*Chunk)
    chunk.Reset()
    return chunk
}

func PutChunk(chunk *Chunk) {
    chunk.Clear()
    chunkPool.Put(chunk)
}

// Careful slice handling
func (fc *FileChunk) GetDataView() []byte {
    // Return view without copying
    return fc.data
}

func (fc *FileChunk) GetDataCopy() []byte {
    // Return defensive copy
    result := make([]byte, len(fc.data))
    copy(result, fc.data)
    return result
}

// Memory-mapped file with cleanup
type MappedFile struct {
    file *os.File
    data []byte
}

func (mf *MappedFile) Close() error {
    if mf.data != nil {
        if err := syscall.Munmap(mf.data); err != nil {
            return err
        }
        mf.data = nil
    }

    if mf.file != nil {
        if err := mf.file.Close(); err != nil {
            return err
        }
        mf.file = nil
    }

    return nil
}
```

### Rust Memory Management

```rust
// Smart pointers for ownership
use std::rc::Rc;
use std::sync::Arc;

// Single-threaded reference counting
pub struct ChunkCache {
    chunks: HashMap<Uuid, Rc<FileChunk>>,
}

// Multi-threaded reference counting
pub struct SharedChunkCache {
    chunks: Arc<RwLock<HashMap<Uuid, Arc<FileChunk>>>>,
}

// Custom allocator for performance
use std::alloc::{GlobalAlloc, Layout, System};

struct ChunkAllocator;

unsafe impl GlobalAlloc for ChunkAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        if layout.size() == CHUNK_SIZE {
            // Use pre-allocated pool for chunk-sized allocations
            // ...
        }
        System.alloc(layout)
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        if layout.size() == CHUNK_SIZE {
            // Return to pool
            // ...
        }
        System.dealloc(ptr, layout)
    }
}

// Zero-copy with Cow
use std::borrow::Cow;

pub struct ChunkData<'a> {
    data: Cow<'a, [u8]>,
}

impl<'a> ChunkData<'a> {
    pub fn borrowed(data: &'a [u8]) -> Self {
        ChunkData {
            data: Cow::Borrowed(data),
        }
    }

    pub fn owned(data: Vec<u8>) -> Self {
        ChunkData {
            data: Cow::Owned(data),
        }
    }

    pub fn into_owned(self) -> Vec<u8> {
        self.data.into_owned()
    }
}
```

## Testing Strategies

### Go Testing

```go
// Table-driven tests
func TestFileChunkValidation(t *testing.T) {
    tests := []struct {
        name        string
        sequenceNum uint32
        offset      int64
        data        []byte
        wantErr     bool
    }{
        {
            name:        "valid chunk",
            sequenceNum: 1,
            offset:      0,
            data:        []byte("test data"),
            wantErr:     false,
        },
        {
            name:        "invalid sequence number",
            sequenceNum: MaxSequenceNumber + 1,
            offset:      0,
            data:        []byte("test"),
            wantErr:     true,
        },
        {
            name:        "empty data",
            sequenceNum: 1,
            offset:      0,
            data:        []byte{},
            wantErr:     true,
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            _, err := NewFileChunk(tt.sequenceNum, tt.offset, tt.data, false)
            if (err != nil) != tt.wantErr {
                t.Errorf("NewFileChunk() error = %v, wantErr %v", err, tt.wantErr)
            }
        })
    }
}

// Mocking interfaces
type MockChunkReader struct {
    chunks []FileChunk
    index  int
}

func (m *MockChunkReader) ReadChunk(offset int64, size int) (Result[FileChunk], error) {
    if m.index >= len(m.chunks) {
        return Err[FileChunk](io.EOF), nil
    }

    chunk := m.chunks[m.index]
    m.index++
    return Ok(chunk), nil
}

// Benchmark tests
func BenchmarkChunkProcessing(b *testing.B) {
    chunk := createTestChunk(1024 * 1024) // 1MB
    processor := NewProcessor()

    b.ResetTimer()
    b.ReportAllocs()

    for i := 0; i < b.N; i++ {
        _ = processor.Process(chunk)
    }
}
```

### Rust Testing

```rust
// Property-based testing with proptest
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_file_chunk_roundtrip(
        seq_num in 0u32..MAX_SEQUENCE_NUMBER,
        offset in 0i64..i64::MAX,
        data in prop::collection::vec(any::<u8>(), 1..MAX_CHUNK_SIZE)
    ) {
        let chunk = FileChunk::new(seq_num, offset, data.clone(), false)?;

        prop_assert_eq!(chunk.sequence_number(), seq_num);
        prop_assert_eq!(chunk.offset(), offset);
        prop_assert_eq!(chunk.data(), &data[..]);
    }
}

// Mocking with mockall
use mockall::predicate::*;
use mockall::*;

#[automock]
#[async_trait]
pub trait ChunkReader {
    async fn read_chunk(&mut self, offset: i64, size: usize) -> Result<Option<FileChunk>>;
}

#[tokio::test]
async fn test_processor_handles_errors() {
    let mut mock_reader = MockChunkReader::new();

    mock_reader
        .expect_read_chunk()
        .with(eq(0), eq(1024))
        .times(1)
        .returning(|_, _| Err(PipelibError::Io(io::Error::new(io::ErrorKind::Other, "test error"))));

    let processor = ChunkProcessor::new(Box::new(mock_reader));
    let result = processor.process().await;

    assert!(result.is_err());
}

// Criterion benchmarks
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn chunk_processing_benchmark(c: &mut Criterion) {
    let chunk = create_test_chunk(1024 * 1024); // 1MB

    c.bench_function("process_chunk", |b| {
        b.iter(|| {
            let processed = black_box(&chunk).process(&TestAlgorithm).unwrap();
            black_box(processed);
        })
    });
}

criterion_group!(benches, chunk_processing_benchmark);
criterion_main!(benches);
```

## Performance Considerations

### Go Performance Tips

1. **Minimize Allocations**
   ```go
   // Bad: Creates new slice each call
   func GetData() []byte {
       return make([]byte, 1024)
   }

   // Good: Reuses buffer
   func GetData(buf []byte) []byte {
       if cap(buf) < 1024 {
           buf = make([]byte, 1024)
       }
       return buf[:1024]
   }
   ```

2. **Avoid Interface Boxing**
   ```go
   // Bad: Boxing overhead
   func ProcessAny(v interface{}) {
       // Type assertion needed
   }

   // Good: Type-specific
   func ProcessChunk(c *Chunk) {
       // Direct access
   }
   ```

3. **Use sync.Pool for Temporary Objects**
   ```go
   var bufferPool = sync.Pool{
       New: func() interface{} {
           return make([]byte, 0, 4096)
       },
   }
   ```

### Rust Performance Tips

1. **Zero-Cost Abstractions**
   ```rust
   // Iterator chains compile to efficient code
   let sum: u64 = chunks
       .iter()
       .filter(|c| c.is_valid())
       .map(|c| c.size())
       .sum();
   ```

2. **Avoid Unnecessary Cloning**
   ```rust
   // Bad: Clones entire vector
   fn process(data: Vec<u8>) {
       let copy = data.clone();
   }

   // Good: Borrows data
   fn process(data: &[u8]) {
       // Work with borrowed data
   }
   ```

3. **Use const generics for compile-time optimization**
   ```rust
   pub struct FixedChunk<const SIZE: usize> {
       data: [u8; SIZE],
   }

   impl<const SIZE: usize> FixedChunk<SIZE> {
       pub const fn new() -> Self {
           FixedChunk { data: [0; SIZE] }
       }
   }
   ```

## Migration Checklist

### Pre-Migration
- [ ] Understand Ada codebase architecture
- [ ] Identify core domain concepts
- [ ] Map Ada types to target language
- [ ] Plan module/package structure
- [ ] Design error handling strategy

### Core Implementation
- [ ] Implement Result/Error types
- [ ] Create value objects
- [ ] Implement entities with state machines
- [ ] Build domain services
- [ ] Create application services
- [ ] Implement infrastructure adapters

### Contract Migration
- [ ] Map preconditions to validation
- [ ] Implement postcondition checks
- [ ] Add invariant enforcement
- [ ] Create contract tests
- [ ] Document contract semantics

### Concurrency
- [ ] Design concurrent architecture
- [ ] Implement thread-safe components
- [ ] Add progress tracking
- [ ] Test concurrent scenarios
- [ ] Benchmark performance

### Testing
- [ ] Unit tests for all components
- [ ] Integration tests for workflows
- [ ] Contract validation tests
- [ ] Performance benchmarks
- [ ] Memory usage profiling

### Documentation
- [ ] API documentation
- [ ] Architecture diagrams
- [ ] Usage examples
- [ ] Performance guidelines
- [ ] Migration notes

## Conclusion

This guide provides a comprehensive roadmap for porting the Ada pipelib to Go and Rust. The key to successful migration is understanding the architectural principles and adapting them idiomatically to each target language rather than attempting a line-by-line translation.

Focus on:
- Maintaining architectural boundaries
- Preserving contract semantics
- Leveraging language-specific features
- Ensuring performance and safety
- Creating comprehensive tests

Remember that each language has its strengths - use them to improve upon the original design while maintaining the core functionality and guarantees of the Ada implementation.
