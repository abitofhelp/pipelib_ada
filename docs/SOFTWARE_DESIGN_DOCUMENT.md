# Software Design Document (SDD)
## Pipelib - Reusable Pipeline Components Library

**Version:** 1.0
**Date:** January 2025
**Document Classification:** Internal Development

---

## Table of Contents

1. [Introduction](#1-introduction)
2. [System Architecture](#2-system-architecture)
3. [Detailed Design](#3-detailed-design)
4. [Component Design](#4-component-design)
5. [Type Safety Design](#5-type-safety-design)
6. [Interface Design](#6-interface-design)
7. [Data Design](#7-data-design)
8. [Error Handling Design](#8-error-handling-design)
9. [Concurrency Design](#9-concurrency-design)

---

## 1. Introduction

### 1.1 Purpose

This Software Design Document provides the architectural and detailed design for Pipelib, a reusable pipeline components library implemented in Ada 2022. The design emphasizes type safety, memory efficiency, concurrent processing capabilities, and comprehensive error handling using the Result pattern.

### 1.2 Scope

This document covers the complete software design including:
- System architecture and component organization
- Detailed interface specifications with Ada 2022 contracts
- Data structure design and memory management
- Concurrency patterns and thread safety mechanisms
- Error handling strategy and result propagation
- Performance optimization techniques

### 1.3 Design Principles

The design follows these core principles:

#### 1.3.1 Hybrid Architecture
- **Domain-Driven Design (DDD)**: Rich domain models with behavior
- **Clean Architecture**: Dependency inversion and separation of concerns
- **Hexagonal Architecture**: Ports and adapters pattern for external integration

#### 1.3.2 Ada 2022 Best Practices
- Comprehensive contract usage (Pre, Post, Type_Invariant)
- Memory safety through ownership and RAII patterns
- Expression functions for performance and clarity
- Generic programming for type safety and reusability

#### 1.3.3 Error Handling Strategy
- Result pattern for all operations that can fail
- No exception propagation across architectural boundaries
- Detailed error context for debugging and monitoring
- Fail-fast design with comprehensive validation

---

## 2. System Architecture

### 2.1 Overall Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        Pipelib Library                         │
├─────────────────────────────────────────────────────────────────┤
│                     Core Domain Layer                          │
│  ┌─────────────────┐  ┌──────────────────┐  ┌────────────────┐  │
│  │  Value Objects  │  │    Entities      │  │   Services     │  │
│  │  • File_Chunk   │  │    • Chunk       │  │ • Progress     │  │
│  │  • Chunk_Size   │  │                  │  │   Tracker      │  │
│  │  • Algorithm    │  │                  │  │                │  │
│  └─────────────────┘  └──────────────────┘  └────────────────┘  │
├─────────────────────────────────────────────────────────────────┤
│                   Application Layer                            │
│  ┌─────────────────────────────────────────────────────────────┐  │
│  │           Parallel Chunk Processor                         │  │
│  │  • Worker Management    • Load Balancing                   │  │
│  │  • Error Aggregation    • Result Coordination             │  │
│  └─────────────────────────────────────────────────────────────┘  │
├─────────────────────────────────────────────────────────────────┤
│                   Infrastructure Layer                         │
│  ┌──────────────────┐  ┌──────────────────┐  ┌───────────────┐  │
│  │  Memory Mapped   │  │  Random Write    │  │   Chunk       │  │
│  │  File Adapter    │  │  File Handler    │  │   Adapter     │  │
│  └──────────────────┘  └──────────────────┘  └───────────────┘  │
└─────────────────────────────────────────────────────────────────┘
          │                      │                      │
          ▼                      ▼                      ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   File System   │    │  Memory System  │    │   Threading     │
│   Operations    │    │   Management    │    │   Runtime       │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

### 2.2 Layer Responsibilities

#### 2.2.1 Core Domain Layer
- **Value Objects**: Immutable data containers with validation logic
- **Entities**: Objects with identity and lifecycle management
- **Domain Services**: Complex business logic that doesn't belong to entities
- **Constants**: Centralized configuration values and thresholds in `Pipelib.Core.Domain.Constants`

#### 2.2.2 Application Layer
- **Use Cases**: Orchestrates domain objects to fulfill application requirements
- **Service Coordination**: Manages interactions between domain services
- **Transaction Management**: Ensures consistency across operations

#### 2.2.3 Infrastructure Layer
- **External Integrations**: File system, memory management, networking
- **Technology-Specific Implementations**: Memory mapping, concurrent I/O
- **Resource Management**: Cleanup, monitoring, and lifecycle management

### 2.3 Dependency Flow

```
Application Layer ────────┐
     │                    │
     ▼                    ▼
Core Domain Layer ◄── Ports (Interfaces)
     ▲                    │
     │                    ▼
Infrastructure Layer ── Adapters (Implementations)
```

- **Dependency Inversion**: High-level modules do not depend on low-level modules
- **Interface Segregation**: Clients depend only on interfaces they use
- **Dependency Injection**: Dependencies provided through constructors/parameters

---

## 3. Detailed Design

### 3.1 Package Organization

```ada
pipelib/
├── Core/
│   ├── Domain/
│   │   ├── Entities/
│   │   │   └── Chunk                    -- Chunk entity with state machine
│   │   ├── Value_Objects/
│   │   │   ├── File_Chunk              -- Immutable data chunk
│   │   │   ├── Chunk_Size              -- Type-safe size representation
│   │   │   └── Algorithm               -- Algorithm identifier
│   │   ├── Services/
│   │   │   └── Progress_Tracker        -- Progress monitoring service
│   │   └── Ports/
│   │       └── Memory_Mapped_File_Interface  -- File mapping abstraction
│   └── Application/
│       └── Services/
│           └── Parallel_Chunk_Processor -- Concurrent processing orchestrator
└── Infrastructure/
    └── IO/
        ├── Memory_Mapped_File          -- Memory mapping implementation
        ├── Memory_Mapped_Chunk_Adapter -- Chunk creation from mapped files
        └── Random_Write_File           -- Concurrent file writing
```

### 3.2 Contract Design Strategy

#### 3.2.1 Contract Types Used

```ada
-- Preconditions: Input validation
with Pre => Input /= null and then Input.all'Length > 0

-- Postconditions: Output guarantees
with Post => Result /= null and then Result.Is_Valid

-- Type Invariants: Always-true properties
type File_Chunk_Type is private
with Type_Invariant =>
  (if not Is_Empty (File_Chunk_Type) then Get_Data (File_Chunk_Type) /= null);

-- Dynamic Predicates: Subtype constraints
subtype Valid_Chunk_Size is Positive
with Dynamic_Predicate =>
  Valid_Chunk_Size >= Min_Chunk_Size and Valid_Chunk_Size <= Max_Chunk_Size;
  -- Where Min_Chunk_Size = 1KB and Max_Chunk_Size = 512MB from Domain.Constants
```

#### 3.2.2 Contract Patterns

1. **State Machine Validation**
```ada
function Set_State
  (Chunk : in out Chunk_Type;
   New_State : Chunk_State) return Boolean
with Pre => Is_Valid_Transition (Get_State (Chunk), New_State),
     Post => (if Set_State'Result then Get_State (Chunk) = New_State);
```

2. **Resource Management**
```ada
procedure Destroy (Processor : in out Parallel_Processor_Access)
with Post => Processor = null;
```

3. **Boundary Validation**
```ada
function Calculate_Optimal_Chunk_Size
  (File_Size : Storage_Count) return Positive
with Pre => File_Size > 0,
     Post => Calculate_Optimal_Chunk_Size'Result >= 1024 and then
             Calculate_Optimal_Chunk_Size'Result <= Integer (File_Size);
```

---

## 4. Component Design

### 4.1 File_Chunk Value Object

#### 4.1.1 Purpose
Represents an immutable chunk of file data with associated metadata, checksums, and processing state information.

#### 4.1.2 Interface Design
```ada
package Pipelib.Core.Domain.Value_Objects.File_Chunk is

   type File_Chunk_Type is private
   with Type_Invariant =>
     (if not Is_Empty (File_Chunk_Type) then
        Get_Data (File_Chunk_Type) /= null and then
        Get_Sequence_Number (File_Chunk_Type) >= 0);

   -- Creation functions
   function Create_Empty return File_Chunk_Type
   with Post => Is_Empty (Create_Empty'Result);

   function Create_Chunk
     (Data : Stream_Element_Array_Access;
      Sequence_Number : Natural;
      Is_Final : Boolean := False)
     return File_Chunk_Type
   with Pre => Data /= null and then Data.all'Length > 0,
        Post => not Is_Empty (Create_Chunk'Result) and then
                Get_Sequence_Number (Create_Chunk'Result) = Sequence_Number;

   -- Query functions
   function Is_Empty (Chunk : File_Chunk_Type) return Boolean;

   function Get_Data (Chunk : File_Chunk_Type)
     return Stream_Element_Array_Access
   with Post => (if Is_Empty (Chunk) then
                   Get_Data'Result = null
                 else
                   Get_Data'Result /= null);

   function Get_Sequence_Number (Chunk : File_Chunk_Type) return Natural
   with Post => (if Is_Empty (Chunk) then
                   Get_Sequence_Number'Result = 0);

end Pipelib.Core.Domain.Value_Objects.File_Chunk;
```

#### 4.1.3 Implementation Strategy

**Immutability:** Once created, chunk data cannot be modified. This ensures thread safety and prevents accidental data corruption.

**Memory Management:** Uses controlled types for automatic cleanup of dynamically allocated data.

**Checksum Validation:** Optional SHA256 checksums for data integrity verification.

### 4.2 Chunk Entity

#### 4.2.1 Purpose
Manages the lifecycle and state transitions of data chunks through the processing pipeline.

#### 4.2.2 State Machine Design
```
   ┌─────────┐
   │ Created │
   └────┬────┘
        │
        ▼
   ┌─────────┐    Retry    ┌──────────┐
   │ Reading │◄────────────┤  Error   │
   └────┬────┘             │ Recovery │
        │                  └──────────┘
        ▼                       ▲
   ┌─────────┐                  │
   │   Read  │──────────────────┘
   └────┬────┘
        │
        ▼
   ┌─────────────┐    Retry    ┌──────────┐
   │ Processing  │◄────────────┤  Error   │
   └─────┬───────┘             │ Recovery │
         │                     └──────────┘
         ▼                          ▲
   ┌─────────────┐                  │
   │  Processed  │──────────────────┘
   └─────┬───────┘
         │
         ▼
   ┌─────────────┐    Retry    ┌──────────┐
   │   Writing   │◄────────────┤  Error   │
   └─────┬───────┘             │ Recovery │
         │                     └──────────┘
         ▼
   ┌─────────────┐
   │   Written   │ (Terminal)
   └─────────────┘
```

#### 4.2.3 State Transition Contracts
```ada
function Is_Valid_Transition (From, To : Chunk_State) return Boolean
with Post => Is_Valid_Transition'Result =
             (case From is
                when Created => To in Reading | Read,
                when Reading => To in Read | Created,      -- Retry allowed
                when Read => To in Processing | Writing,
                when Processing => To in Processed | Read, -- Retry allowed
                when Processed => To in Writing,
                when Writing => To in Written | Processed, -- Retry allowed
                when Written => False);                    -- Terminal state
```

### 4.3 Parallel_Chunk_Processor

#### 4.3.1 Purpose
Orchestrates parallel processing of chunks using multiple worker tasks with load balancing and error aggregation.

#### 4.3.2 Architecture Design
```
                    ┌─────────────────────────────┐
                    │    Parallel Processor       │
                    │                             │
   Input Chunks ────┤  ┌───────────────────────┐  │──── Results
                    │  │     Work Queue        │  │
                    │  │  (Thread-Safe)        │  │
                    │  └───────────────────────┘  │
                    │            │                │
                    │            ▼                │
                    │  ┌─────┐ ┌─────┐ ┌─────┐   │
                    │  │ W1  │ │ W2  │ │ WN  │   │
                    │  └─────┘ └─────┘ └─────┘   │
                    │     │       │       │      │
                    │     ▼       ▼       ▼      │
                    │  ┌─────────────────────────┐ │
                    │  │   Random Write File     │ │
                    │  │   (Thread-Safe)         │ │
                    │  └─────────────────────────┘ │
                    └─────────────────────────────┘
```

#### 4.3.3 Generic Design
```ada
generic
   type Context_Type is private;
   with function Process_Chunk
     (Chunk : File_Chunk_Type;
      Context : Context_Type)
      return File_Chunk_Type;

package Pipelib.Core.Application.Services.Parallel_Chunk_Processor is

   type Parallel_Processor_Type is tagged limited private;

   function Create
     (Worker_Count : Positive;
      Output_File : Random_Write_File_Access;
      Context : Context_Type) return Parallel_Processor_Access
   with Pre => Worker_Count <= 64,
        Post => Create'Result /= null;

   procedure Submit_Chunk
     (Processor : in out Parallel_Processor_Type;
      Chunk : File_Chunk_Type)
   with Pre => Processor.Is_Running and then not Is_Empty (Chunk);

end Pipelib.Core.Application.Services.Parallel_Chunk_Processor;
```

#### 4.3.4 Worker Task Design
```ada
task type Worker_Task_Type (Parent : access Parallel_Processor_Type) is
   entry Start;
   entry Stop;
end Worker_Task_Type;

task body Worker_Task_Type is
   Work_Item : Work_Item_Type;
   Result : File_Chunk_Type;
begin
   accept Start;

   loop
      select
         accept Stop;
         exit;
      or
         Parent.Work_Queue.Dequeue (Work_Item);

         if Work_Item.Is_End_Marker then
            exit;
         end if;

         -- Process the chunk
         Result := Process_Chunk (Work_Item.Chunk, Parent.Context);

         -- Write result to output file
         Parent.Output_File.Write_Chunk (Result);

         -- Update statistics
         Parent.Statistics.Increment_Processed;
      end select;
   end loop;
exception
   when E : others =>
      Parent.Statistics.Set_Error (Exception_Information (E));
end Worker_Task_Type;
```

### 4.4 Memory_Mapped_File Operations

#### 4.4.1 Purpose
Provides efficient zero-copy access to large files through memory mapping with automatic optimization decisions.

#### 4.4.2 Decision Algorithm
```ada
function Should_Use_Memory_Mapping_For_File
  (File_Size : Storage_Count;
   Available_Memory : Storage_Count := 0) return Boolean
is
   -- Uses constants from Pipelib.Core.Domain.Constants:
   -- Min_Memory_Map_Size = 100MB
   -- Max_Memory_Map_Size = 1GB

   Available : Storage_Count := Available_Memory;
begin
   if Available = 0 then
      Available := Get_Available_Memory; -- System query
   end if;

   return File_Size >= Min_Mapping_Size
          and then File_Size <= Max_Mapping_Size
          and then File_Size <= Available / 2; -- Use max 50% of available
end Should_Use_Memory_Mapping_For_File;
```

#### 4.4.3 Zero-Copy Implementation
```ada
function Create_Stream_Array_Access_From_Memory
  (View : Memory_View;
   Offset : Storage_Count;
   Length : Storage_Count)
  return Stream_Element_Array_Access
is
   -- Use Address_To_Access_Conversions to create array access
   -- that points directly to memory-mapped region
   subtype Mapped_Array is Stream_Element_Array (1 .. Stream_Element_Offset (Length));
   type Mapped_Array_Access is access all Mapped_Array;

   function To_Access is new Address_To_Access_Conversions.To_Pointer
     (Object => Mapped_Array, Pointer => Mapped_Array_Access);

   Target_Address : constant System.Address :=
     View.Address + Storage_Offset (Offset);
begin
   return Stream_Element_Array_Access (To_Access (Target_Address));
end Create_Stream_Array_Access_From_Memory;
```

### 4.5 Domain Constants Package

#### 4.5.1 Purpose
Centralizes all configuration values, thresholds, and constants used throughout the pipeline system. This eliminates magic numbers and provides a single source of truth for system-wide values.

#### 4.5.2 Design Principles
- **Centralization**: All constants in one location for easy maintenance
- **Type Safety**: Uses Ada's strong typing for compile-time validation
- **Documentation**: Each constant includes clear comments explaining its purpose
- **Categorization**: Constants grouped by their functional area

#### 4.5.3 Constant Categories

**File Size Thresholds**
```ada
-- Uses typed byte counts for type safety
Small_File_Threshold  : constant Byte_Count_Type := 10 * MB;     -- 10MB
Medium_File_Threshold : constant Byte_Count_Type := 100 * MB;    -- 100MB
Large_File_Threshold  : constant Byte_Count_Type := GB;          -- 1GB
```

**Chunk Size Configuration**
```ada
-- All chunk sizes use typed byte counts to prevent confusion
Small_Chunk_Size  : constant Byte_Count_Type := 64 * KB;    -- For small files
Medium_Chunk_Size : constant Byte_Count_Type := MB;         -- For medium files
Large_Chunk_Size  : constant Byte_Count_Type := 4 * MB;     -- For large files
Huge_Chunk_Size   : constant Byte_Count_Type := 16 * MB;    -- For very large files

-- Boundaries for chunk size validation
Default_Chunk_Size : constant Byte_Count_Type := Small_Chunk_Size;  -- 64KB
Min_Chunk_Size     : constant Byte_Count_Type := KB;               -- 1KB minimum
Max_Chunk_Size     : constant Byte_Count_Type := 512 * MB;        -- 512MB maximum
```

**Worker Thread Limits**
```ada
-- Uses typed worker count values to enforce constraints
Max_Worker_Count       : constant Worker_Count_Value_Type := 256;   -- System maximum
Default_Worker_Count   : constant Worker_Count_Value_Type := 4;     -- Conservative default
Max_Worker_Count_Range : constant Worker_Count_Value_Type := 64;    -- For DTOs

-- Worker multiplier for CPU-based calculations
Max_Worker_Multiplier  : constant Worker_Multiplier_Type := 32;    -- For scaling
```

**Memory Mapping Thresholds**
```ada
-- Uses typed byte counts for memory mapping decisions
Min_Memory_Map_Size : constant Byte_Count_Type := 100 * MB;  -- Below this, use regular I/O
Max_Memory_Map_Size : constant Byte_Count_Type := GB;        -- Above this, risk address space
```

**Pipeline Configuration**
```ada
-- Typed values for queue management and display formatting
Default_Max_Queue_Depth : constant Queue_Depth_Type := 100;
Progress_Field_Width    : constant Field_Width_Type := 4;

-- Numeric thresholds for formatting (typed for clarity)
Progress_Threshold_Ten      : constant Numeric_Threshold_Type := 10;
Progress_Threshold_Hundred  : constant Numeric_Threshold_Type := 100;
Progress_Threshold_Thousand : constant Numeric_Threshold_Type := 1_000;
```

#### 4.5.4 Usage Example
```ada
-- In application code with type-safe constants
with Pipelib.Core.Domain.Constants;
use Pipelib.Core.Domain.Constants;

-- Adaptive chunk sizing based on file size (type-safe)
function Get_Optimal_Chunk_Size (File_Size : Byte_Count_Type) return Byte_Count_Type is
begin
   if File_Size <= Small_File_Threshold then
      return Small_Chunk_Size;    -- Both are Byte_Count_Type - type safe
   elsif File_Size <= Medium_File_Threshold then
      return Medium_Chunk_Size;
   else
      return Large_Chunk_Size;
   end if;
end Get_Optimal_Chunk_Size;

-- Progress tracking with typed counts (prevents parameter confusion)
procedure Update_Pipeline_Progress
  (Read_Count      : Read_Count_Type;      -- Cannot pass processed count here
   Processed_Count : Processed_Count_Type; -- Cannot pass read count here
   Written_Count   : Written_Count_Type)   -- Cannot pass other count types here
is
   Tracker : Progress_Tracker_Type;
begin
   Tracker.Update_Read_Count (Read_Count);          -- Type-safe call
   Tracker.Update_Processed_Count (Processed_Count); -- Type-safe call
   Tracker.Update_Written_Count (Written_Count);     -- Type-safe call
end Update_Pipeline_Progress;
```

#### 4.5.5 Benefits
- **Maintainability**: Change values in one place affects entire system
- **Type Safety**: Typed constants prevent accidental misuse or confusion
- **Clarity**: Named constants are self-documenting with semantic meaning
- **Consistency**: Ensures same values and types used throughout codebase
- **Compile-Time Validation**: Type constraints catch errors at compile time
- **Testing**: Easy to adjust for different test scenarios while maintaining type safety

---

## 5. Type Safety Design

### 5.1 Type Safety Overview

Pipelib implements a comprehensive type safety system that eliminates entire classes of bugs by using Ada's strong typing system to prevent parameter confusion and semantic errors. The system uses distinct types for different semantic concepts, ensuring that values cannot be accidentally mixed or misused.

### 5.2 Type Safety Principles

#### 5.2.1 Semantic Type Distinction
Rather than using generic types like `Natural` or `Long_Long_Integer` for different concepts, Pipelib leverages strong types from Abohlib and defines additional domain-specific types:

```ada
-- From Abohlib: Strong types for byte sizes
with Abohlib.Core.Domain.Types.Bytes; use Abohlib.Core.Domain.Types.Bytes;
Chunk_Size : SI_Bytes_Type := From_MB(16);  -- Type-safe byte measurements

-- Pipelib-specific: Prevent mixing different kinds of progress counts
type Read_Count_Type is new Natural;        -- Chunks read from input
type Processed_Count_Type is new Natural;   -- Chunks processed/transformed
type Written_Count_Type is new Natural;     -- Chunks written to output

-- Prevent confusion between positions and sequence numbers
type File_Position_Type is new Long_Long_Integer range 0 .. Long_Long_Integer'Last;
type Sequence_Number_Type is new Natural;

-- From Abohlib: Performance measurements with units
with Abohlib.Core.Domain.Types.Performance; use Abohlib.Core.Domain.Types.Performance;
Throughput : MB_Per_Second_Type;  -- Type-safe throughput
Progress : Percentage_Type;       -- Range-limited percentage (0.0 .. 100.0)
```

#### 5.2.2 Compile-Time Error Prevention
The type system prevents common programming errors at compile time:

```ada
declare
   Read_Count : Read_Count_Type := 42;
   Processed_Count : Processed_Count_Type := 35;
begin
   -- This would be a compile error - cannot mix different count types
   -- if Read_Count > Processed_Count then  -- COMPILE ERROR

   -- Must explicitly convert when intentional comparison is needed
   if Natural(Read_Count) > Natural(Processed_Count) then
      -- Safe comparison with explicit intent
   end if;
end;
```

#### 5.2.3 Self-Documenting Code
Types serve as documentation, making code more readable and maintainable:

```ada
procedure Update_Progress
  (Chunks_Read      : Read_Count_Type;      -- Clear intent: input chunks
   Chunks_Processed : Processed_Count_Type; -- Clear intent: processed chunks
   Chunks_Written   : Written_Count_Type)   -- Clear intent: output chunks
```

### 5.3 Type Categories

#### 5.3.1 Progress Tracking Types
These types prevent accidental mixing of different progress measurements:

```ada
type Read_Count_Type is new Natural;        -- Chunks read from input source
type Processed_Count_Type is new Natural;   -- Chunks that completed processing
type Written_Count_Type is new Natural;     -- Chunks written to output
type Error_Count_Type is new Natural;       -- Errors encountered during processing
```

**Usage Example:**
```ada
with Pipelib.Core.Domain.Constants.Count_Arithmetic;
use Pipelib.Core.Domain.Constants.Count_Arithmetic;

-- Progress tracker maintains separate typed counts
type Progress_State is record
   Chunks_Read      : Read_Count_Type := 0;
   Chunks_Processed : Processed_Count_Type := 0;
   Chunks_Written   : Written_Count_Type := 0;
   Errors           : Error_Count_Type := 0;
end record;

-- Safe arithmetic operations with count types
procedure Update_Progress (Progress : in out Progress_State) is
begin
   -- Increment operations
   Progress.Chunks_Read := Increment(Progress.Chunks_Read);

   -- Addition with same types
   Progress.Chunks_Processed := Progress.Chunks_Processed + Processed_Count_Type(1);

   -- Calculate totals requires explicit conversion
   Total : Natural := Natural(Progress.Chunks_Read) +
                      Natural(Progress.Chunks_Processed) +
                      Natural(Progress.Chunks_Written);
end Update_Progress;
```

#### 5.3.2 Position and Identification Types
These types prevent confusion between different positional concepts:

```ada
type File_Position_Type is new Long_Long_Integer range 0 .. Long_Long_Integer'Last;
type Sequence_Number_Type is new Natural;
type Chunk_Count_Type is new Natural;
type Chunk_Index_Type is new Positive;    -- 1-based indexing for display
```

**Usage Example:**
```ada
-- File chunk with typed position and sequence
type File_Chunk_Type is record
   Sequence_Number : Sequence_Number_Type;  -- Order in processing pipeline
   Offset          : File_Position_Type;    -- Byte position in source file
   -- ...
end record;
```

#### 5.3.3 Performance Measurement Types
These types ensure different performance metrics cannot be accidentally mixed:

```ada
type Processing_Time_Ms_Type is new Natural;
type Throughput_MBps_Type is new Float range 0.0 .. Float'Last;
```

**Usage Example:**
```ada
-- DTO with typed performance measurements
type Process_Chunk_Response is record
   Processing_Time_Ms : Processing_Time_Ms_Type;  -- Cannot be confused with counts
   -- ...
end record;
```

#### 5.3.4 Configuration Types
These types provide type safety for system configuration:

```ada
type Worker_Count_Value_Type is range 1 .. 256;
type Queue_Depth_Type is new Positive;
type Byte_Count_Type is new Storage_Count;
```

### 5.4 Type Safety Implementation Patterns

#### 5.4.1 Conversion Functions
Safe conversion between typed and untyped values when interfacing with external code:

```ada
-- From constants package
function To_Natural (Value : Read_Count_Type) return Natural is (Natural (Value));
function To_Natural (Value : Processed_Count_Type) return Natural is (Natural (Value));
function To_Natural (Value : Written_Count_Type) return Natural is (Natural (Value));
```

#### 5.4.2 Type-Safe Interfaces
All public interfaces use typed parameters to prevent confusion:

```ada
function Create_Process_Request
  (Data_Size       : Storage_Count;
   File_Position   : File_Position_Type;        -- Typed position
   Sequence_Number : Sequence_Number_Type := 0; -- Typed sequence
   Is_Final        : Boolean := False) return Process_Chunk_Request;
```

#### 5.4.3 Protected Type Operations
Thread-safe operations maintain type safety:

```ada
protected type Progress_Tracker_Type is
   procedure Update_Read_Count (New_Count : Read_Count_Type);
   procedure Update_Processed_Count (New_Count : Processed_Count_Type);
   procedure Update_Written_Count (New_Count : Written_Count_Type);
   -- Cannot accidentally call with wrong count type
end Progress_Tracker_Type;
```

### 5.5 Type Safety Benefits

#### 5.5.1 Bug Prevention
- **Parameter Confusion**: Cannot pass a sequence number where a file position is expected
- **Unit Confusion**: Cannot mix milliseconds with counts or bytes
- **Semantic Errors**: Cannot accidentally use read counts for write operations

#### 5.5.2 Code Documentation
- **Self-Describing**: Function signatures clearly indicate what each parameter represents
- **Intent Clarity**: Code reviewer can immediately understand data flow and meaning
- **Maintenance**: Changes to type definitions propagate throughout the system

#### 5.5.3 Compiler Assistance
- **Compile-Time Checking**: Most errors caught before runtime
- **IDE Support**: Better autocomplete and error highlighting
- **Refactoring Safety**: Type changes cause compilation errors where updates needed

#### 5.5.4 Testing Advantages
- **Mock Safety**: Cannot accidentally provide wrong data types in tests
- **Boundary Testing**: Type constraints define clear test boundaries
- **Contract Enforcement**: Type-based preconditions and postconditions

---

## 6. Interface Design

### 6.1 Generic Stage Interface

#### 6.1.1 Purpose
Provides a common interface for all pipeline processing stages, enabling composition and reusability.

#### 6.1.2 Interface Definition
```ada
generic
   type Input_Type is private;
   type Output_Type is private;
   type Context_Type is private;

package Pipelib.Core.Domain.Interfaces.Stage_Interface is

   type Stage_Interface is interface;

   function Process
     (Stage : Stage_Interface;
      Input : Input_Type;
      Context : Context_Type) return Output_Type is abstract
   with Pre'Class => Input /= Input_Type'Default,
        Post'Class => Output /= Output_Type'Default;

   function Get_Stage_Name (Stage : Stage_Interface) return String is abstract
   with Post'Class => Get_Stage_Name'Result'Length > 0;

   function Is_Ready (Stage : Stage_Interface) return Boolean is abstract;

end Pipelib.Core.Domain.Interfaces.Stage_Interface;
```

#### 6.1.3 Implementation Example
```ada
package Pipelib.Core.Domain.Services.Stages.Generic_Hasher_Stage is

   type Hasher_Stage_Type is new Stage_Interface with private;

   overriding
   function Process
     (Stage : Hasher_Stage_Type;
      Input : File_Chunk_Type;
      Context : Algorithm_Type) return File_Chunk_Type
   with Pre => not Is_Empty (Input) and then Is_Hash_Algorithm (Context),
        Post => not Is_Empty (Process'Result) and then
                Has_Checksum (Process'Result);

end Pipelib.Core.Domain.Services.Stages.Generic_Hasher_Stage;
```

### 5.2 Result Pattern Implementation

#### 5.2.1 Generic Result Type
```ada
generic
   type Ok_Type is private;
   type Err_Type is private;

package Abohlib.Core.Domain.Result.Result_Package is

   type Result is private;

   function Ok (Value : Ok_Type) return Result
   with Post => Is_Ok (Ok'Result);

   function Err (Error : Err_Type) return Result
   with Post => Is_Err (Err'Result);

   function Is_Ok (R : Result) return Boolean;
   function Is_Err (R : Result) return Boolean
   with Post => Is_Err'Result = not Is_Ok (R);

   function Unwrap (R : Result) return Ok_Type
   with Pre => Is_Ok (R);

   function Unwrap_Err (R : Result) return Err_Type
   with Pre => Is_Err (R);

private
   type Result_Tag is (Ok_Tag, Err_Tag);

   type Result (Tag : Result_Tag := Err_Tag) is record
      case Tag is
         when Ok_Tag => Ok_Value : Ok_Type;
         when Err_Tag => Err_Value : Err_Type;
      end case;
   end record;

end Abohlib.Core.Domain.Result.Result_Package;
```

#### 5.2.2 Error Handling Patterns
```ada
-- Operation that can fail
function Create_Chunks_From_File
  (Filename : String) return Chunk_Vector_Result.Result is
begin
   if not Ada.Directories.Exists (Filename) then
      return Chunk_Vector_Result.Err
        (To_Unbounded_String ("File not found: " & Filename));
   end if;

   -- Process file...
   return Chunk_Vector_Result.Ok (Chunks);
end Create_Chunks_From_File;

-- Error handling at call site
declare
   Result : constant Chunk_Vector_Result.Result := Create_Chunks_From_File ("data.bin");
begin
   if Chunk_Vector_Result.Is_Ok (Result) then
      declare
         Chunks : constant File_Chunk_Vector := Chunk_Vector_Result.Unwrap (Result);
      begin
         -- Process chunks...
      end;
   else
      Put_Line ("Error: " & To_String (Chunk_Vector_Result.Unwrap_Err (Result)));
      return;
   end if;
end;
```

---

## 7. Data Design

### 7.1 Memory Management Strategy

#### 7.1.1 Ownership Model
```ada
-- Clear ownership transfer
procedure Set_Data
  (Chunk : in out File_Chunk_Type;
   Data : Stream_Element_Array_Access)
with Pre => Data /= null,
     Post => Get_Data (Chunk) = Data;  -- Ownership transferred

-- Controlled types for automatic cleanup
type File_Chunk_Type is new Ada.Finalization.Controlled with private;

overriding
procedure Finalize (Object : in out File_Chunk_Type);
```

#### 7.1.2 Memory Pool Usage
```ada
-- Subpools for related allocations
package Chunk_Pools is new System.Storage_Pools.Subpools.Root_Storage_Pool_With_Subpools;

Chunk_Pool : Chunk_Pools.Root_Storage_Pool_With_Subpools_Type;
Processing_Subpool : Chunk_Pools.Subpool_Handle :=
  Chunk_Pools.Create_Subpool (Chunk_Pool);

-- Allocate chunks in processing subpool
type Chunk_Access is access File_Chunk_Type;
for Chunk_Access'Storage_Pool use Processing_Subpool;
```

### 7.2 Data Structure Optimization

#### 7.2.1 Chunk Vector Design
```ada
-- Efficient vector implementation for chunks
package File_Chunk_Vectors is new Ada.Containers.Vectors
  (Index_Type   => Natural,
   Element_Type => File_Chunk_Type,
   "="          => "=");

type File_Chunk_Vector is new File_Chunk_Vectors.Vector with null record;

-- Optimized for append operations and random access
```

#### 7.2.2 Memory Layout Optimization
```ada
-- Packed records for memory efficiency
type Chunk_Metadata is record
   Sequence_Number : Natural;
   Data_Size : Storage_Count;
   Checksum : Digest_Type;
   Is_Final : Boolean;
   State : Chunk_State;
end record
with Pack => True;

-- Alignment for performance
type Aligned_Buffer is new Stream_Element_Array
with Alignment => 64; -- Cache line alignment
```

---

## 8. Error Handling Design

### 8.1 Error Classification

#### 8.1.1 Error Categories
```ada
type Error_Category is
  (System_Error,          -- OS-level errors
   Validation_Error,      -- Input validation failures
   Resource_Error,        -- Memory/file handle exhaustion
   Contract_Error,        -- Pre/postcondition violations
   State_Error,           -- Invalid state transitions
   Concurrency_Error);    -- Thread synchronization issues

type Error_Record is record
   Category : Error_Category;
   Code : Natural;
   Message : Unbounded_String;
   Context : Unbounded_String;
   Timestamp : Time;
end record;
```

#### 8.1.2 Error Propagation
```ada
-- Error aggregation in parallel processing
protected type Error_Collector is
   procedure Add_Error (Error : Error_Record);
   function Has_Errors return Boolean;
   function Get_Errors return Error_Vector;
   procedure Clear_Errors;
private
   Errors : Error_Vector;
end Error_Collector;

-- Result chaining for operation sequences
function Chain_Operations return Final_Result is
   Intermediate : constant Step1_Result := Step1;
begin
   if Step1_Result.Is_Err (Intermediate) then
      return Final_Result.Err (Step1_Result.Unwrap_Err (Intermediate));
   end if;

   declare
      Value1 : constant Step1_Type := Step1_Result.Unwrap (Intermediate);
      Step2_Res : constant Step2_Result := Step2 (Value1);
   begin
      if Step2_Result.Is_Err (Step2_Res) then
         return Final_Result.Err (Step2_Result.Unwrap_Err (Step2_Res));
      end if;

      -- Continue chain...
   end;
end Chain_Operations;
```

### 8.2 Contract Violation Handling

#### 8.2.1 Development vs Production
```ada
-- Development mode: Detailed contract violation reporting
pragma Assertion_Policy (Check);

-- Production mode: Optimized with basic checking
pragma Assertion_Policy (Ignore);

-- Conditional compilation for debug information
procedure Report_Contract_Violation
  (Condition : String;
   Location : String := GNAT.Source_Info.Source_Location)
is
begin
   if Debug_Mode then
      Put_Line ("Contract violation: " & Condition & " at " & Location);
      -- Log to file, send to monitoring system, etc.
   end if;
end Report_Contract_Violation;
```

---

## 9. Concurrency Design

### 9.1 Thread Safety Mechanisms

#### 9.1.1 Protected Types
```ada
-- Thread-safe statistics collection
protected type Statistics_Type is
   procedure Increment_Processed;
   procedure Set_Error (Msg : Unbounded_String);
   function Get_Processed return Natural;
   function Get_Error return Unbounded_String;
   function Has_Error return Boolean;
private
   Processed_Count : Natural := 0;
   Error_Message : Unbounded_String;
end Statistics_Type;

-- Thread-safe work queue
package Work_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
  (Queue_Interfaces => Work_Queue_Interface);

type Work_Queue_Type is new Work_Queues.Queue with null record;
```

#### 9.1.2 Lock-Free Algorithms
```ada
-- Atomic counters for performance-critical paths
package Atomic_Counters is
   type Atomic_Counter is limited private;

   procedure Increment (Counter : in out Atomic_Counter);
   function Get_Value (Counter : Atomic_Counter) return Natural;

private
   type Atomic_Counter is limited record
      Value : Natural := 0;
   end record
   with Atomic => True;
end Atomic_Counters;
```

### 9.2 Task Communication Patterns

#### 9.2.1 Producer-Consumer
```ada
-- Work distribution pattern
task type Producer_Task is
   entry Start (Queue : Work_Queue_Access);
   entry Stop;
end Producer_Task;

task type Consumer_Task is
   entry Start (Queue : Work_Queue_Access; ID : Worker_ID);
   entry Stop;
end Consumer_Task;

-- Coordination through rendezvous
task body Coordinator is
   Workers : Worker_Array (1 .. Worker_Count);
begin
   -- Start all workers
   for I in Workers'Range loop
      Workers (I).Start (Work_Queue, I);
   end loop;

   -- Wait for completion signal
   accept All_Work_Complete;

   -- Stop all workers
   for Worker of Workers loop
      Worker.Stop;
   end loop;
end Coordinator;
```

#### 9.2.2 Error Propagation in Concurrent Context
```ada
-- Exception handling in tasks
task body Worker_Task is
   Work_Item : Work_Item_Type;
begin
   loop
      select
         accept Stop;
         exit;
      or
         Work_Queue.Dequeue (Work_Item);

         begin
            -- Process work item
            Process_Item (Work_Item);
         exception
            when E : others =>
               -- Convert exception to Result
               Error_Collector.Add_Error (
                 Error_Record'(
                   Category => System_Error,
                   Code => 1,
                   Message => To_Unbounded_String (Exception_Message (E)),
                   Context => To_Unbounded_String ("Worker " & Worker_ID'Image),
                   Timestamp => Clock));
         end;
      end select;
   end loop;
end Worker_Task;
```

---

## Appendices

### Appendix A: Performance Considerations

#### A.1 Memory Access Patterns
- Sequential access optimization for large files
- Cache-friendly data structure layouts
- Memory prefetching hints for predictable access patterns

#### A.2 Compiler Optimizations
- Inline pragma usage for hot paths
- Loop unrolling for known iteration counts
- Whole program optimization flags

### Appendix B: Testing Strategy

#### B.1 Contract Validation Tests
- 43 test functions across 7 test suites
- Comprehensive coverage of all contract specifications
- Integration with continuous integration pipeline

#### B.2 Concurrency Testing
- Race condition detection through stress testing
- Deadlock prevention verification
- Performance benchmarking under load

### Appendix C: Future Enhancements

#### C.1 SPARK Integration
- Formal verification of critical algorithms
- Mathematical proof of contract correctness
- Enhanced static analysis capabilities

#### C.2 Performance Monitoring
- Runtime contract overhead measurement
- Memory usage profiling and optimization
- Throughput benchmarking and regression detection

---

**Document Status:** Complete
**Approval:** Pending Review
**Next Review Date:** March 2025
