# Pipelib Architecture Guide

This comprehensive guide covers both the conceptual architecture and practical implementation details of Pipelib. It serves as the single source of truth for all architectural information.

## Table of Contents

### Part 1: Conceptual Architecture
1. [Introduction and Design Principles](#1-introduction-and-design-principles)
2. [System Architecture](#2-system-architecture)
3. [Package Organization](#3-package-organization)
4. [Contract Design Strategy](#4-contract-design-strategy)

### Part 2: Practical Implementation
5. [Design Patterns in Practice](#5-design-patterns-in-practice)
6. [Type Safety Architecture](#6-type-safety-architecture)
7. [Working with the Architecture](#7-working-with-the-architecture)
8. [Performance Architecture](#8-performance-architecture)

### Part 3: Component Specifications
9. [Core Components](#9-core-components)
10. [Infrastructure Components](#10-infrastructure-components)

### Part 4: Architecture Guidance
11. [Extension Points](#11-extension-points)
12. [Architecture Decision Records](#12-architecture-decision-records)
13. [Troubleshooting Architecture Issues](#13-troubleshooting-architecture-issues)

---

## Part 1: Conceptual Architecture

## 1. Introduction and Design Principles

### 1.1 Overview

Pipelib is a reusable pipeline components library implemented in Ada 2022, designed for high-performance, concurrent data processing. The architecture emphasizes type safety, memory efficiency, and comprehensive error handling using the Result pattern.

### 1.2 Design Principles

#### 1.2.1 Hybrid Architecture
- **Domain-Driven Design (DDD)**: Rich domain models with behavior encapsulation
- **Clean Architecture**: Dependency inversion and separation of concerns
- **Hexagonal Architecture**: Ports and adapters pattern for external integration

#### 1.2.2 Ada 2022 Best Practices
- Comprehensive contract usage (Pre, Post, Type_Invariant, Dynamic_Predicate)
- Memory safety through ownership and RAII patterns
- Expression functions for performance and clarity
- Generic programming for type safety and reusability
- Strong typing to prevent parameter confusion

#### 1.2.3 Error Handling Strategy
- Result pattern for all operations that can fail
- No exception propagation across architectural boundaries
- Detailed error context for debugging and monitoring
- Fail-fast design with comprehensive validation

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

## 3. Package Organization

```ada
pipelib/
├── Core/
│   ├── Domain/
│   │   ├── Constants/
│   │   │   └── pipelib-core-domain-constants.ads  -- Typed constants
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

## 4. Contract Design Strategy

### 4.1 Contract Types Used

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

### 4.2 Contract Patterns

#### 4.2.1 State Machine Validation
```ada
function Set_State
  (Chunk : in out Chunk_Type;
   New_State : Chunk_State) return Boolean
with Pre => Is_Valid_Transition (Get_State (Chunk), New_State),
     Post => (if Set_State'Result then Get_State (Chunk) = New_State);
```

#### 4.2.2 Resource Management
```ada
procedure Destroy (Processor : in out Parallel_Processor_Access)
with Post => Processor = null;
```

#### 4.2.3 Boundary Validation
```ada
function Calculate_Optimal_Chunk_Size
  (File_Size : Storage_Count) return Positive
with Pre => File_Size > 0,
     Post => Calculate_Optimal_Chunk_Size'Result >= 1024 and then
             Calculate_Optimal_Chunk_Size'Result <= Integer (File_Size);
```

---

## Part 2: Practical Implementation

## 5. Design Patterns in Practice

### 5.1 Repository Pattern with Result Types
The repository pattern abstracts data access while the Result pattern ensures error handling without exceptions:

```ada
-- Port definition (domain layer)
package File_Reader_Interface is
   function Read_Chunk(
      Reader : in out File_Reader;
      Offset : File_Position_Type;
      Size   : Natural
   ) return Chunk_Result.Result;
end File_Reader_Interface;

-- Infrastructure implementation
package body Memory_Mapped_File is
   function Read_Chunk(...) return Chunk_Result.Result is
   begin
      if not Is_Valid_Offset(Offset) then
         return Chunk_Result.Err("Invalid offset: " & Offset'Image);
      end if;

      -- Memory-mapped read (zero-copy)
      declare
         Data : constant Stream_Element_Array_Access :=
            Map_Region(File, Offset, Size);
      begin
         return Chunk_Result.Ok(Create_Chunk(Data));
      exception
         when E : others =>
            return Chunk_Result.Err(Exception_Message(E));
      end;
   end Read_Chunk;
end Memory_Mapped_File;
```

### 5.2 Factory Pattern with Validation
Factories ensure objects are created in a valid state:

```ada
-- Generic factory for pipeline stages
generic
   type Stage_Type is new Stage_Interface with private;
   with function Validate_Config(Config : JSON_Value) return Boolean;
package Stage_Factory is

   function Create_Stage(Config : JSON_Value) return Stage_Access
   with Pre => Validate_Config(Config),
        Post => Create_Stage'Result /= null
                and then Create_Stage'Result.Is_Ready;

   -- Registry for dynamic stage creation
   procedure Register_Stage(
      Name : String;
      Constructor : Stage_Constructor_Access
   );
end Stage_Factory;

-- Usage
Factory.Register_Stage("sha256", SHA256_Stage_Constructor'Access);
Factory.Register_Stage("compress", Compression_Stage_Constructor'Access);

-- Dynamic creation from config
Stage := Factory.Create_Stage(Config);
```

### 5.3 State Machine with Contract Enforcement
The chunk state machine uses contracts to prevent invalid transitions:

```ada
-- State transition validation as a contract
function Is_Valid_Transition(From, To : Chunk_State) return Boolean is
   ((case From is
      when Created    => To in Reading | Read,
      when Reading    => To in Read | Created,      -- Retry allowed
      when Read       => To in Processing | Writing, -- Can skip processing
      when Processing => To in Processed | Read,     -- Retry allowed
      when Processed  => To in Writing,
      when Writing    => To in Written | Processed,  -- Retry allowed
      when Written    => False));                    -- Terminal state

-- Enforced in state setter
procedure Set_State(Chunk : in out Chunk_Type; New_State : Chunk_State)
with Pre => Is_Valid_Transition(Chunk.State, New_State),
     Post => Chunk.State = New_State;
```

### 5.4 Template Method for Pipeline Stages
Generic stages provide a template for custom processing:

```ada
generic
   type Input_Type is private;
   type Output_Type is private;
   with function Transform(Input : Input_Type) return Output_Type;
package Generic_Pipeline_Stage is

   type Stage_Type is new Stage_Interface with private;

   -- Template method
   overriding function Process(
      Stage : in out Stage_Type;
      Chunk : Chunk_Type
   ) return Result_Type is
   begin
      -- Pre-processing (metrics, validation)
      Stage.Items_Processed := @ + 1;
      Stage.Bytes_Processed := @ + Chunk.Size;

      if not Validate_Input(Chunk) then
         return Result.Err("Invalid input");
      end if;

      -- Core transformation (provided by generic)
      declare
         Input  : constant Input_Type := Extract_Input(Chunk);
         Output : constant Output_Type := Transform(Input);
      begin
         -- Post-processing (update chunk)
         return Result.Ok(Update_Chunk(Chunk, Output));
      exception
         when E : others =>
            Stage.Errors := @ + 1;
            return Result.Err(Exception_Message(E));
      end;
   end Process;
end Generic_Pipeline_Stage;
```

## 6. Type Safety Architecture

Pipelib implements comprehensive type safety to prevent parameter confusion and semantic errors. This section demonstrates practical patterns for leveraging Ada's type system effectively.

### 6.1 Semantic Type Distinction Strategy

Rather than using raw `Natural` or `Long_Long_Integer` for all numeric values, Pipelib creates distinct types for different concepts:

```ada
-- Progress tracking types prevent accidental mixing
type Read_Count_Type is new Natural;
type Processed_Count_Type is new Natural;
type Written_Count_Type is new Natural;

-- Position types prevent confusion between different location concepts
type File_Position_Type is new Long_Long_Integer range 0 .. Long_Long_Integer'Last;
type Sequence_Number_Type is new Natural;

-- Performance measurement types
type Processing_Time_Ms_Type is new Natural;
type Throughput_MBps_Type is new Float range 0.0 .. Float'Last;
```

### 6.2 Compile-Time Safety Enforcement

The type system prevents nonsensical operations at compile time:

```ada
procedure Update_Progress_Safe
  (Read_Count    : Read_Count_Type;
   Written_Count : Written_Count_Type)
is
begin
   -- This would cause a compile error (prevents parameter confusion):
   -- if Read_Count > Written_Count then  -- COMPILE ERROR: different types

   -- Explicit conversion required for intentional comparison:
   if Natural(Read_Count) > Natural(Written_Count) then
      -- Handle case where more chunks read than written
      Log_Warning("Read ahead of write by " &
                  Natural(Natural(Read_Count) - Natural(Written_Count))'Image);
   end if;
end Update_Progress_Safe;
```

### 6.3 Type-Safe Interface Design

All public interfaces use semantic types to ensure correctness:

```ada
-- DTOs with typed fields prevent parameter confusion
type Process_Chunk_Request is record
   File_Position   : File_Position_Type;        -- Cannot confuse with sequence
   Sequence_Number : Sequence_Number_Type := 0; -- Cannot confuse with position
   Data_Size       : Storage_Count;             -- Size information
   Is_Final        : Boolean := False;
end record;

-- Functions with typed parameters
function Create_Progress_Report
  (Chunks_Read      : Read_Count_Type;
   Chunks_Processed : Processed_Count_Type;
   Chunks_Written   : Written_Count_Type;
   Processing_Time  : Processing_Time_Ms_Type) return Progress_Report_Type;
```

### 6.4 Protected Type Safety

Progress tracking uses distinct types to prevent accidental mixing:

```ada
protected type Progress_Tracker_Type is
   -- Each procedure only accepts the semantically correct type
   procedure Update_Read_Count (New_Count : Read_Count_Type);
   procedure Update_Processed_Count (New_Count : Processed_Count_Type);
   procedure Update_Written_Count (New_Count : Written_Count_Type);

   -- Getters return typed values
   function Get_Read_Count return Read_Count_Type;
   function Get_Processed_Count return Processed_Count_Type;
   function Get_Written_Count return Written_Count_Type;

   -- Type-safe completion detection
   function Is_All_Complete
     (Total_Expected : Read_Count_Type) return Boolean;

private
   Chunks_Read      : Read_Count_Type := 0;
   Chunks_Processed : Processed_Count_Type := 0;
   Chunks_Written   : Written_Count_Type := 0;
end Progress_Tracker_Type;
```

### 6.5 Conversion Patterns for External Interfaces

When interfacing with external code that uses untyped values, provide explicit conversion functions:

```ada
-- Type-safe conversion utilities
package Type_Conversions is
   function To_File_Position (Value : Natural) return File_Position_Type
   with Pre => Value >= 0;

   function To_Sequence_Number (Value : Natural) return Sequence_Number_Type
   with Pre => Value >= 0;

   function From_File_Position (Position : File_Position_Type) return Natural
   with Post => From_File_Position'Result >= 0;

   function From_Sequence_Number (Sequence : Sequence_Number_Type) return Natural;
end Type_Conversions;

-- Usage in external interfaces
procedure Handle_External_Request (Raw_Position : Natural; Raw_Sequence : Natural) is
   Position : constant File_Position_Type := To_File_Position (Raw_Position);
   Sequence : constant Sequence_Number_Type := To_Sequence_Number (Raw_Sequence);
begin
   Process_Chunk_Request (Position => Position, Sequence => Sequence);
end Handle_External_Request;
```

### 6.6 Type Safety in Generic Programming

Generic packages maintain type safety across instantiations:

```ada
generic
   type Count_Type is new Natural;
   type Item_Type is private;
package Typed_Container is
   type Container_Type is tagged private;

   procedure Add_Item (Container : in out Container_Type; Item : Item_Type);
   function Get_Count (Container : Container_Type) return Count_Type;

   -- Type safety maintained through generic contract
   function Get_Item (Container : Container_Type; Index : Count_Type) return Item_Type
   with Pre => Index <= Get_Count (Container);

private
   type Item_Array is array (Count_Type range <>) of Item_Type;
   type Container_Type is tagged record
      Items : Item_Array (1 .. 1000);
      Count : Count_Type := 0;
   end record;
end Typed_Container;

-- Type-safe instantiations
package Read_Count_Container is new Typed_Container (
   Count_Type => Read_Count_Type,
   Item_Type => File_Chunk_Type
);

package Processed_Count_Container is new Typed_Container (
   Count_Type => Processed_Count_Type,
   Item_Type => Processed_Chunk_Type
);
```

### 6.7 Design Benefits and Trade-offs

**Benefits of Type Safety Architecture:**
- **Compile-time error detection**: Parameter confusion caught at build time
- **Self-documenting interfaces**: Function signatures clearly show semantic intent
- **Refactoring safety**: Type changes propagate through codebase automatically
- **Domain modeling accuracy**: Types reflect real-world concepts precisely

**Trade-offs:**
- **Conversion overhead**: Explicit conversions required when interfacing with external code
- **Verbosity**: More type declarations and conversion functions needed
- **Learning curve**: Developers must understand semantic type distinctions

### 6.8 Migration from Untyped Code

When converting existing untyped code to use semantic types:

1. **Identify semantic concepts**: Group related numeric values by meaning
2. **Create distinct types**: Define new types for each semantic concept
3. **Update interfaces gradually**: Start with public APIs, work inward
4. **Add conversion functions**: Provide explicit conversions for compatibility
5. **Validate with tests**: Ensure no behavioral changes during migration

```ada
-- Before: Parameter confusion possible
procedure Process_File_Old (
   Position : Natural;     -- Could accidentally pass sequence number
   Sequence : Natural      -- Could accidentally pass file position
);

-- After: Type safety enforced
procedure Process_File_New (
   Position : File_Position_Type;   -- Only accepts file positions
   Sequence : Sequence_Number_Type  -- Only accepts sequence numbers
);
```

## 7. Working with the Architecture

### 7.1 Layer Responsibilities

#### When Working in Domain Layer
✅ **DO:**
- Define business rules and invariants
- Use value objects for immutable concepts
- Create rich domain models with behavior
- Define ports (interfaces) for external dependencies

❌ **DON'T:**
- Import from infrastructure or application layers
- Use external libraries directly
- Handle technical concerns (files, network, etc.)

```ada
-- Good: Domain service with business logic
package Progress_Tracker is
   procedure Update_Progress(
      Tracker : in out Tracker_Type;
      Stage   : Stage_Kind;
      Count   : Positive
   )
   with Pre => not Is_Complete(Tracker, Stage),
        Post => Progress_Count(Tracker, Stage) =
                Progress_Count(Tracker, Stage)'Old + Count;
end Progress_Tracker;

-- Bad: Domain depending on infrastructure
package Chunk_Entity is
   with Memory_Mapped_File; -- WRONG! Domain shouldn't know infrastructure
end Chunk_Entity;
```

#### When Working in Application Layer
✅ **DO:**
- Orchestrate domain objects
- Implement use cases
- Manage transactions
- Coordinate between ports

❌ **DON'T:**
- Implement business rules (belongs in domain)
- Depend on infrastructure implementations
- Expose infrastructure details to presentation

```ada
-- Good: Application service orchestrating domain
procedure Process_File(
   Input_Path  : String;
   Output_Path : String;
   Pipeline    : Pipeline_Type
) is
   Reader : File_Reader_Interface'Class := Create_Reader(Input_Path);
   Writer : File_Writer_Interface'Class := Create_Writer(Output_Path);
   Tracker : Progress_Tracker.Tracker_Type;
begin
   -- Orchestrate domain objects
   while Reader.Has_More loop
      declare
         Chunk : Chunk_Type := Reader.Read_Next;
      begin
         Pipeline.Process(Chunk);
         Writer.Write(Chunk);
         Tracker.Update(Writing, 1);
      end;
   end loop;
end Process_File;
```

### 7.2 Dependency Injection Patterns

```ada
-- Constructor injection
generic
   type Reader_Type is new File_Reader_Interface with private;
   type Writer_Type is new File_Writer_Interface with private;
package Pipeline_Processor is
   type Processor_Type is tagged record
      Reader : access Reader_Type;
      Writer : access Writer_Type;
   end record;

   function Create(
      Reader : access Reader_Type;
      Writer : access Writer_Type
   ) return Processor_Type;
end Pipeline_Processor;

-- Interface injection for testability
procedure Process_With_Mocks is
   Mock_Reader : aliased Mock_File_Reader := Create_Mock_Reader(Test_Data);
   Mock_Writer : aliased Mock_File_Writer := Create_Mock_Writer;
   Processor : Processor_Type := Create(
      Reader => Mock_Reader'Access,
      Writer => Mock_Writer'Access
   );
begin
   Processor.Process_File("dummy.txt");
   Assert(Mock_Writer.Write_Count = Expected_Chunks);
end Process_With_Mocks;
```

## 8. Performance Architecture

### 8.1 Memory Management Strategy

#### Stack vs Heap Allocation
```ada
-- Prefer stack allocation for small, fixed-size data
procedure Process_Small_Chunk is
   Data : Stream_Element_Array(1 .. 4096); -- Stack allocated
begin
   Read_Into(File, Data);
   Process(Data);
end Process_Small_Chunk;

-- Use heap with controlled types for large or variable data
type Large_Buffer is new Ada.Finalization.Controlled with record
   Data : Stream_Element_Array_Access;
   Size : Natural;
end record;

overriding procedure Initialize(Buffer : in out Large_Buffer) is
begin
   Buffer.Data := new Stream_Element_Array(1 .. Default_Size);
   Buffer.Size := Default_Size;
end Initialize;

overriding procedure Finalize(Buffer : in out Large_Buffer) is
begin
   Free(Buffer.Data);
end Finalize;
```

#### Memory Pool Pattern
```ada
-- Chunk pool to reduce allocation overhead
package Chunk_Pool is
   type Pool_Type(Capacity : Positive) is tagged limited private;

   function Acquire(Pool : in out Pool_Type) return Chunk_Access
   with Post => Acquire'Result /= null;

   procedure Release(Pool : in out Pool_Type; Chunk : in out Chunk_Access)
   with Pre => Chunk /= null,
        Post => Chunk = null;

private
   type Pool_Type(Capacity : Positive) is tagged limited record
      Available : Chunk_Vectors.Vector;
      In_Use    : Natural := 0;
      Lock      : Read_Write_Lock;
   end record;
end Chunk_Pool;

-- Usage reduces allocation from O(n) to O(1)
Pool : Chunk_Pool.Pool_Type(Capacity => Worker_Count * 2);

-- In worker task
loop
   Chunk : Chunk_Access := Pool.Acquire;
   Process(Chunk.all);
   Pool.Release(Chunk); -- Chunk recycled, not deallocated
end loop;
```

### 8.2 Concurrency Patterns

#### Lock-Free Progress Tracking
```ada
-- Atomic operations for progress updates
package Progress_Tracker is
   type Atomic_Counter is limited record
      Value : aliased Interfaces.C.Atomic_Long;
   end record;

   procedure Increment(Counter : in out Atomic_Counter; By : Natural := 1)
   with Inline;

   function Value(Counter : Atomic_Counter) return Long_Long_Integer
   with Inline;
end Progress_Tracker;

package body Progress_Tracker is
   procedure Increment(Counter : in out Atomic_Counter; By : Natural := 1) is
   begin
      System.Atomic_Primitives.Atomic_Add(
         Counter.Value'Access,
         Interfaces.C.long(By)
      );
   end Increment;
end Progress_Tracker;
```

#### Work Stealing Queue
```ada
-- Efficient work distribution among threads
generic
   type Work_Item is private;
package Work_Stealing_Queue is
   type Queue_Type is tagged limited private;

   procedure Push(Queue : in out Queue_Type; Item : Work_Item);

   -- Try to pop from own queue (LIFO for cache locality)
   function Pop(Queue : in out Queue_Type) return Work_Item;

   -- Try to steal from another queue (FIFO for fairness)
   function Steal(Queue : in out Queue_Type) return Work_Item;

private
   type Queue_Type is tagged limited record
      Top    : Atomic_Integer;
      Bottom : Atomic_Integer;
      Buffer : Work_Item_Array_Access;
   end record;
end Work_Stealing_Queue;
```

### 8.3 Cache Optimization

```ada
-- Align data structures to cache lines
type Cache_Aligned_Chunk is record
   -- Frequently accessed together (same cache line)
   State : Chunk_State;
   Number : Sequence_Number_Type;
   Size : Natural;
   Retry_Count : Natural;

   -- Padding to cache line boundary
   Padding : Stream_Element_Array(1 .. 48);

   -- Less frequently accessed (different cache line)
   Data : Stream_Element_Array_Access;
   Metadata : Metadata_Access;
end record
with Alignment => 64; -- Cache line size
```

---

## Part 3: Component Specifications

## 9. Core Components

### 9.1 File_Chunk Value Object

#### Purpose
Represents an immutable chunk of file data with associated metadata, checksums, and processing state information.

#### Interface Design
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
      Sequence_Number : Sequence_Number_Type;
      File_Position : File_Position_Type;
      Is_Final : Boolean := False)
     return File_Chunk_Type
   with Pre => Data /= null and then Data.all'Length > 0,
        Post => not Is_Empty (Create_Chunk'Result) and then
                Get_Sequence_Number (Create_Chunk'Result) = Sequence_Number;

   -- Query functions
   function Is_Empty (Chunk : File_Chunk_Type) return Boolean;
   function Get_Data (Chunk : File_Chunk_Type) return Stream_Element_Array_Access;
   function Get_Sequence_Number (Chunk : File_Chunk_Type) return Sequence_Number_Type;
   function Get_File_Position (Chunk : File_Chunk_Type) return File_Position_Type;
   function Is_Final (Chunk : File_Chunk_Type) return Boolean;

   -- Checksum operations
   function Calculate_Checksum (Chunk : File_Chunk_Type) return String
   with Pre => not Is_Empty (Chunk);

   function Verify_Checksum (Chunk : File_Chunk_Type; Expected : String) return Boolean
   with Pre => not Is_Empty (Chunk);

private
   type File_Chunk_Type is record
      Data            : Stream_Element_Array_Access;
      Sequence_Number : Sequence_Number_Type;
      File_Position   : File_Position_Type;
      Is_Final        : Boolean;
      Checksum        : Unbounded_String;
   end record;
end File_Chunk;
```

### 9.2 Progress_Tracker Service

#### Purpose
Thread-safe service for tracking multi-stage pipeline progress with typed counters.

#### Interface Design
```ada
package Pipelib.Core.Domain.Services.Progress_Tracker is

   protected type Progress_Tracker_Type is
      -- Update procedures (thread-safe)
      procedure Update_Read_Count (New_Count : Read_Count_Type);
      procedure Update_Processed_Count (New_Count : Processed_Count_Type);
      procedure Update_Written_Count (New_Count : Written_Count_Type);

      -- Query functions (thread-safe)
      function Get_Read_Count return Read_Count_Type;
      function Get_Processed_Count return Processed_Count_Type;
      function Get_Written_Count return Written_Count_Type;

      -- Progress percentage calculations
      function Read_Percentage (Total : Read_Count_Type) return Float
      with Pre => Total > 0;

      function Processing_Percentage return Float;
      function Writing_Percentage return Float;

      -- Completion checks
      function Is_All_Complete (Total_Expected : Read_Count_Type) return Boolean;

   private
      Chunks_Read      : Read_Count_Type := 0;
      Chunks_Processed : Processed_Count_Type := 0;
      Chunks_Written   : Written_Count_Type := 0;
   end Progress_Tracker_Type;

end Progress_Tracker;
```

### 9.3 Parallel_Chunk_Processor

#### Purpose
Orchestrates concurrent chunk processing with worker management and error aggregation.

#### Interface Design
```ada
package Pipelib.Core.Application.Services.Parallel_Chunk_Processor is

   type Processor_Config is record
      Worker_Count    : Worker_Count_Value_Type;
      Queue_Size      : Positive := 1000;
      Retry_Limit     : Natural := 3;
      Retry_Delay_Ms  : Natural := 100;
   end record;

   type Processor_Type is tagged limited private;

   function Create
     (Config : Processor_Config;
      Pipeline : Pipeline_Type'Class) return Processor_Type;

   procedure Process_File
     (Processor : in out Processor_Type;
      Input_Path : String;
      Output_Path : String)
   with Pre => Is_Valid_Path (Input_Path) and then
               Is_Valid_Path (Output_Path);

   function Get_Progress (Processor : Processor_Type) return Progress_Info;

   procedure Shutdown (Processor : in out Processor_Type)
   with Post => not Is_Running (Processor);

private
   type Worker_Task;
   type Worker_Task_Access is access Worker_Task;

   type Processor_Type is tagged limited record
      Config   : Processor_Config;
      Pipeline : Pipeline_Access;
      Workers  : Worker_Array;
      Queue    : Chunk_Queue.Queue_Type;
      Tracker  : Progress_Tracker.Progress_Tracker_Type;
   end record;
end Parallel_Chunk_Processor;
```

## 10. Infrastructure Components

### 10.1 Memory_Mapped_File

#### Purpose
Provides efficient file access through memory mapping with zero-copy operations.

#### Interface Design
```ada
package Pipelib.Infrastructure.IO.Memory_Mapped_File is

   type Memory_Mapped_File_Type is new File_Reader_Interface with private;

   overriding function Open
     (File : in out Memory_Mapped_File_Type;
      Path : String) return Result_Type;

   overriding function Read_Chunk
     (File : in out Memory_Mapped_File_Type;
      Position : File_Position_Type;
      Size : Natural) return Chunk_Result_Type;

   overriding procedure Close (File : in out Memory_Mapped_File_Type);

   -- Memory mapping specific operations
   function Get_Page_Size return Natural;

   function Advise_Sequential
     (File : Memory_Mapped_File_Type) return Result_Type;

private
   type Memory_Mapped_File_Type is new File_Reader_Interface with record
      File_Descriptor : File_Descriptor_Type;
      Mapped_Address  : System.Address;
      File_Size       : File_Position_Type;
      Is_Open         : Boolean := False;
   end record;
end Memory_Mapped_File;
```

### 10.2 Random_Write_File

#### Purpose
Enables concurrent, out-of-order chunk writing with thread safety.

#### Interface Design
```ada
package Pipelib.Infrastructure.IO.Random_Write_File is

   type Random_Write_File_Type is new File_Writer_Interface with private;

   overriding function Open
     (File : in out Random_Write_File_Type;
      Path : String) return Result_Type;

   overriding function Write_Chunk
     (File : in out Random_Write_File_Type;
      Chunk : File_Chunk_Type) return Result_Type
   with Pre => not Is_Empty (Chunk);

   overriding procedure Close (File : in out Random_Write_File_Type);

   -- Random access specific
   function Preallocate
     (File : in out Random_Write_File_Type;
      Size : File_Position_Type) return Result_Type
   with Pre => Is_Open (File);

private
   protected type Write_Synchronizer is
      procedure Write_At
        (Position : File_Position_Type;
         Data : Stream_Element_Array;
         Success : out Boolean);
   private
      File_Handle : File_Type;
   end Write_Synchronizer;

   type Random_Write_File_Type is new File_Writer_Interface with record
      Synchronizer : Write_Synchronizer;
      Is_Open      : Boolean := False;
   end record;
end Random_Write_File;
```

---

## Part 4: Architecture Guidance

## 11. Extension Points

### 11.1 Adding New Processing Stages

1. **Define the transformation**:
```ada
function My_Transform(Input : Stream_Element_Array) return Stream_Element_Array is
begin
   -- Your transformation logic
   return Transformed_Data;
end My_Transform;
```

2. **Instantiate generic stage**:
```ada
package My_Stage is new Generic_Pipeline_Stage(
   Input_Type => Stream_Element_Array,
   Output_Type => Stream_Element_Array,
   Transform => My_Transform
);
```

3. **Register with factory**:
```ada
Pipeline_Factory.Register_Stage(
   Name => "my_transform",
   Constructor => My_Stage.Create'Access
);
```

### 11.2 Adding New I/O Backends

1. **Implement the port interface**:
```ada
type S3_File_Reader is new File_Reader_Interface with record
   Bucket : Unbounded_String;
   Client : AWS_S3_Client_Access;
end record;

overriding function Read_Chunk(
   Reader : in out S3_File_Reader;
   Offset : File_Position_Type;
   Size   : Natural
) return Chunk_Result.Result;
```

2. **Register with factory**:
```ada
IO_Factory.Register_Reader(
   Scheme => "s3://",
   Constructor => S3_File_Reader_Constructor'Access
);
```

### 11.3 Custom Progress Reporters

```ada
-- Define custom reporter interface
type Progress_Reporter is interface;

procedure Report(
   Reporter : Progress_Reporter;
   Progress : Progress_Info
) is abstract;

-- Implement various reporters
type Console_Reporter is new Progress_Reporter with null record;
type Web_Reporter is new Progress_Reporter with record
   Endpoint : Unbounded_String;
end record;

type Metrics_Reporter is new Progress_Reporter with record
   Client : Prometheus_Client_Access;
end record;
```

## 12. Architecture Decision Records

### ADR-001: Result Type Over Exceptions
**Status**: Accepted
**Context**: Need consistent error handling across architectural boundaries
**Decision**: Use Result<T,E> pattern instead of exceptions
**Consequences**:
- ✅ Explicit error handling
- ✅ No hidden control flow
- ✅ Better for contracts
- ❌ More verbose code

### ADR-002: Zero-Copy Operations
**Status**: Accepted
**Context**: Large file processing requires memory efficiency
**Decision**: Transfer ownership instead of copying data
**Consequences**:
- ✅ O(1) memory operations
- ✅ Better cache utilization
- ❌ More complex ownership tracking
- ❌ Requires careful lifetime management

### ADR-003: Generic Stages Over Inheritance
**Status**: Accepted
**Context**: Need extensible stage implementations
**Decision**: Use generic packages instead of type hierarchies
**Consequences**:
- ✅ Compile-time optimization
- ✅ Better performance
- ✅ Type safety
- ❌ More instantiation boilerplate

### ADR-004: Semantic Type Safety
**Status**: Accepted
**Context**: Prevent parameter confusion bugs in large codebase
**Decision**: Use distinct types for different semantic concepts
**Consequences**:
- ✅ Compile-time error detection for parameter confusion
- ✅ Self-documenting interfaces
- ✅ Better domain modeling
- ❌ More verbose code with explicit conversions
- ❌ Learning curve for developers

### ADR-005: Type Naming Conventions
**Status**: Accepted
**Context**: Need consistent naming for semantic types
**Decision**: Append `_Type` suffix to all distinct semantic types
**Consequences**:
- ✅ Clear distinction between types and values
- ✅ Consistent codebase conventions
- ✅ Easier code navigation and understanding
- ❌ Slightly longer type names

## 13. Troubleshooting Architecture Issues

### 13.1 Layer Violations

**Symptom**: Compilation errors about missing dependencies
**Diagnosis**: Check with statements for cross-layer imports
```bash
# Find potential violations
grep -r "with.*Infrastructure" src/pipelib/core/domain/
```

**Fix**: Introduce interface in domain layer:
```ada
-- Instead of domain depending on infrastructure
with Pipelib.Infrastructure.IO.Memory_Mapped_File; -- WRONG!

-- Define interface in domain
package File_Reader_Interface is
   type File_Reader is interface;
   -- methods
end File_Reader_Interface;

-- Implement in infrastructure
type Memory_Mapped_File is new File_Reader_Interface.File_Reader with ...
```

### 13.2 Performance Bottlenecks

**Symptom**: Lower than expected throughput
**Diagnosis**: Profile with gprof
```bash
gprof ./pipeline_benchmark | head -20
```

**Common Fixes**:

1. **Allocation overhead**: Use chunk pooling
2. **Lock contention**: Use lock-free structures
3. **Cache misses**: Align data structures
4. **False sharing**: Pad to cache line boundaries

### 13.3 Memory Leaks

**Symptom**: Growing memory usage
**Diagnosis**: Use valgrind
```bash
valgrind --leak-check=full ./pipeline_app
```

**Fix Pattern**:
```ada
-- Always use controlled types for dynamic memory
type Safe_Buffer is new Ada.Finalization.Controlled with record
   Data : Stream_Element_Array_Access;
end record;

overriding procedure Finalize(Buffer : in out Safe_Buffer) is
begin
   if Buffer.Data /= null then
      Free(Buffer.Data);
   end if;
end Finalize;
```

### 13.4 Contract Violations

**Symptom**: Assertion_Error at runtime
**Diagnosis**: Enable full contract checking
```ada
pragma Assertion_Policy(Check);
```

**Fix**: Review preconditions and fix caller:
```ada
-- If seeing: failed precondition from chunk_size.ads:45
-- Check the precondition:
-- Pre => Bytes >= MIN_CHUNK_SIZE and Bytes <= MAX_CHUNK_SIZE

-- Fix the caller:
if Size >= MIN_CHUNK_SIZE and Size <= MAX_CHUNK_SIZE then
   Chunk_Size := Create(Size);
else
   -- Handle invalid size
   return Error_Result;
end if;
```

### 13.5 Type Safety Issues

**Symptom**: Compilation errors about incompatible types
**Diagnosis**: Check for mixing semantically different types
```bash
# Find potential type mixing
grep -n "Natural.*:=" src/ --include="*.adb" | grep -E "(Sequence|Position|Count)"
```

**Fix**: Use explicit conversions or correct types:
```ada
-- Instead of mixing types:
-- Position := Sequence_Number;  -- COMPILE ERROR

-- Use explicit conversion:
Position := File_Position_Type(Natural(Sequence_Number));

-- Or better, redesign to avoid conversion:
procedure Process_Chunk (
   Position : File_Position_Type;  -- Use correct semantic type
   Sequence : Sequence_Number_Type -- Use correct semantic type
);
```

**Common Type Confusion Fixes**:
1. **Progress counts**: Use `Read_Count_Type`, `Processed_Count_Type`, `Written_Count_Type`
2. **File positions**: Use `File_Position_Type` for byte offsets
3. **Sequence numbers**: Use `Sequence_Number_Type` for chunk ordering
4. **Performance metrics**: Use `Processing_Time_Ms_Type`, `Throughput_MBps_Type`

## Summary

This guide provides comprehensive architectural patterns and solutions for working with Pipelib. The key principles are:

1. **Respect layer boundaries** - Domain → Application → Infrastructure
2. **Enforce type safety** - Use distinct semantic types to prevent parameter confusion
3. **Use dependency injection** - For testability and flexibility
4. **Optimize thoughtfully** - Profile first, then optimize
5. **Handle errors explicitly** - Result types over exceptions
6. **Design for concurrency** - Thread-safe by design

For formal requirements, see the [Software Requirements Specification](SOFTWARE_REQUIREMENTS_SPECIFICATION.md). For testing guidance and strategies, see the [Testing Guide](TESTING_GUIDE.md).
