# Developer's Architecture Guide

This guide provides practical insights into Pipelib's architecture for developers working with or extending the library. For formal specifications, see the [Software Design Document](SOFTWARE_DESIGN_DOCUMENT.md). For a quick overview, see the [README](../README.md).

## Table of Contents
1. [Design Patterns in Practice](#design-patterns-in-practice)
2. [Type Safety Architecture](#type-safety-architecture)
3. [Working with the Architecture](#working-with-the-architecture)
4. [Performance Architecture](#performance-architecture)
5. [Extension Points](#extension-points)
6. [Architecture Decision Records](#architecture-decision-records)
7. [Troubleshooting Architecture Issues](#troubleshooting-architecture-issues)

## Design Patterns in Practice

### Repository Pattern with Result Types
The repository pattern abstracts data access while the Result pattern ensures error handling without exceptions:

```ada
-- Port definition (domain layer)
package File_Reader_Interface is
   function Read_Chunk(
      Reader : in out File_Reader;
      Offset : Long_Long_Integer;
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

### Factory Pattern with Validation
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

### State Machine with Contract Enforcement
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

### Template Method for Pipeline Stages
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

## Type Safety Architecture

Pipelib implements comprehensive type safety to prevent parameter confusion and semantic errors. This section demonstrates practical patterns for leveraging Ada's type system effectively.

### Semantic Type Distinction Strategy

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

### Compile-Time Safety Enforcement

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

### Type-Safe Interface Design

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

### Protected Type Safety

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

### Conversion Patterns for External Interfaces

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

### Type Safety in Generic Programming

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

### Design Benefits and Trade-offs

**Benefits of Type Safety Architecture:**
- **Compile-time error detection**: Parameter confusion caught at build time
- **Self-documenting interfaces**: Function signatures clearly show semantic intent
- **Refactoring safety**: Type changes propagate through codebase automatically
- **Domain modeling accuracy**: Types reflect real-world concepts precisely

**Trade-offs:**
- **Conversion overhead**: Explicit conversions required when interfacing with external code
- **Verbosity**: More type declarations and conversion functions needed
- **Learning curve**: Developers must understand semantic type distinctions

### Migration from Untyped Code

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

## Working with the Architecture

### Layer Responsibilities

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

### Dependency Injection Patterns

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

## Performance Architecture

### Memory Management Strategy

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

### Concurrency Patterns

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

### Cache Optimization

```ada
-- Align data structures to cache lines
type Cache_Aligned_Chunk is record
   -- Frequently accessed together (same cache line)
   State : Chunk_State;
   Number : Natural;
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

## Extension Points

### Adding New Processing Stages

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

### Adding New I/O Backends

1. **Implement the port interface**:
```ada
type S3_File_Reader is new File_Reader_Interface with record
   Bucket : Unbounded_String;
   Client : AWS_S3_Client_Access;
end record;

overriding function Read_Chunk(
   Reader : in out S3_File_Reader;
   Offset : Long_Long_Integer;
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

### Custom Progress Reporters

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

## Architecture Decision Records

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

## Troubleshooting Architecture Issues

### Layer Violations

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

### Performance Bottlenecks

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

### Memory Leaks

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

### Contract Violations

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

### Type Safety Issues

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

This guide provides practical patterns and solutions for working with Pipelib's architecture. The key principles are:

1. **Respect layer boundaries** - Domain → Application → Infrastructure
2. **Enforce type safety** - Use distinct semantic types to prevent parameter confusion
3. **Use dependency injection** - For testability and flexibility
4. **Optimize thoughtfully** - Profile first, then optimize
5. **Handle errors explicitly** - Result types over exceptions
6. **Design for concurrency** - Thread-safe by design

For formal specifications, see the [Software Design Document](SOFTWARE_DESIGN_DOCUMENT.md). For API details, see the source code contracts.
