# Pipelib Testing Guide

A comprehensive guide to testing in Pipelib, covering philosophy, practical examples, and advanced techniques for ensuring code quality through Ada 2022 contracts and comprehensive test coverage.

## Table of Contents

1. [Testing Philosophy](#testing-philosophy)
2. [Testing Architecture](#testing-architecture)
3. [Writing Your First Test](#writing-your-first-test)
4. [Contract-Based Testing](#contract-based-testing)
5. [Unit Testing](#unit-testing)
6. [Integration Testing](#integration-testing)
7. [Performance Testing](#performance-testing)
8. [Advanced Testing Techniques](#advanced-testing-techniques)
9. [Mocking and Test Doubles](#mocking-and-test-doubles)
10. [Test Environment Setup](#test-environment-setup)
11. [Running Tests](#running-tests)
12. [Troubleshooting](#troubleshooting)
13. [Command Reference](#command-reference)

## Testing Philosophy

### Core Principles

1. **Contract-First Testing**: Ada 2022 contracts are executable specifications. Test them rigorously.
2. **Result Pattern**: All tests use the Result pattern - no exceptions cross test boundaries.
3. **Comprehensive Coverage**: Target 90% line coverage, 100% contract coverage.
4. **Test Pyramid**: 80% unit tests, 15% integration tests, 5% end-to-end tests.
5. **Early Detection**: Compile-time checks > runtime checks > test failures > production bugs.

### Why We Test

- **Contracts as Documentation**: Tests validate that contracts accurately describe behavior
- **Regression Prevention**: Automated tests catch breaking changes early
- **Design Validation**: Tests drive better API design through usage examples
- **Performance Guarantees**: Benchmarks ensure throughput and latency requirements

## Testing Architecture

### Framework Overview

Pipelib uses the Abohlib testing framework, which provides Result-based testing without exceptions:

```ada
-- Base test structure from Abohlib
type Test_Function is access function
  (Output : access Test_Output_Port'Class) return Void_Result.Result;

type Test_Case is record
   Name : Unbounded_String;
   Func : Test_Function;
end record;
```

### Test Organization

```
tests/
├── unit/                    # Unit tests for individual components
│   ├── core/               # Domain entity tests
│   ├── application/        # Service layer tests
│   └── infrastructure/     # Adapter tests
├── integration/            # Component interaction tests
├── e2e/                   # End-to-end pipeline tests
├── performance/           # Benchmark tests
├── data/                  # Test data files
│   ├── small/            # <1MB test files
│   ├── medium/           # 1-100MB test files
│   └── large/            # >100MB test files
└── utilities/            # Test helpers and mocks
```

## Writing Your First Test

### Step 1: Create a Test Package

```ada
-- tests/unit/core/test_chunk_entity.adb
with Pipelib.Core.Domain.Entities.Chunk;
with Pipelib.Testing.Framework;
with Abohlib.Testing.Framework;

package body Test_Chunk_Entity is
   use Pipelib.Core.Domain.Entities.Chunk;
   use Pipelib.Testing.Framework;

   -- Your test functions go here
end Test_Chunk_Entity;
```

### Step 2: Write a Simple Test

```ada
function Test_Chunk_Creation
  (Output : access Test_Output_Port'Class) return Void_Result.Result
is
   use Abohlib.Core.Domain.Errors;

   -- Arrange: Create test data
   Test_Data : constant Stream_Element_Array_Access :=
     new Stream_Element_Array'(1 .. 1024 => 0);

   -- Act: Create chunk
   Chunk_Result : constant Chunk_Result.Result :=
     Create_Chunk (Data => Test_Data,
                   Sequence => 1,
                   Copy_Data => False);
begin
   -- Assert: Verify creation succeeded
   if Chunk_Result.Is_Err then
      return Output.Fail ("Chunk creation failed: " &
                         Chunk_Result.Err.Message);
   end if;

   declare
      Chunk : constant Chunk_Type := Chunk_Result.Ok;
   begin
      -- Verify initial state
      if Get_State (Chunk) /= Created then
         return Output.Fail ("Expected Created state");
      end if;

      -- Verify sequence number
      if Get_Sequence (Chunk) /= 1 then
         return Output.Fail ("Expected sequence 1");
      end if;

      return Void_Result.Ok_Void;
   end;
end Test_Chunk_Creation;
```

### Step 3: Register Your Test

```ada
function Get_Test_Cases return Test_Array is
begin
   return (
      (Name => To_Unbounded_String ("Chunk Creation"),
       Func => Test_Chunk_Creation'Access),

      (Name => To_Unbounded_String ("State Transitions"),
       Func => Test_State_Transitions'Access),

      -- Add more tests here
   );
end Get_Test_Cases;
```

### Step 4: Run Your Test

```bash
# Build with contracts enabled
alr build --profiles=development

# Run your specific test suite
./bin/test_runner --suite=chunk_entity

# Run all tests
make test
```

## Contract-Based Testing

### Testing Preconditions

Preconditions should reject invalid inputs. Test both acceptance and rejection:

```ada
function Test_Set_State_Preconditions
  (Output : access Test_Output_Port'Class) return Void_Result.Result
is
   Chunk : Chunk_Type := Create_Test_Chunk;
   Contract_Violated : Boolean := False;
begin
   -- Test 1: Valid transition should succeed
   if not Set_State (Chunk, Reading) then
      return Output.Fail ("Valid transition rejected");
   end if;

   -- Test 2: Invalid transition should be caught
   begin
      -- Try invalid transition from Created to Processing
      if Set_State (Chunk, Processing) then
         return Output.Fail ("Invalid transition accepted");
      end if;
   exception
      when Assertion_Error =>
         Contract_Violated := True;  -- Expected
   end;

   if not Contract_Violated then
      return Output.Fail ("Precondition not enforced");
   end if;

   return Void_Result.Ok_Void;
end Test_Set_State_Preconditions;
```

### Testing Postconditions

Verify that functions deliver their promised guarantees:

```ada
function Test_Create_Chunk_Postconditions
  (Output : access Test_Output_Port'Class) return Void_Result.Result
is
   Data : constant Stream_Element_Array_Access := Generate_Test_Data (1_024);
   Chunk_Result : constant Chunk_Result.Result :=
     Create_Chunk (Data, Sequence => 42, Copy_Data => True);
begin
   if Chunk_Result.Is_Err then
      return Output.Fail ("Creation failed");
   end if;

   declare
      Chunk : constant Chunk_Type := Chunk_Result.Ok;
   begin
      -- Verify all postconditions
      if Get_State (Chunk) /= Created then
         return Output.Fail ("Postcondition: State = Created");
      end if;

      if Get_Sequence (Chunk) /= 42 then
         return Output.Fail ("Postcondition: Sequence preserved");
      end if;

      if Get_Data (Chunk) = Data then
         return Output.Fail ("Postcondition: Data was copied");
      end if;

      if not Is_Valid (Chunk) then
         return Output.Fail ("Postcondition: Is_Valid = True");
      end if;

      return Void_Result.Ok_Void;
   end;
end Test_Create_Chunk_Postconditions;
```

### Testing Type Invariants

Ensure type invariants are maintained throughout object lifetime:

```ada
function Test_Chunk_Type_Invariants
  (Output : access Test_Output_Port'Class) return Void_Result.Result
is
   Chunk : Chunk_Type := Create_Test_Chunk;
begin
   -- Perform various operations
   for Op in 1 .. 100 loop
      -- Random valid state transitions
      Perform_Random_Valid_Operation (Chunk);

      -- After each operation, invariants must hold
      if not Is_Valid (Chunk) then
         return Output.Fail ("Type invariant violated after op" & Op'Image);
      end if;

      if Get_Data (Chunk) = null then
         return Output.Fail ("Data invariant violated");
      end if;
   end loop;

   return Void_Result.Ok_Void;
end Test_Chunk_Type_Invariants;
```

### State Machine Testing

Test all valid and invalid state transitions systematically:

```ada
function Test_Complete_State_Machine
  (Output : access Test_Output_Port'Class) return Void_Result.Result
is
   type Transition_Test is record
      From_State : Chunk_State;
      To_State   : Chunk_State;
      Should_Succeed : Boolean;
   end record;

   -- Define all transitions
   Transitions : constant array (Positive range <>) of Transition_Test := (
      (Created, Reading, True),
      (Created, Processing, False),  -- Invalid
      (Reading, Read, True),
      (Read, Processing, True),
      (Processing, Processed, True),
      (Processed, Writing, True),
      (Writing, Written, True),
      (Written, Reading, False),      -- Terminal state
      -- Add all other combinations
   );
begin
   for Test of Transitions loop
      declare
         Chunk : Chunk_Type := Create_Chunk_In_State (Test.From_State);
         Success : Boolean;
      begin
         Success := Try_Transition (Chunk, Test.To_State);

         if Success /= Test.Should_Succeed then
            return Output.Fail (
              "Transition " & Test.From_State'Image & " -> " &
              Test.To_State'Image & " expectation mismatch");
         end if;
      end;
   end loop;

   return Void_Result.Ok_Void;
end Test_Complete_State_Machine;
```

## Unit Testing

### Component Isolation

Test each component in isolation using dependency injection:

```ada
function Test_Memory_Mapped_Chunk_Adapter
  (Output : access Test_Output_Port'Class) return Void_Result.Result
is
   -- Use mock file instead of real file system
   Mock_File : constant Mock_Memory_Mapped_File_Access :=
     Create_Mock_File (Size => 10 * MB);

   Adapter : constant Memory_Mapped_Chunk_Adapter_Access :=
     Create_Adapter (File => Mock_File_Access (Mock_File));
begin
   -- Test chunk size calculation
   declare
      Optimal_Size : constant Natural :=
        Calculate_Optimal_Chunk_Size (Mock_File.Get_Size);
   begin
      if Optimal_Size < Min_Chunk_Size or
         Optimal_Size > Max_Chunk_Size then
         return Output.Fail ("Invalid chunk size calculated");
      end if;
   end;

   -- Test chunk creation
   declare
      Chunks_Result : constant Chunk_Vector_Result.Result :=
        Create_Chunks (Adapter);
   begin
      if Chunks_Result.Is_Err then
         return Output.Fail ("Chunk creation failed");
      end if;

      -- Verify chunks cover entire file
      if not Verify_Complete_Coverage (Chunks_Result.Ok, Mock_File.Get_Size) then
         return Output.Fail ("Incomplete file coverage");
      end if;
   end;

   return Void_Result.Ok_Void;
end Test_Memory_Mapped_Chunk_Adapter;
```

### Boundary Testing

Test edge cases and limits:

```ada
function Test_Chunk_Size_Boundaries
  (Output : access Test_Output_Port'Class) return Void_Result.Result
is
   Test_Sizes : constant array (Positive range <>) of Storage_Count := (
      0,                        -- Empty file
      1,                        -- Single byte
      Min_Chunk_Size - 1,      -- Just below minimum
      Min_Chunk_Size,          -- Exactly minimum
      Medium_Chunk_Size,       -- Typical size
      Max_Chunk_Size,          -- Exactly maximum
      Max_Chunk_Size + 1,      -- Just above maximum
      Storage_Count'Last       -- Maximum possible
   );
begin
   for Size of Test_Sizes loop
      declare
         Calculated : constant Natural :=
           Calculate_Optimal_Chunk_Size (Size);
      begin
         -- Verify constraints
         if Size > 0 and then
            (Calculated < Min_Chunk_Size or Calculated > Max_Chunk_Size) then
            return Output.Fail ("Size constraint violated for " & Size'Image);
         end if;

         -- Verify no overflow
         if Size / Storage_Count (Calculated) > Storage_Count (Natural'Last) then
            return Output.Fail ("Potential overflow for " & Size'Image);
         end if;
      end;
   end loop;

   return Void_Result.Ok_Void;
end Test_Chunk_Size_Boundaries;
```

### Error Path Testing

Test all error conditions:

```ada
function Test_Error_Handling
  (Output : access Test_Output_Port'Class) return Void_Result.Result
is
begin
   -- Test null data handling
   declare
      Result : constant Chunk_Result.Result :=
        Create_Chunk (null, 0, False);
   begin
      if not Result.Is_Err then
         return Output.Fail ("Null data should fail");
      end if;

      if Result.Err.Code /= Invalid_Input_Error then
         return Output.Fail ("Wrong error code for null data");
      end if;
   end;

   -- Test resource exhaustion
   declare
      Chunks : Chunk_Vector;
   begin
      -- Try to allocate many chunks
      for I in 1 .. 10_000 loop
         declare
            Data : constant Stream_Element_Array_Access :=
              new Stream_Element_Array (1 .. Max_Chunk_Size);
            Result : constant Chunk_Result.Result :=
              Create_Chunk (Data, I, True);
         begin
            exit when Result.Is_Err;  -- Expected to fail eventually
            Chunks.Append (Result.Ok);
         end;
      end loop;

      -- Cleanup
      for Chunk of Chunks loop
         Destroy (Chunk);
      end loop;
   end;

   return Void_Result.Ok_Void;
end Test_Error_Handling;
```

## Integration Testing

### Component Integration

Test interactions between layers:

```ada
function Test_Pipeline_Integration
  (Output : access Test_Output_Port'Class) return Void_Result.Result
is
   -- Create complete pipeline
   Input_File : constant Memory_Mapped_File_Access :=
     Open_File ("tests/data/medium/test_10mb.bin");

   Output_File : constant Random_Write_File_Access :=
     Create_File ("tests/output/processed_10mb.bin");

   Processor : constant Parallel_Processor_Access :=
     Create (Worker_Count => 4,
             Output_File => Output_File,
             Context => Test_Context);

   Adapter : constant Memory_Mapped_Chunk_Adapter_Access :=
     Create_Adapter (File => Input_File);
begin
   -- Create chunks from input
   declare
      Chunks_Result : constant Chunk_Vector_Result.Result :=
        Create_Chunks (Adapter);
   begin
      if Chunks_Result.Is_Err then
         return Output.Fail ("Chunk creation failed");
      end if;

      -- Process chunks
      Processor.Start;

      for Chunk of Chunks_Result.Ok loop
         if not Processor.Submit_Chunk (Chunk) then
            return Output.Fail ("Chunk submission failed");
         end if;
      end loop;

      Processor.Signal_End_Of_Input;

      -- Wait for completion
      if not Processor.Wait_For_Completion (Timeout_Ms => 30_000) then
         return Output.Fail ("Processing timeout");
      end if;

      -- Verify output
      if not Files_Match (Input_File, Output_File) then
         return Output.Fail ("Output doesn't match input");
      end if;
   end;

   return Void_Result.Ok_Void;
end Test_Pipeline_Integration;
```

### Concurrent Integration

Test multi-threaded scenarios:

```ada
function Test_Concurrent_Processing
  (Output : access Test_Output_Port'Class) return Void_Result.Result
is
   Output_File : constant Random_Write_File_Access := Create_Mock_File;

   -- Create multiple processors
   Processors : array (1 .. 4) of Parallel_Processor_Access;
   Test_Chunks : constant Chunk_Array := Generate_Test_Chunks (Count => 1000);
begin
   -- Initialize processors
   for I in Processors'Range loop
      Processors (I) := Create (
        Worker_Count => 2,
        Output_File => Output_File,
        Context => Test_Context);
      Processors (I).Start;
   end loop;

   -- Distribute chunks among processors
   for I in Test_Chunks'Range loop
      declare
         Processor_Index : constant Positive :=
           ((I - 1) mod Processors'Length) + 1;
      begin
         if not Processors (Processor_Index).Submit_Chunk (Test_Chunks (I)) then
            return Output.Fail ("Submission failed");
         end if;
      end;
   end loop;

   -- Signal completion and wait
   for Processor of Processors loop
      Processor.Signal_End_Of_Input;
   end loop;

   for Processor of Processors loop
      if not Processor.Wait_For_Completion (Timeout_Ms => 60_000) then
         return Output.Fail ("Processor timeout");
      end if;
   end loop;

   -- Verify all chunks processed exactly once
   if not Verify_All_Chunks_Processed (Output_File, Test_Chunks) then
      return Output.Fail ("Chunk processing error");
   end if;

   return Void_Result.Ok_Void;
end Test_Concurrent_Processing;
```

## Performance Testing

### Throughput Benchmarks

Measure processing speed:

```ada
function Benchmark_Throughput
  (Output : access Test_Output_Port'Class) return Void_Result.Result
is
   File_Sizes : constant array (Positive range <>) of Storage_Count := (
      100 * MB,   -- Small
      500 * MB,   -- Medium
      1 * GB      -- Large
   );

   Worker_Counts : constant array (Positive range <>) of Positive := (
      1, 2, 4, 8, 16
   );
begin
   Output.Put_Line ("Throughput Benchmark Results:");
   Output.Put_Line ("=============================");

   for Size of File_Sizes loop
      for Workers of Worker_Counts loop
         declare
            Start_Time : constant Time := Clock;
            Bytes_Processed : constant Storage_Count :=
              Run_Processing_Benchmark (Size, Workers);
            Duration : constant Duration := Clock - Start_Time;
            Throughput_MBps : constant Float :=
              Float (Bytes_Processed) / Float (Duration) / 1_000_000.0;
         begin
            Output.Put_Line (
              "Size: " & Format_Size (Size) &
              ", Workers: " & Workers'Image &
              ", Throughput: " & Format_Float (Throughput_MBps, 1) & " MB/s");

            -- Verify minimum throughput
            if Workers >= 8 and Throughput_MBps < 500.0 then
               return Output.Fail ("Below minimum throughput requirement");
            end if;
         end;
      end loop;
   end loop;

   return Void_Result.Ok_Void;
end Benchmark_Throughput;
```

### Memory Efficiency

Track memory usage:

```ada
function Test_Memory_Efficiency
  (Output : access Test_Output_Port'Class) return Void_Result.Result
is
   Initial_Memory : constant Natural := Get_Current_Memory_Usage;
   Peak_Memory : Natural := Initial_Memory;

   Monitor_Task : Memory_Monitor_Task;
begin
   Monitor_Task.Start (Peak_Memory'Access);

   -- Process large file
   declare
      Result : constant Boolean :=
        Process_Large_File ("tests/data/large/test_1gb.bin");
   begin
      if not Result then
         return Output.Fail ("Processing failed");
      end if;
   end;

   Monitor_Task.Stop;

   declare
      Memory_Overhead : constant Float :=
        Float (Peak_Memory - Initial_Memory) / Float (GB);
   begin
      Output.Put_Line ("Memory overhead: " &
                      Format_Float (Memory_Overhead * 100.0, 1) & "%");

      if Memory_Overhead > 0.05 then  -- 5% max
         return Output.Fail ("Excessive memory usage");
      end if;
   end;

   return Void_Result.Ok_Void;
end Test_Memory_Efficiency;
```

### Latency Measurements

Test operation timing:

```ada
function Benchmark_Operation_Latency
  (Output : access Test_Output_Port'Class) return Void_Result.Result
is
   Iterations : constant := 10_000;

   -- Measure chunk creation
   declare
      Start : constant Time := Clock;
   begin
      for I in 1 .. Iterations loop
         declare
            Data : constant Stream_Element_Array_Access :=
              new Stream_Element_Array (1 .. Medium_Chunk_Size);
            Chunk : constant Chunk_Type := Create_Chunk (Data, I, False).Ok;
         begin
            Destroy (Chunk);
         end;
      end loop;

      declare
         Duration_Ms : constant Float :=
           Float (Clock - Start) * 1000.0 / Float (Iterations);
      begin
         Output.Put_Line ("Chunk creation: " &
                         Format_Float (Duration_Ms, 3) & " ms");

         if Duration_Ms > 1.0 then
            return Output.Fail ("Chunk creation too slow");
         end if;
      end;
   end;

   -- Measure state transitions
   declare
      Chunk : Chunk_Type := Create_Test_Chunk;
      Start : constant Time := Clock;
   begin
      for I in 1 .. Iterations loop
         Reset (Chunk);
         Set_State (Chunk, Reading);
         Set_State (Chunk, Read);
      end loop;

      declare
         Duration_Us : constant Float :=
           Float (Clock - Start) * 1_000_000.0 / Float (Iterations);
      begin
         Output.Put_Line ("State transition: " &
                         Format_Float (Duration_Us, 1) & " μs");

         if Duration_Us > 100.0 then
            return Output.Fail ("State transitions too slow");
         end if;
      end;
   end;

   return Void_Result.Ok_Void;
end Benchmark_Operation_Latency;
```

## Advanced Testing Techniques

### Property-Based Testing

Test invariants with random inputs:

```ada
function Property_Test_Chunk_Invariants
  (Output : access Test_Output_Port'Class) return Void_Result.Result
is
   Gen : Generator;
begin
   -- Test with 1000 random inputs
   for Trial in 1 .. 1_000 loop
      declare
         -- Generate random valid chunk data
         Size : constant Positive :=
           Random_Range (Gen, Min_Chunk_Size, Max_Chunk_Size);
         Data : constant Stream_Element_Array_Access :=
           Generate_Random_Data (Size);
         Sequence : constant Natural := Random (Gen) mod 10_000;

         -- Create chunk
         Chunk_Result : constant Chunk_Result.Result :=
           Create_Chunk (Data, Sequence, Copy_Data => Random_Boolean (Gen));
      begin
         if Chunk_Result.Is_Err then
            return Output.Fail ("Valid input rejected in trial" & Trial'Image);
         end if;

         declare
            Chunk : constant Chunk_Type := Chunk_Result.Ok;
         begin
            -- Properties that must always hold
            if not Is_Valid (Chunk) then
               return Output.Fail ("Invalid chunk created");
            end if;

            if Get_Sequence (Chunk) /= Sequence then
               return Output.Fail ("Sequence not preserved");
            end if;

            if Get_State (Chunk) /= Created then
               return Output.Fail ("Wrong initial state");
            end if;

            if Get_Data (Chunk) = null then
               return Output.Fail ("Null data in valid chunk");
            end if;
         end;
      end;
   end loop;

   return Void_Result.Ok_Void;
end Property_Test_Chunk_Invariants;
```

### Stress Testing

Push the system to its limits:

```ada
function Stress_Test_Parallel_Processing
  (Output : access Test_Output_Port'Class) return Void_Result.Result
is
   Max_Workers : constant := 64;  -- System limit
   Chunk_Count : constant := 10_000;

   Output_File : constant Random_Write_File_Access := Create_Mock_File;
   Processor : constant Parallel_Processor_Access :=
     Create (Worker_Count => Max_Workers,
             Output_File => Output_File,
             Context => Stress_Test_Context);
begin
   Output.Put_Line ("Starting stress test with " & Max_Workers'Image & " workers");

   Processor.Start;

   -- Flood with chunks
   declare
      Submitted : Natural := 0;
   begin
      for I in 1 .. Chunk_Count loop
         declare
            Chunk : constant Chunk_Type := Generate_Test_Chunk (Max_Chunk_Size);
         begin
            if Processor.Submit_Chunk (Chunk) then
               Submitted := Submitted + 1;
            else
               -- Queue full, wait a bit
               delay 0.001;
               if not Processor.Submit_Chunk (Chunk) then
                  return Output.Fail ("Cannot submit after delay");
               end if;
            end if;
         end;
      end loop;

      Output.Put_Line ("Submitted " & Submitted'Image & " chunks");
   end;

   Processor.Signal_End_Of_Input;

   if not Processor.Wait_For_Completion (Timeout_Ms => 300_000) then
      return Output.Fail ("Stress test timeout");
   end if;

   -- Verify no chunks lost
   declare
      Stats : constant Processing_Stats := Processor.Get_Stats;
   begin
      if Stats.Chunks_Processed /= Chunk_Count then
         return Output.Fail ("Lost chunks: processed " &
                           Stats.Chunks_Processed'Image &
                           " of " & Chunk_Count'Image);
      end if;
   end;

   return Void_Result.Ok_Void;
end Stress_Test_Parallel_Processing;
```

### Long-Running Tests

Test system stability over time:

```ada
function Long_Running_Memory_Stability_Test
  (Output : access Test_Output_Port'Class) return Void_Result.Result
is
   Test_Duration : constant Duration := 3600.0;  -- 1 hour
   Start_Time : constant Time := Clock;
   Initial_Memory : constant Natural := Get_Current_Memory_Usage;

   Iterations : Natural := 0;
   Memory_Samples : Memory_Sample_Array (1 .. 3600);  -- One per second
begin
   Output.Put_Line ("Starting 1-hour memory stability test");

   while Clock - Start_Time < Test_Duration loop
      -- Process a file
      Process_Test_File ("tests/data/medium/test_10mb.bin");

      Iterations := Iterations + 1;

      -- Sample memory every second
      if Natural ((Clock - Start_Time) / 1.0) <= Memory_Samples'Last then
         Memory_Samples (Natural ((Clock - Start_Time) / 1.0)) :=
           Get_Current_Memory_Usage;
      end if;

      -- Check for memory leak
      declare
         Current_Memory : constant Natural := Get_Current_Memory_Usage;
         Growth : constant Float :=
           Float (Current_Memory - Initial_Memory) / Float (Initial_Memory);
      begin
         if Growth > 0.10 then  -- 10% growth
            return Output.Fail ("Potential memory leak detected: " &
                              Format_Float (Growth * 100.0, 1) & "% growth");
         end if;
      end;
   end loop;

   Output.Put_Line ("Completed " & Iterations'Image & " iterations");
   Output.Put_Line ("Memory remained stable");

   return Void_Result.Ok_Void;
end Long_Running_Memory_Stability_Test;
```

## Mocking and Test Doubles

### Creating Mocks

Build test doubles for external dependencies:

```ada
-- Mock memory-mapped file
type Mock_Memory_Mapped_File is new Memory_Mapped_File_Interface with record
   Mock_Data : Stream_Element_Array_Access;
   Mock_Size : Storage_Count;
   Is_Mapped : Boolean := False;
   Map_Count : Natural := 0;  -- Track operations
   View_Count : Natural := 0;
end record;

overriding
function Map (File : in out Mock_Memory_Mapped_File) return Boolean is
begin
   File.Map_Count := File.Map_Count + 1;
   File.Is_Mapped := True;
   return True;
end Map;

overriding
function Get_View
  (File : Mock_Memory_Mapped_File;
   Offset : Storage_Offset := 0;
   Size : Storage_Count := 0) return Memory_View
is
begin
   File.View_Count := File.View_Count + 1;

   -- Return view into mock data
   return Memory_View'(
     Data => File.Mock_Data (Offset + 1 .. Offset + Size),
     Size => Size);
end Get_View;

-- Factory function
function Create_Mock_File
  (Size : Storage_Count;
   Pattern : Data_Pattern := Sequential) return Mock_Memory_Mapped_File_Access
is
   Mock : constant Mock_Memory_Mapped_File_Access := new Mock_Memory_Mapped_File;
begin
   Mock.Mock_Size := Size;
   Mock.Mock_Data := new Stream_Element_Array (1 .. Size);

   -- Fill with test pattern
   case Pattern is
      when Sequential =>
         for I in Mock.Mock_Data'Range loop
            Mock.Mock_Data (I) := Stream_Element (I mod 256);
         end loop;

      when Random =>
         Fill_Random_Data (Mock.Mock_Data.all);

      when Zeros =>
         Mock.Mock_Data.all := (others => 0);
   end case;

   return Mock;
end Create_Mock_File;
```

### Configurable Test Doubles

Make mocks configurable for different scenarios:

```ada
type Mock_Random_Write_File is new Random_Write_File_Interface with record
   Config : Mock_Config;
   Buffer : Stream_Element_Array_Access;
   Position : Natural := 0;
   Is_Open : Boolean := False;
   Written_Chunks : Chunk_Record_Vector;

   -- Configurable behavior
   Fail_After_Writes : Natural := Natural'Last;
   Writes_Count : Natural := 0;
   Latency_Ms : Natural := 0;
end record;

type Mock_Config is record
   Capacity : Natural := 10 * MB;
   Fail_On_Write : Boolean := False;
   Fail_After_N_Writes : Natural := Natural'Last;
   Simulate_Latency : Boolean := False;
   Latency_Range : Latency_Range_Type := (Min => 0, Max => 0);
   Track_Writes : Boolean := True;
end record;

overriding
function Write_At
  (File : in out Mock_Random_Write_File;
   Position : Natural;
   Data : Stream_Element_Array) return Boolean
is
begin
   -- Simulate configured behavior
   if File.Config.Fail_On_Write then
      return False;
   end if;

   File.Writes_Count := File.Writes_Count + 1;
   if File.Writes_Count > File.Config.Fail_After_N_Writes then
      return False;  -- Simulate failure after N writes
   end if;

   if File.Config.Simulate_Latency then
      delay Duration (File.Config.Latency_Range.Min) / 1000.0;
   end if;

   -- Track write if configured
   if File.Config.Track_Writes then
      File.Written_Chunks.Append ((
        Position => Position,
        Size => Data'Length,
        Timestamp => Clock));
   end if;

   -- Actually "write" to buffer
   File.Buffer (Position + 1 .. Position + Data'Length) := Data;
   return True;
end Write_At;
```

### Spy Objects

Track interactions for verification:

```ada
type Spy_Progress_Tracker is new Progress_Tracker_Interface with record
   Base_Tracker : Progress_Tracker_Access;
   Call_Log : Call_Record_Vector;
end record;

type Call_Record is record
   Operation : Operation_Type;
   Timestamp : Time;
   Parameters : Parameter_Map;
end record;

overriding
procedure Update_Progress
  (Tracker : in out Spy_Progress_Tracker;
   Category : Progress_Category;
   Count : Natural)
is
begin
   -- Log the call
   Tracker.Call_Log.Append ((
     Operation => Update_Op,
     Timestamp => Clock,
     Parameters => Create_Map (("category", Category'Image),
                              ("count", Count'Image))));

   -- Delegate to real implementation
   Tracker.Base_Tracker.Update_Progress (Category, Count);
end Update_Progress;

-- Verification helpers
function Verify_Update_Called
  (Spy : Spy_Progress_Tracker;
   Category : Progress_Category;
   Min_Times : Natural := 1) return Boolean
is
   Count : Natural := 0;
begin
   for Call of Spy.Call_Log loop
      if Call.Operation = Update_Op and then
         Call.Parameters.Contains ("category") and then
         Call.Parameters ("category") = Category'Image then
         Count := Count + 1;
      end if;
   end loop;

   return Count >= Min_Times;
end Verify_Update_Called;
```

## Test Environment Setup

### Project Configuration

Configure your test project with proper settings:

```ada
-- tests.gpr
project Tests is
   for Source_Dirs use ("tests/**");
   for Object_Dir use "obj/tests";
   for Exec_Dir use "bin";
   for Main use ("test_runner.adb");

   type Mode_Type is ("debug", "release", "coverage");
   Mode : Mode_Type := external ("TEST_MODE", "debug");

   package Compiler is
      Common_Switches := (
         "-gnat2022",    -- Ada 2022
         "-gnata",       -- Enable assertions/contracts
         "-gnatVa",      -- All validity checks
         "-gnatwa",      -- All warnings
         "-gnatyy",      -- Style checks
         "-gnatQ"        -- Don't quit on errors
      );

      case Mode is
         when "debug" =>
            for Default_Switches ("Ada") use Common_Switches & (
               "-g",        -- Debug info
               "-O0"        -- No optimization
            );

         when "release" =>
            for Default_Switches ("Ada") use Common_Switches & (
               "-O2"        -- Optimize
            );

         when "coverage" =>
            for Default_Switches ("Ada") use Common_Switches & (
               "-g",
               "-O0",
               "--coverage" -- Coverage instrumentation
            );
      end case;
   end Compiler;

   package Binder is
      for Default_Switches ("Ada") use ("-Es"); -- Exception traceback
   end Binder;

   package Linker is
      case Mode is
         when "coverage" =>
            for Default_Switches ("Ada") use ("--coverage");
         when others =>
            null;
      end case;
   end Linker;
end Tests;
```

### Test Data Setup

Create test data programmatically:

```bash
#!/bin/bash
# scripts/setup_test_data.sh

# Create directory structure
mkdir -p tests/data/{small,medium,large}
mkdir -p tests/output
mkdir -p tests/temp

# Generate test files
echo "Generating test data..."

# Small files (< 1MB)
for i in {1..10}; do
    size=$((RANDOM % 1024 + 1))  # 1KB to 1MB
    dd if=/dev/urandom of=tests/data/small/test_${size}kb.bin bs=1024 count=$size 2>/dev/null
    echo "Created small test file: ${size}KB"
done

# Medium files (1MB - 100MB)
for size in 1 10 50 100; do
    dd if=/dev/urandom of=tests/data/medium/test_${size}mb.bin bs=1048576 count=$size 2>/dev/null
    echo "Created medium test file: ${size}MB"
done

# Large files (> 100MB)
for size in 500 1000; do
    dd if=/dev/urandom of=tests/data/large/test_${size}mb.bin bs=1048576 count=$size 2>/dev/null
    echo "Created large test file: ${size}MB"
done

# Special pattern files
echo "Creating pattern files..."

# All zeros
dd if=/dev/zero of=tests/data/special/zeros_1mb.bin bs=1048576 count=1 2>/dev/null

# Repeating pattern
python3 -c "
data = b'PATTERN!' * (1024 * 128)  # 1MB of repeating pattern
with open('tests/data/special/pattern_1mb.bin', 'wb') as f:
    f.write(data)
"

echo "Test data setup complete!"
```

### Environment Variables

Configure test behavior through environment:

```ada
-- Test configuration from environment
package Test_Config is
   -- Timeouts
   Default_Timeout_Ms : constant Natural :=
     Natural'Value (Getenv ("TEST_TIMEOUT", "30000"));

   Long_Test_Timeout_Ms : constant Natural :=
     Natural'Value (Getenv ("LONG_TEST_TIMEOUT", "300000"));

   -- Parallelism
   Max_Test_Workers : constant Positive :=
     Positive'Value (Getenv ("TEST_WORKERS", "4"));

   Run_Stress_Tests : constant Boolean :=
     Getenv ("RUN_STRESS_TESTS", "false") = "true";

   Run_Long_Tests : constant Boolean :=
     Getenv ("RUN_LONG_TESTS", "false") = "true";

   -- Output control
   Verbose_Output : constant Boolean :=
     Getenv ("TEST_VERBOSE", "false") = "true";

   Generate_Reports : constant Boolean :=
     Getenv ("TEST_REPORTS", "true") = "true";

   -- Test data
   Test_Data_Dir : constant String :=
     Getenv ("TEST_DATA_DIR", "tests/data");
end Test_Config;
```

## Running Tests

### Basic Test Execution

```bash
# Run all tests
make test

# Run specific test suite
./bin/test_runner --suite=unit

# Run specific test file
./bin/test_runner --file=test_chunk_entity

# Run with verbose output
./bin/test_runner --verbose

# Run with custom timeout
./bin/test_runner --timeout=60000
```

### Selective Test Execution

```bash
# Run only contract tests
./bin/test_runner --filter="*_contract*"

# Run performance tests
./bin/test_runner --suite=performance --jobs=1

# Run integration tests in parallel
./bin/test_runner --suite=integration --jobs=8

# Run specific test by name
./bin/test_runner --test="Test_Chunk_State_Transitions"
```

### Continuous Integration

```yaml
# .github/workflows/test.yml
name: Test Pipeline

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest

    strategy:
      matrix:
        test-suite: [unit, integration, performance]

    steps:
    - uses: actions/checkout@v3

    - name: Setup Ada Toolchain
      uses: ada-actions/toolchain@v1
      with:
        distrib: community
        target: native

    - name: Install Alire
      uses: ada-actions/setup-alire@v1

    - name: Setup Test Data
      run: ./scripts/setup_test_data.sh

    - name: Build Tests
      run: |
        alr build --profiles=development
        gprbuild -P tests.gpr -XTEST_MODE=coverage

    - name: Run ${{ matrix.test-suite }} Tests
      run: |
        ./bin/test_runner --suite=${{ matrix.test-suite }} \
                         --output=results_${{ matrix.test-suite }}.xml \
                         --format=junit

    - name: Generate Coverage Report
      if: matrix.test-suite == 'unit'
      run: |
        gcov obj/tests/*.gcda
        lcov --capture --directory obj/tests --output-file coverage.info
        genhtml coverage.info --output-directory coverage_html

    - name: Upload Results
      uses: actions/upload-artifact@v3
      with:
        name: test-results-${{ matrix.test-suite }}
        path: |
          results_*.xml
          coverage_html/

    - name: Publish Test Results
      uses: dorny/test-reporter@v1
      if: always()
      with:
        name: ${{ matrix.test-suite }} Test Results
        path: results_${{ matrix.test-suite }}.xml
        reporter: java-junit
```

### Local Development Workflow

```bash
# Quick test during development
make test-quick  # Runs only fast unit tests

# Full test before commit
make test-full   # All tests including slow ones

# Test with debugging
gdb ./bin/test_runner
(gdb) catch exception
(gdb) run --suite=unit --test=Test_That_Fails

# Test with profiling
gprof ./bin/test_runner gmon.out > profile.txt

# Memory leak detection
valgrind --leak-check=full ./bin/test_runner --suite=unit
```

## Troubleshooting

### Common Issues

#### Contract Violations During Tests

**Problem**: Tests fail with assertion errors when contracts are enabled.

**Solution**:
```ada
-- Ensure test setup satisfies preconditions
function Test_With_Valid_Setup
  (Output : access Test_Output_Port'Class) return Void_Result.Result
is
   -- Create valid test data that satisfies all preconditions
   Valid_Data : constant Stream_Element_Array_Access :=
     new Stream_Element_Array'(1 .. Medium_Chunk_Size => 0);

   -- Verify preconditions before calling
   pragma Assert (Valid_Data /= null);
   pragma Assert (Valid_Data'Length >= Min_Chunk_Size);
begin
   -- Now safe to call
   declare
      Result : constant Chunk_Result.Result :=
        Create_Chunk (Valid_Data, 1, False);
   begin
      -- Test continues...
   end;
end Test_With_Valid_Setup;
```

#### Race Conditions in Tests

**Problem**: Intermittent failures in concurrent tests.

**Solution**:
```ada
-- Use proper synchronization
protected type Test_Synchronizer is
   procedure Signal_Ready;
   entry Wait_Until_Ready;
private
   Is_Ready : Boolean := False;
end Test_Synchronizer;

function Test_Concurrent_Operation
  (Output : access Test_Output_Port'Class) return Void_Result.Result
is
   Sync : Test_Synchronizer;

   task Worker is
      entry Start;
   end Worker;

   task body Worker is
   begin
      accept Start;

      -- Wait for main thread setup
      Sync.Wait_Until_Ready;

      -- Now safe to proceed
      Perform_Concurrent_Operation;
   end Worker;
begin
   -- Setup test state
   Setup_Shared_Resources;

   -- Start worker
   Worker.Start;

   -- Signal worker to proceed
   Sync.Signal_Ready;

   -- Continue with test...
end Test_Concurrent_Operation;
```

#### Memory Leaks in Tests

**Problem**: Tests consume increasing memory.

**Solution**:
```ada
-- Ensure proper cleanup
function Test_With_Cleanup
  (Output : access Test_Output_Port'Class) return Void_Result.Result
is
   -- Track allocations
   Allocated_Objects : Object_Vector;

   -- Cleanup handler
   procedure Cleanup is
   begin
      for Obj of Allocated_Objects loop
         Free (Obj);
      end loop;
      Allocated_Objects.Clear;
   end Cleanup;
begin
   -- Main test logic
   begin
      -- Allocate resources
      for I in 1 .. 100 loop
         declare
            Obj : constant Object_Access := new Object_Type;
         begin
            Allocated_Objects.Append (Obj);
            -- Use object...
         end;
      end loop;

      -- Test assertions...

   exception
      when others =>
         Cleanup;
         raise;
   end;

   -- Always cleanup
   Cleanup;
   return Void_Result.Ok_Void;
end Test_With_Cleanup;
```

#### Flaky Tests

**Problem**: Tests pass sometimes but fail others.

**Solution**:
```ada
-- Make tests deterministic
function Test_Deterministic_Behavior
  (Output : access Test_Output_Port'Class) return Void_Result.Result
is
   -- Use fixed seed for randomness
   Gen : Generator;
begin
   Reset (Gen, 12345);  -- Fixed seed

   -- Avoid time-dependent behavior
   declare
      -- Bad: Uses current time
      -- Timeout : constant Time := Clock + 1.0;

      -- Good: Uses fixed duration
      Max_Iterations : constant := 1000;
   begin
      for I in 1 .. Max_Iterations loop
         -- Deterministic operations...
      end loop;
   end;

   -- Avoid file system races
   declare
      -- Use unique names
      Test_File : constant String :=
        "test_" & Current_Task_Id'Image & ".tmp";
   begin
      -- Test with unique file...
   end;

   return Void_Result.Ok_Void;
end Test_Deterministic_Behavior;
```

### Debugging Test Failures

#### Enable Detailed Output

```ada
-- Add diagnostic output
function Test_With_Diagnostics
  (Output : access Test_Output_Port'Class) return Void_Result.Result
is
   Enable_Debug : constant Boolean := Test_Config.Verbose_Output;
begin
   if Enable_Debug then
      Output.Put_Line ("=== Test Setup ===");
      Output.Put_Line ("Test data size: " & Test_Data_Size'Image);
      Output.Put_Line ("Worker count: " & Worker_Count'Image);
   end if;

   -- Perform operation
   declare
      Result : constant Operation_Result := Perform_Operation;
   begin
      if Enable_Debug then
         Output.Put_Line ("Operation result: " & Result'Image);
         Output.Put_Line ("Internal state: " & Get_State_String);
      end if;

      -- On failure, provide context
      if not Result.Success then
         Output.Put_Line ("FAILURE CONTEXT:");
         Output.Put_Line ("  Error: " & Result.Error_Message);
         Output.Put_Line ("  State: " & Dump_State);
         Output.Put_Line ("  Stack: " & Get_Stack_Trace);

         return Output.Fail ("Operation failed - see context above");
      end if;
   end;

   return Void_Result.Ok_Void;
end Test_With_Diagnostics;
```

#### Use Test Fixtures

```ada
-- Reusable test setup
generic
   with function Create_Test_Data return Test_Data_Type;
package Test_Fixture is

   type Fixture_Type is tagged limited record
      Data : Test_Data_Type;
      Output_File : File_Access;
      Processor : Processor_Access;
   end record;

   function Setup (Output : access Test_Output_Port'Class) return Fixture_Type;
   procedure Teardown (Fixture : in out Fixture_Type);

end Test_Fixture;

-- Use in tests
function Test_Using_Fixture
  (Output : access Test_Output_Port'Class) return Void_Result.Result
is
   package My_Fixture is new Test_Fixture (Create_Standard_Test_Data);
   Fixture : My_Fixture.Fixture_Type := My_Fixture.Setup (Output);
begin
   -- Test using fixture...

   -- Cleanup happens automatically
   My_Fixture.Teardown (Fixture);
   return Void_Result.Ok_Void;
exception
   when others =>
      My_Fixture.Teardown (Fixture);
      raise;
end Test_Using_Fixture;
```

## Command Reference

### test_runner Options

```bash
Usage: test_runner [OPTIONS]

Options:
  --suite=SUITE          Run specific test suite (unit|integration|performance|all)
  --file=FILE            Run tests from specific file
  --test=TEST            Run specific test by name
  --filter=PATTERN       Run tests matching pattern (supports wildcards)
  --exclude=PATTERN      Exclude tests matching pattern
  --jobs=N              Number of parallel test jobs (default: 4)
  --timeout=MS          Global timeout in milliseconds (default: 30000)
  --verbose             Enable verbose output
  --quiet               Minimal output (errors only)
  --output=FILE         Write results to file
  --format=FORMAT       Output format (console|junit|json|html)
  --coverage            Enable coverage collection
  --profile             Enable profiling
  --debug               Enable debug mode (no timeout, full traces)
  --list                List all available tests
  --dry-run             Show what would be run without executing
  --fail-fast           Stop on first failure
  --repeat=N            Run each test N times
  --shuffle             Randomize test order
  --seed=N              Random seed for shuffling
  --help                Show this help

Environment Variables:
  TEST_DATA_DIR         Test data directory (default: tests/data)
  TEST_TIMEOUT          Default timeout in ms (default: 30000)
  TEST_WORKERS          Default job count (default: 4)
  TEST_VERBOSE          Enable verbose mode (default: false)
  RUN_STRESS_TESTS      Enable stress tests (default: false)
  RUN_LONG_TESTS        Enable long-running tests (default: false)

Examples:
  # Run all unit tests
  test_runner --suite=unit

  # Run specific test with verbose output
  test_runner --test=Test_Chunk_Creation --verbose

  # Run integration tests in parallel
  test_runner --suite=integration --jobs=8

  # Run with coverage
  test_runner --suite=all --coverage --output=coverage.xml

  # Debug a failing test
  test_runner --test=Test_That_Fails --debug --jobs=1
```

### Makefile Targets

```makefile
# Run all tests
make test

# Run specific test suites
make test-unit
make test-integration
make test-performance

# Run with coverage
make test-coverage

# Run quick smoke tests
make test-quick

# Run full test suite including slow tests
make test-full

# Generate test report
make test-report

# Clean test artifacts
make clean-tests

# Setup test environment
make test-setup

# Run specific test file
make test-file FILE=test_chunk_entity

# Run tests matching pattern
make test-filter FILTER="*_contract*"

# Debug test
make test-debug TEST=Test_Name
```

### Coverage Reports

```bash
# Generate coverage report
make coverage

# View coverage in terminal
make coverage-summary

# Generate HTML report
make coverage-html
# Open coverage_html/index.html in browser

# Coverage for specific package
make coverage-package PACKAGE=Pipelib.Core.Domain

# Coverage diff against main branch
make coverage-diff

# Fail if coverage below threshold
make coverage-check THRESHOLD=90
```

## Summary

This testing guide provides comprehensive coverage of testing in Pipelib:

1. **Philosophy**: Contract-first, Result-based testing with high coverage targets
2. **Practical Examples**: Real code samples you can adapt for your tests
3. **Advanced Techniques**: Property-based testing, stress testing, performance benchmarks
4. **Troubleshooting**: Solutions to common testing problems
5. **Complete Reference**: All commands and options documented

Remember:
- Enable contracts during development (`-gnata`)
- Write tests for both success and failure paths
- Use mocks to isolate components
- Keep tests deterministic and independent
- Measure and maintain coverage targets

The key to successful testing in Pipelib is leveraging Ada 2022's contracts as executable specifications, combined with comprehensive test coverage across all architectural layers.
