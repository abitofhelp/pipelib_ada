--  =============================================================================
--  Pipelib.Infrastructure.IO.Memory_Mapped_Chunk_Adapter - Memory-Mapped File Chunk Adapter
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  ## Memory-Mapped File Chunk Adapter - Zero-Copy File Processing
--
--  High-performance adapter for creating File_Chunk objects from memory-mapped
--  files with zero-copy access. This is the most efficient way to process
--  large files (100MB - 1GB) by mapping them directly into virtual memory.
--
--  ### Performance Benefits
--
--  **Zero-Copy Access:**
--  * No memory allocation for file data - uses virtual memory mapping
--  * No copying of file contents - direct access to mapped memory
--  * Minimal memory footprint - OS manages memory pages on demand
--  * Automatic prefetching - OS optimizes memory access patterns
--
--  **Throughput Characteristics:**
--  * Sequential reads: 1-3 GB/s (depends on storage and memory bandwidth)
--  * Random access: Significantly faster than traditional file I/O
--  * Memory pressure: Automatically managed by OS virtual memory system
--  * Cache efficiency: Leverages OS page cache without duplication
--
--  ### When to Use Memory Mapping
--
--  **Optimal Use Cases:**
--  * Large files (100MB - 1GB) that benefit from virtual memory management
--  * Sequential or predictable access patterns
--  * Multiple passes over the same data
--  * Systems with sufficient virtual address space
--
--  **Avoid Memory Mapping For:**
--  * Small files (<100MB) - overhead exceeds benefits
--  * Very large files (>1GB) - may exhaust virtual address space
--  * Highly random access patterns on spinning disk storage
--  * Memory-constrained systems
--
--  ### Architecture Integration
--
--  This adapter sits in the Infrastructure layer and implements the Hexagonal
--  Architecture pattern by adapting the memory-mapped file port interface
--  to create domain value objects (File_Chunk).
--
--  ```
--  Domain Layer:          File_Chunk (Value Object)
--                              Ã¢ÂÂ²
--  Application Layer:           Ã¢ÂÂ
--                              Ã¢ÂÂ
--  Infrastructure Layer:  Memory_Mapped_Chunk_Adapter Ã¢ÂÂÃ¢ÂÂÃ¢ÂÂÃ¢ÂÂ This Component
--                              Ã¢ÂÂ
--  External Systems:      Memory_Mapped_File_Interface Ã¢ÂÂÃ¢ÂÂÃ¢ÂÂÃ¢ÂÂ OS Virtual Memory
--  ```
--
--  ### Memory Safety Guarantees
--
--  * All memory access bounds are validated through Ada 2022 contracts
--  * Stream_Element_Array_Access provides type-safe access to mapped memory
--  * Automatic cleanup when File_Chunk objects are finalized
--  * Protection against buffer overruns through contract preconditions
--
--  ### Configuration Options
--
--  The adapter provides configurable options for different use cases:
--  * **Chunk Size**: Adaptive sizing based on file characteristics
--  * **Checksum Calculation**: Optional SHA-256 for data integrity
--  * **Access Patterns**: Hints to OS for sequential vs. random access optimization
--
--  ### Error Handling
--
--  All operations use the Result pattern for comprehensive error handling:
--  * Memory mapping failures (insufficient address space, permissions)
--  * File access errors (file not found, permissions, corruption)
--  * Configuration validation errors (invalid parameters)
--  * Resource exhaustion (memory, file handles)
--  =============================================================================
pragma Ada_2022;

with System;                use System;
with System.Storage_Elements;
with Pipelib.Core.Domain.Ports.Memory_Mapped_File_Interface;
with Pipelib.Core.Domain.Value_Objects.File_Chunk;
with Pipelib.Core.Domain.Value_Objects.File_Chunk.Vectors;
with Abohlib.Core.Domain.Result;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Pipelib.Infrastructure.IO.Memory_Mapped_Chunk_Adapter is

   use System.Storage_Elements;
   use Pipelib.Core.Domain.Ports.Memory_Mapped_File_Interface;
   use Pipelib.Core.Domain.Value_Objects.File_Chunk;
   use Pipelib.Core.Domain.Value_Objects.File_Chunk.Vectors;

   --  ## Configuration for Memory-Mapped Chunk Creation
   --
   --  Configures how chunks are created from memory-mapped files.
   --  These settings affect both performance and memory usage characteristics.
   --
   --  ### Configuration Fields
   --
   --  * **Default_Chunk_Size** - Base chunk size in bytes (default: 64KB)
   --    - Smaller chunks: Lower memory usage, higher processing overhead
   --    - Larger chunks: Higher memory usage, better throughput
   --    - Adaptive algorithms may override this based on file characteristics
   --
   --  * **Calculate_Checksums** - Whether to compute SHA-256 checksums (default: True)
   --    - Enabled: Provides data integrity verification, slight performance cost
   --    - Disabled: Maximum performance, no integrity checking
   --
   --  * **Use_Sequential_Access** - Memory access pattern hint to OS (default: True)
   --    - True: Optimized for sequential file processing
   --    - False: Better for random access patterns
   --
   --  ### Performance Tuning
   --
   --  ```ada
   --  -- High-throughput sequential processing
   --  High_Throughput_Config : constant Chunk_Config := (
   --     Default_Chunk_Size => 1024 * 1024,  -- 1MB chunks
   --     Calculate_Checksums => False,       -- Skip checksums for speed
   --     Use_Sequential_Access => True       -- Optimize for sequential access
   --  );
   --
   --  -- Integrity-focused processing
   --  Integrity_Config : constant Chunk_Config := (
   --     Default_Chunk_Size => 64 * 1024,    -- 64KB chunks (default)
   --     Calculate_Checksums => True,        -- Always verify integrity
   --     Use_Sequential_Access => True       -- Sequential optimization
   --  );
   --
   --  -- Memory-constrained processing
   --  Low_Memory_Config : constant Chunk_Config := (
   --     Default_Chunk_Size => 16 * 1024,    -- 16KB chunks
   --     Calculate_Checksums => True,        -- Keep integrity checking
   --     Use_Sequential_Access => True       -- Sequential optimization
   --  );
   --  ```
   type Chunk_Config is record
      Default_Chunk_Size    : Positive := 64 * 1024; -- 64KB default
      Calculate_Checksums   : Boolean := True;
      Use_Sequential_Access : Boolean := True; -- Hint for OS optimization
   end record;

   --  ## Result Type for Chunk Creation Operations
   --
   --  Result type following the Result pattern for error handling without exceptions.
   --  Operations return either a vector of successfully created chunks or an error message.
   package Chunk_Vector_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => File_Chunk_Vector,
        Err_Type => Unbounded_String);

   --  ## Bulk Chunk Creation from Memory-Mapped File
   --
   --  Creates multiple File_Chunk objects from a memory-mapped file using zero-copy
   --  access. This is the most efficient way to process large files as it avoids
   --  copying data and leverages the OS virtual memory system.
   --
   --  ### Parameters
   --  * `Map` - Memory-mapped file interface (must be successfully mapped)
   --  * `Config` - Configuration for chunk creation behavior
   --  * `Start_Offset` - Byte offset from file beginning to start creating chunks
   --  * `Max_Bytes` - Maximum bytes to process (0 = entire file from offset)
   --
   --  ### Zero-Copy Implementation
   --  This function creates Stream_Element_Array_Access objects that point directly
   --  to the memory-mapped file data. No copying occurs - chunks reference the
   --  mapped memory directly, providing:
   --
   --  * **Minimal Memory Usage**: Only chunk metadata allocated, not data
   --  * **Maximum Throughput**: No time spent copying large data arrays
   --  * **OS Optimization**: Automatic prefetching and caching by virtual memory system
   --
   --  ### Chunk Size Adaptation
   --  The function uses adaptive algorithms to determine optimal chunk sizes:
   --
   --  1. **File Size Analysis**: Larger files get larger chunks for better throughput
   --  2. **Memory Pressure**: Adjusts based on available system memory
   --  3. **Access Pattern**: Sequential vs. random access optimization
   --  4. **Configuration Override**: Respects user-specified chunk sizes when appropriate
   --
   --  ### Performance Characteristics
   --  * **Time Complexity**: O(file_size / chunk_size) - linear in number of chunks
   --  * **Space Complexity**: O(number_of_chunks) - only metadata storage
   --  * **Memory Bandwidth**: Near-zero for chunk creation, full bandwidth for access
   --
   --  ### Examples
   --  ```ada
   --  -- Process entire file with default configuration
   --  declare
   --     Mapped_File : Memory_Mapped_File := Map_File ("large_data.bin");
   --     Result : constant Chunk_Vector_Result.Result :=
   --        Create_Chunks_From_Memory_Map (Mapped_File);
   --  begin
   --     if Chunk_Vector_Result.Is_Ok (Result) then
   --        declare
   --           Chunks : constant File_Chunk_Vector := Chunk_Vector_Result.Unwrap (Result);
   --        begin
   --           Put_Line ("Created" & Chunks.Length'Image & " chunks");
   --           -- Process chunks...
   --        end;
   --     else
   --        Put_Line ("Error: " & To_String (Chunk_Vector_Result.Unwrap_Err (Result)));
   --     end if;
   --  end;
   --
   --  -- Process file segment with custom configuration
   --  declare
   --     Custom_Config : constant Chunk_Config := (
   --        Default_Chunk_Size => 1024 * 1024,  -- 1MB chunks
   --        Calculate_Checksums => False,       -- Skip for performance
   --        Use_Sequential_Access => True
   --     );
   --
   --     Start : constant Storage_Count := 1024 * 1024;      -- Start at 1MB
   --     Length : constant Storage_Count := 100 * 1024 * 1024; -- Process 100MB
   --
   --     Result : constant Chunk_Vector_Result.Result :=
   --        Create_Chunks_From_Memory_Map (
   --           Map => Mapped_File,
   --           Config => Custom_Config,
   --           Start_Offset => Start,
   --           Max_Bytes => Length
   --        );
   --  begin
   --     -- Handle result...
   --  end;
   --
   --  -- Parallel processing pattern
   --  declare
   --     Chunks : constant File_Chunk_Vector := Get_Chunks_From_File;
   --     Processor : Parallel_Processor_Access := Create_Processor;
   --  begin
   --     Processor.Start;
   --
   --     for Chunk of Chunks loop
   --        Processor.Submit_Chunk (Chunk);  -- Zero-copy submission
   --     end loop;
   --
   --     Processor.Signal_End_Of_Input;
   --     Processor.Wait_For_Completion;
   --  end;
   --  ```
   --
   --  ### Error Conditions
   --  Returns error results for:
   --  * Memory mapping failures (file not mapped, mapping lost)
   --  * Invalid offset/length parameters (beyond file boundaries)
   --  * Memory allocation failures (for chunk metadata)
   --  * File system errors (permissions, corruption)
   --
   --  ### Thread Safety
   --  This function is thread-safe for read-only access to the memory-mapped file.
   --  Multiple tasks can call this function concurrently on the same mapped file.
   function Create_Chunks_From_Memory_Map
     (Map          : Memory_Mapped_File_Interface'Class;
      Config       : Chunk_Config := (others => <>);
      Start_Offset : Storage_Count := 0;
      Max_Bytes    : Storage_Count := 0) -- 0 means entire file
      return Chunk_Vector_Result.Result
   with Pre => Map.Is_Mapped;

   --  Create a single chunk from memory-mapped file region
   function Create_Single_Chunk_From_Memory_Map
     (Map                : Memory_Mapped_File_Interface'Class;
      Sequence_Number    : Natural;
      Offset             : Storage_Count;
      Length             : Storage_Count;
      Is_Final           : Boolean;
      Calculate_Checksum : Boolean := True) return File_Chunk_Type
   with
     Pre =>
       Map.Is_Mapped
       and then Offset + Length <= Map.Get_Size
       and then Length > 0;
   --  Creates a single File_Chunk from a specific region of memory-mapped data

   --  Utility function to determine optimal chunk size based on file size
   function Calculate_Optimal_Chunk_Size
     (File_Size        : Storage_Count;
      Available_Memory : Storage_Count := 0) -- 0 means auto-detect
      return Positive
   with
     Pre => File_Size > 0,
     Post =>
       Calculate_Optimal_Chunk_Size'Result >= 1024
       and then  -- Min 1KB
                            Calculate_Optimal_Chunk_Size'Result
                            <= Integer (File_Size)
       and then Calculate_Optimal_Chunk_Size'Result
                <= 512 * 1024 * 1024;  -- Max 512MB
   --  Calculates optimal chunk size based on file size and available memory
   --  Uses adaptive algorithms similar to the Rust implementation

   --  Check if memory mapping should be used for a file
   function Should_Use_Memory_Mapping_For_File
     (File_Size        : Storage_Count;
      Available_Memory : Storage_Count := 0) -- 0 means auto-detect
      return Boolean
   with
     Pre => File_Size > 0,
     Post =>
       (if File_Size < 100 * 1024 * 1024
        then not Should_Use_Memory_Mapping_For_File'Result)
       and then (if File_Size > 1024 * 1024 * 1024
                 then not Should_Use_Memory_Mapping_For_File'Result);
   --  Determines if memory mapping is beneficial for the given file size

private

   --  Internal function to create Stream_Element_Array_Access from memory view
   function Create_Stream_Array_Access_From_Memory
     (View : Memory_View; Offset : Storage_Count; Length : Storage_Count)
      return Pipelib
               .Core
               .Domain
               .Value_Objects
               .File_Chunk
               .Stream_Element_Array_Access
   with
     Pre =>
       Offset + Length <= View.Size
       and then Length > 0
       and then View.Address /= System.Null_Address,
     Post =>
       Create_Stream_Array_Access_From_Memory'Result /= null
       and then Create_Stream_Array_Access_From_Memory'Result.all'Length
                = Natural (Length);
   --  Creates a Stream_Element_Array_Access that points to memory-mapped data
   --  This enables zero-copy access to the file data

   --  Internal function to convert memory address to stream elements
   function Memory_To_Stream_Elements
     (Address : System.Address; Length : Storage_Count)
      return Pipelib
               .Core
               .Domain
               .Value_Objects
               .File_Chunk
               .Stream_Element_Array_Access
   with
     Pre => Address /= System.Null_Address and then Length > 0,
     Post =>
       Memory_To_Stream_Elements'Result /= null
       and then Memory_To_Stream_Elements'Result.all'Length = Natural (Length);
   --  Converts memory address and length to Stream_Element_Array_Access

end Pipelib.Infrastructure.IO.Memory_Mapped_Chunk_Adapter;
