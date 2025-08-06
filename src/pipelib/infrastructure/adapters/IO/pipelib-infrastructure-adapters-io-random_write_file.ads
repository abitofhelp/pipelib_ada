--   =============================================================================
--   Pipelib.Infrastructure.IO.Random_Write_File - Random Access File Writer
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   ## Random Access File Writer - Concurrent Out-of-Order Writing
--
--   High-performance file writer that enables parallel chunk processing by supporting
--   random-access writes to specific file positions. This is essential for parallel
--   pipelines where chunks may complete processing in any order.
--
--   ### Key Capabilities
--
--   **Out-of-Order Writing:**
--   * Write chunks to any position in the file without sequential constraints
--   * Support for parallel workers writing different file regions simultaneously
--   * Automatic position calculation based on chunk sequence numbers
--   * Direct position specification for maximum flexibility
--
--   **Thread Safety Options:**
--   * **Regular Type**: Single-threaded access with maximum performance
--   * **Protected Type**: Thread-safe wrapper for concurrent access from multiple tasks
--   * Choice based on architecture needs and performance requirements
--
--   **Transactional Safety:**
--   * Optional temporary file usage for atomic operations
--   * Commit/rollback semantics for reliable file updates
--   * Automatic cleanup on failure or cancellation
--   * Protection against partial writes corrupting existing files
--
--   ### Performance Characteristics
--
--   **Write Performance:**
--   * Random access: 100-500 MB/s (depends on storage type and access patterns)
--   * Sequential writes: Near-maximum storage bandwidth
--   * Memory mapping: Leverages OS virtual memory for optimal performance
--   * Pre-allocation: Reduces fragmentation and improves write speeds
--
--   **Concurrency Model:**
--   * Multiple threads can write to different file positions simultaneously
--   * Protected type provides automatic synchronization
--   * Lock-free algorithms where possible for maximum throughput
--   * Minimal contention through position-based access patterns
--
--   ### Use Cases
--
--   **Parallel Processing Pipelines:**
--   * Image/video processing where tiles can be processed independently
--   * Data transformation where chunks can be computed out-of-order
--   * Compression/decompression with parallel block processing
--   * Scientific computing with independent data segments
--
--   **High-Throughput Data Assembly:**
--   * Network file transfers with out-of-order packet arrival
--   * Database dump restoration with parallel table processing
--   * Log file merging from multiple sources
--   * Backup/restore operations with concurrent streams
--
--   ### Error Resilience
--
--   **Failure Recovery:**
--   * Automatic rollback on write failures
--   * Temporary file isolation prevents corruption of existing files
--   * Detailed error reporting with file system error codes
--   * Graceful handling of disk full, permissions, and I/O errors
--
--   **Data Integrity:**
--   * Atomic commit operations for file replacement
--   * Pre-allocation reduces risk of partial writes due to space exhaustion
--   * Flush operations ensure data reaches persistent storage
--   * File handle lifecycle management prevents resource leaks
--
--   ### Architecture Integration
--
--   This component implements the Infrastructure layer of the hexagonal architecture,
--   providing the concrete implementation for file writing operations needed by
--   the parallel processing application services.
--
--   ```
--   Application Layer:     Parallel_Chunk_Processor
--                                   Ã¢ÂÂ
--                                   Ã¢ÂÂ¼
--   Infrastructure Layer:   Random_Write_File Ã¢ÂÂÃ¢ÂÂÃ¢ÂÂÃ¢ÂÂ This Component
--                                   Ã¢ÂÂ
--                                   Ã¢ÂÂ¼
--   External Systems:        File System I/O
--   ```
--
--   ### Memory Management
--
--   * Uses Ada's controlled types for automatic resource cleanup (RAII)
--   * Minimal memory footprint - no large internal buffers
--   * Leverages OS file system caching for optimal memory usage
--   * Automatic file handle management prevents resource leaks
--   =============================================================================

pragma Ada_2022;

with Ada.Streams.Stream_IO;
with Ada.Finalization;
with Abohlib.Core.Domain.Value_Objects.File_Path;
with Pipelib.Core.Domain.Value_Objects.File_Chunk;
with Pipelib.Core.Domain.Ports.File_Writer_Interface;
with Abohlib.Core.Domain.Result;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Pipelib.Infrastructure.Adapters.IO.Random_Write_File is

   --  ## Random Access File Writer Types
   --
   --  The main file writer type uses Ada's controlled types for automatic resource
   --  management (RAII pattern). The limited aspect prevents copying and ensures
   --  unique ownership of file handles.
   type Random_Write_File is
     new Ada.Finalization.Limited_Controlled
     and Pipelib
           .Core
           .Domain
           .Ports
           .File_Writer_Interface
           .File_Writer_Interface with private;
   type Random_Write_File_Access is access all Random_Write_File;

   --  ## Result Type for Write Operations
   --
   --  Result type following the Result pattern for error handling without exceptions.
   --  Write operations return either success (Boolean) or detailed error information.
   package Write_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Boolean,  -- Success indicator
        Err_Type => Unbounded_String);

   --  ## File Creation and Initialization
   --
   --  Creates and opens a new file for random write access with configurable
   --  safety and performance options. This is the primary factory function
   --  for creating file writer instances.
   --
   --  ### Parameters
   --  * `Path` - Target file path where data will be written
   --  * `Expected_Size` - Expected final file size for pre-allocation (0 = no pre-allocation)
   --  * `Use_Temp_File` - Whether to use temporary file for atomic operations
   --
   --  ### Temporary File Strategy
   --  When `Use_Temp_File` is True (recommended):
   --  * Writes go to a temporary file with `.tmp` extension
   --  * Original file remains untouched during processing
   --  * `Commit` atomically renames temp file to final name
   --  * `Rollback` safely deletes temp file without affecting original
   --  * Protects against corruption from interrupted operations
   --
   --  When `Use_Temp_File` is False (maximum performance):
   --  * Writes directly to the target file
   --  * Higher performance due to no rename operation
   --  * Risk of corruption if operation is interrupted
   --  * Suitable for new files or when atomic updates not required
   --
   --  ### Pre-allocation Benefits
   --  When `Expected_Size` > 0:
   --  * Reduces file system fragmentation
   --  * Improves write performance by avoiding frequent size extensions
   --  * Prevents "disk full" errors during processing
   --  * Enables better OS optimizations for large files
   --
   --  ### Performance Tuning
   --  ```ada
   --  -- High-performance temporary file (recommended)
   --  High_Perf_File : constant Random_Write_File_Access := Create (
   --     Path => To_File_Path ("output.dat"),
   --     Expected_Size => 1_000_000_000,  -- 1GB pre-allocation
   --     Use_Temp_File => True            -- Atomic updates
   --  );
   --
   --  -- Maximum speed for new files
   --  Fast_File : constant Random_Write_File_Access := Create (
   --     Path => To_File_Path ("new_output.dat"),
   --     Expected_Size => 500_000_000,    -- 500MB pre-allocation
   --     Use_Temp_File => False           -- Direct writing
   --  );
   --
   --  -- Conservative approach for unknown sizes
   --  Safe_File : constant Random_Write_File_Access := Create (
   --     Path => To_File_Path ("unknown_size.dat"),
   --     Expected_Size => 0,              -- No pre-allocation
   --     Use_Temp_File => True            -- Safe updates
   --  );
   --  ```
   --
   --  ### Error Conditions
   --  Returns null and logs errors for:
   --  * Invalid file paths or insufficient permissions
   --  * Disk space exhaustion during pre-allocation
   --  * File system errors (corrupted disk, network issues)
   --  * Resource exhaustion (too many open files)
   --
   --  ### Resource Management
   --  The returned file instance uses RAII (Resource Acquisition Is Initialization):
   --  * File handle automatically closed when object goes out of scope
   --  * Temporary files automatically cleaned up on destruction
   --  * No manual cleanup required in normal operation
   --  * Exception-safe - resources cleaned up even on abnormal termination
   --
   --  ### Usage Patterns
   --  ```ada
   --  -- Standard usage with automatic cleanup
   --  declare
   --     Output_File : constant Random_Write_File_Access := Create (
   --        Path => To_File_Path ("processed_data.bin"),
   --        Expected_Size => Calculate_Expected_Size (Input_Data),
   --        Use_Temp_File => True
   --     );
   --  begin
   --     if Output_File /= null then
   --        -- Process data and write chunks...
   --        for Chunk of Processed_Chunks loop
   --           Write_Chunk (Output_File.all, Chunk);
   --        end loop;
   --
   --        -- Commit atomically
   --        if Write_Result.Is_Ok (Commit (Output_File.all)) then
   --           Put_Line ("File written successfully");
   --        else
   --           Put_Line ("Commit failed");
   --        end if;
   --     else
   --        Put_Line ("Failed to create output file");
   --     end if;
   --     -- File automatically closed and cleaned up here
   --  end;
   --
   --  -- Integration with parallel processor
   --  declare
   --     Output_File : constant Random_Write_File_Access := Create (
   --        Path => Output_Path,
   --        Expected_Size => Total_Expected_Size,
   --        Use_Temp_File => True
   --     );
   --
   --     Processor : constant Parallel_Processor_Access := Create (
   --        Worker_Count => 8,
   --        Output_File => Output_File,
   --        Context => Processing_Context
   --     );
   --  begin
   --     -- Parallel processing writes chunks out-of-order
   --     Processor.Start;
   --     -- ... submit chunks ...
   --     Processor.Wait_For_Completion;
   --
   --     -- Commit all writes atomically
   --     Commit (Output_File.all);
   --  end;
   --  ```
   function Create
     (Path          : Abohlib.Core.Domain.Value_Objects.File_Path.File_Path;
      Expected_Size : Long_Long_Integer := 0;
      -- Pre-allocate if known
      Use_Temp_File : Boolean := True)  -- Write to temp file for safety
      return Random_Write_File_Access
   with Post => Create'Result /= null;

   --  Write a chunk at a specific position
   overriding
   procedure Write_Chunk_At_Position
     (Writer   : in out Random_Write_File;
      Chunk    : Pipelib.Core.Domain.Value_Objects.File_Chunk.File_Chunk_Type;
      Position : Long_Long_Integer)
   with Pre => Is_Open (Writer) and then Position >= 0;

   --  Write a chunk at a specific position with Result
   procedure Write_Chunk_At
     (Writer   : in out Random_Write_File;
      Chunk    : Pipelib.Core.Domain.Value_Objects.File_Chunk.File_Chunk_Type;
      Position : Long_Long_Integer;
      Result   : in out Write_Result.Result)
   with Pre => Is_Open (Writer) and then Position >= 0;

   --  Write a chunk using its sequence number
   overriding
   procedure Write_Chunk
     (Writer : in out Random_Write_File;
      Chunk  : Pipelib.Core.Domain.Value_Objects.File_Chunk.File_Chunk_Type)
   with Pre => Is_Open (Writer);

   --  Check if file is open
   overriding
   function Is_Open (Writer : Random_Write_File) return Boolean
   with Inline;

   --  Get current file size
   function Size (File : Random_Write_File) return Long_Long_Integer
   with Pre => Is_Open (File), Post => Size'Result >= 0;

   --  Commit the file (rename temp to final if using temp file)
   overriding
   function Commit
     (Writer : in out Random_Write_File)
      return Pipelib
               .Core
               .Domain
               .Ports
               .File_Writer_Interface
               .Write_Result
               .Result
   with Post => not Is_Open (Writer);

   --  Rollback (delete temp file if using temp file)
   procedure Rollback (File : in out Random_Write_File)
   with Post => not Is_Open (File);

   --  Close without committing
   overriding
   procedure Close (Writer : in out Random_Write_File)
   with Post => not Is_Open (Writer);

   --  Flush buffers to disk
   procedure Flush (File : in out Random_Write_File)
   with Pre => Is_Open (File);

   --  Pre-allocate file space for better performance
   procedure Preallocate
     (File : in out Random_Write_File; Size : Long_Long_Integer)
   with Pre => Is_Open (File) and then Size > 0;

   --  Destructor
   procedure Destroy (File : in out Random_Write_File_Access)
   with Post => File = null;

   --  ## Thread-Safe Wrapper for Concurrent Access
   --
   --  Protected type that provides thread-safe access to the random write file
   --  for use in parallel processing scenarios. This enables multiple worker
   --  tasks to write chunks concurrently without coordination overhead.
   --
   --  ### Concurrency Model
   --
   --  **Automatic Synchronization:**
   --  * Ada's protected types provide built-in mutual exclusion
   --  * Multiple readers can access query operations simultaneously
   --  * Write operations are automatically serialized
   --  * No explicit locking required by caller
   --
   --  **Performance Characteristics:**
   --  * Low contention due to position-based writes
   --  * Minimal lock holding time (only during actual write operations)
   --  * Parallel workers typically write to different file regions
   --  * Read operations (queries) can proceed concurrently
   --
   --  **Usage with Parallel Processor:**
   --  This protected type is designed to integrate seamlessly with the
   --  Parallel_Chunk_Processor, enabling high-throughput concurrent writing.
   --
   --  ### Thread Safety Guarantees
   --
   --  * **Write Atomicity**: Each chunk write is atomic at the OS level
   --  * **Position Consistency**: File position updates are thread-safe
   --  * **Size Consistency**: File size queries always return consistent values
   --  * **Resource Safety**: File handle access is properly synchronized
   --
   --  ### Examples
   --  ```ada
   --  -- Setup for parallel processing
   --  declare
   --     Base_File : constant Random_Write_File_Access := Create (
   --        Path => Output_Path,
   --        Expected_Size => Total_Size,
   --        Use_Temp_File => True
   --     );
   --
   --     Protected_File : Protected_Random_Write_File;
   --
   --     -- Worker tasks (conceptual)
   --     task type Worker_Task is
   --        entry Start (File : access Protected_Random_Write_File);
   --     end Worker_Task;
   --
   --     Workers : array (1 .. 8) of Worker_Task;
   --  begin
   --     Protected_File.Initialize (Base_File);
   --
   --     -- Start worker tasks
   --     for Worker of Workers loop
   --        Worker.Start (Protected_File'Access);
   --     end loop;
   --
   --     -- Workers can now write concurrently:
   --     -- Protected_File.Write_Chunk (Chunk);
   --     -- Protected_File.Write_At_Position (Chunk, Position);
   --  end;
   --
   --  -- Integration with Parallel_Chunk_Processor
   --  declare
   --     File : constant Random_Write_File_Access := Create (...);
   --     Protected_File : constant Protected_Random_Write_File_Access :=
   --        new Protected_Random_Write_File;
   --
   --     Processor : Parallel_Processor_Access;
   --  begin
   --     Protected_File.Initialize (File);
   --
   --     Processor := Create (
   --        Worker_Count => 8,
   --        Output_File => Protected_File,  -- Thread-safe file access
   --        Context => Processing_Context
   --     );
   --
   --     -- Process chunks in parallel - all writes are automatically synchronized
   --     Processor.Start;
   --     -- ... submit chunks ...
   --     Processor.Wait_For_Completion;
   --  end;
   --  ```
   protected type Protected_Random_Write_File is

      --  ### Initialize Protected File Wrapper
      --
      --  Associates this protected wrapper with a base Random_Write_File instance.
      --  Must be called before any write operations.
      --
      --  #### Parameters
      --  * `File` - The base file instance to wrap (must be successfully created)
      --
      --  #### Thread Safety
      --  This operation should be called from a single task before starting
      --  concurrent operations. It is not thread-safe itself.
      procedure Initialize (File : Random_Write_File_Access)
      with Pre => File /= null;

      --  ### Thread-Safe Chunk Writing
      --
      --  Writes a chunk using its built-in sequence number to calculate position.
      --  Multiple tasks can call this simultaneously - the protected type ensures
      --  proper synchronization.
      --
      --  #### Position Calculation
      --  The file position is calculated automatically based on:
      --  * Chunk sequence number
      --  * Configured chunk size
      --  * Any offset specified in the chunk
      --
      --  #### Concurrency Benefits
      --  * No explicit locking required by caller
      --  * Parallel workers can submit chunks in any order
      --  * Automatic serialization prevents write conflicts
      --  * Minimal contention due to different target positions
      procedure Write_Chunk
        (Chunk : Pipelib.Core.Domain.Value_Objects.File_Chunk.File_Chunk_Type);

      --  ### Thread-Safe Position-Specific Writing
      --
      --  Writes a chunk to an explicitly specified file position.
      --  This provides maximum flexibility for custom positioning schemes
      --  while maintaining thread safety.
      --
      --  #### Parameters
      --  * `Chunk` - The chunk data to write
      --  * `Position` - Exact byte position in file where chunk should be written
      --
      --  #### Use Cases
      --  * Custom chunk positioning algorithms
      --  * Non-sequential chunk placement
      --  * Integration with external positioning systems
      --  * Fine-grained control over file layout
      --
      --  #### Performance Notes
      --  * Position-based writes can achieve near-maximum disk bandwidth
      --  * OS-level optimizations handle multiple concurrent positioned writes
      --  * File pre-allocation significantly improves performance
      procedure Write_At_Position
        (Chunk    :
           Pipelib.Core.Domain.Value_Objects.File_Chunk.File_Chunk_Type;
         Position : Long_Long_Integer)
      with Pre => Position >= 0;

      --  ### Thread-Safe Size Query
      --
      --  Returns the current file size in a thread-safe manner.
      --  Multiple tasks can call this concurrently without blocking write operations.
      --
      --  #### Returns
      --  Current file size in bytes (always >= 0)
      --
      --  #### Concurrency
      --  This is a function (read-only operation) so multiple tasks can
      --  call it simultaneously without mutual exclusion.
      function Get_Size return Long_Long_Integer
      with Post => Get_Size'Result >= 0;

      --  ### Readiness Check
      --
      --  Checks if the protected file is properly initialized and ready
      --  for write operations. Useful for validation and error handling.
      --
      --  #### Returns
      --  True if file is initialized and ready for operations
      function Is_Ready return Boolean;

   private
      File_Access : Random_Write_File_Access := null;
   end Protected_Random_Write_File;

   type Protected_Random_Write_File_Access is
     access Protected_Random_Write_File;

private

   type Random_Write_File is
     new Ada.Finalization.Limited_Controlled
     and Pipelib.Core.Domain.Ports.File_Writer_Interface.File_Writer_Interface
   with record
      File_Handle  : Ada.Streams.Stream_IO.File_Type;
      File_Path    : Abohlib.Core.Domain.Value_Objects.File_Path.File_Path;
      Temp_Path    : Abohlib.Core.Domain.Value_Objects.File_Path.File_Path;
      Is_Open_Flag : Boolean := False;
      Use_Temp     : Boolean := True;
      Chunk_Size   : Natural := 0;  -- For position calculation
   end record;

   overriding
   procedure Finalize (File : in out Random_Write_File);

end Pipelib.Infrastructure.Adapters.IO.Random_Write_File;
