--   =============================================================================
--   Pipelib.Core.Domain.Ports.File_Writer_Interface - File Writing Port
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   ## File Writer Domain Port Interface
--
--   This port defines the abstraction for file writing operations required by
--   the domain layer. It follows the hexagonal architecture pattern where the
--   domain defines the interface and infrastructure provides implementations.
--
--   ### Purpose
--
--   **Dependency Inversion**: Allows application services to depend on abstractions
--   rather than concrete infrastructure implementations, enabling:
--   * Easy testing with mock implementations
--   * Pluggable infrastructure implementations
--   * Clean architectural boundaries
--   * Separation of business logic from technical concerns
--
--   **Domain-Driven Design**: Represents the domain's need for persistent file
--   output without exposing technical implementation details like:
--   * File system APIs
--   * Memory mapping strategies
--   * Concurrency mechanisms
--   * Platform-specific optimizations
--
--   ### Core Operations
--
--   **Chunk Writing**: Write processed data chunks to output files
--   * Position-based writing for parallel processing
--   * Automatic position calculation from chunk sequence
--   * Type-safe chunk data handling
--
--   **Transaction Management**: Ensure data integrity during write operations
--   * Atomic commit/rollback semantics
--   * Temporary file safety for reliable updates
--   * Resource cleanup and error recovery
--
--   **Resource Lifecycle**: Manage file resources throughout processing
--   * Open/close file handles
--   * Status checking for operational safety
--   * Proper resource cleanup on completion
--
--   ### Usage Pattern
--
--   ```ada
--   -- Application service depends on abstraction
--   procedure Process_Chunks(Writer : in out File_Writer_Interface'Class) is
--   begin
--      for Chunk of Processed_Chunks loop
--         Writer.Write_Chunk(Chunk);
--      end loop;
--      Writer.Commit;
--   end Process_Chunks;
--
--   -- Infrastructure provides concrete implementation
--   type Random_Write_File is new File_Writer_Interface with ...;
--   ```
--
--   ### Error Handling
--
--   All operations return Result types following domain error handling patterns:
--   * Success/failure indication without exceptions
--   * Detailed error context for debugging
--   * Structured error types for programmatic handling
--   * No technical error details exposed to domain
--
--   ### Contracts
--
--   Ada 2022 contracts ensure interface integrity:
--   * Preconditions validate call prerequisites
--   * Postconditions guarantee operation results
--   * Type invariants maintain interface consistency
--   * Abstract operations enforce implementation requirements
--
--   =============================================================================

pragma Ada_2022;

with Pipelib.Core.Domain.Value_Objects.File_Chunk;
with Abohlib.Core.Domain.Result;
with Ada.Strings.Unbounded;

package Pipelib.Core.Domain.Ports.File_Writer_Interface is

   --  Domain error types for file writing operations
   package Write_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Boolean,
        Err_Type => Ada.Strings.Unbounded.Unbounded_String);

   --  ## File Writer Interface
   --
   --  Abstract interface defining file writing operations required by the domain.
   --  Infrastructure layer must provide concrete implementations following these
   --  contracts and semantic guarantees.
   --
   --  ### Implementation Requirements
   --
   --  **Thread Safety**: Implementations may provide thread-safe variants
   --  **Resource Management**: Must use RAII patterns for automatic cleanup
   --  **Error Handling**: Must not propagate exceptions across boundaries
   --  **Performance**: Should optimize for parallel chunk processing patterns
   type File_Writer_Interface is limited interface;

   --  ## Write Chunk at Specific Position
   --
   --  Writes a data chunk to a specific byte position in the output file.
   --  Essential for parallel processing where chunks complete out-of-order.
   --
   --  ### Parameters
   --  * `Writer` - The file writer instance (must be open)
   --  * `Chunk` - Data chunk to write (validated domain object)
   --  * `Position` - Target byte position in file (0-based offset)
   --
   --  ### Behavior
   --  * Writes chunk data to exact file position
   --  * Overwrites existing data at that position
   --  * May buffer writes for performance optimization
   --  * Does not modify chunk sequence number or metadata
   --
   --  ### Error Conditions
   --  * File not open or invalid writer state
   --  * Invalid position (negative or beyond file limits)
   --  * I/O errors during write operation
   --  * Insufficient disk space or permissions
   procedure Write_Chunk_At_Position
     (Writer   : in out File_Writer_Interface;
      Chunk    : Pipelib.Core.Domain.Value_Objects.File_Chunk.File_Chunk_Type;
      Position : Long_Long_Integer)
   is abstract
   with Pre'Class => Position >= 0;

   --  ## Write Chunk by Sequence Number
   --
   --  Writes a chunk using its embedded sequence number to calculate position.
   --  Convenient for ordered processing where position calculation is automatic.
   --
   --  ### Parameters
   --  * `Writer` - The file writer instance (must be open)
   --  * `Chunk` - Data chunk to write (contains sequence number)
   --
   --  ### Position Calculation
   --  Position typically calculated as: Sequence_Number * Chunk_Size
   --  Exact calculation depends on implementation and chunk size strategy.
   --  Chunk metadata (sequence number, size) used for automatic positioning.
   --
   --  ### Use Cases
   --  * Sequential processing with automatic positioning
   --  * Simplified API when position calculation is standard
   --  * Batch processing with consistent chunk sizes
   procedure Write_Chunk
     (Writer : in out File_Writer_Interface;
      Chunk  : Pipelib.Core.Domain.Value_Objects.File_Chunk.File_Chunk_Type)
   is abstract;

   --  ## Commit All Writes
   --
   --  Ensures all buffered writes are flushed to persistent storage.
   --  Provides transaction-like semantics for reliable file operations.
   --
   --  ### Transactional Behavior
   --  * Flushes any buffered write operations
   --  * Moves temporary files to final locations (if applicable)
   --  * Synchronizes data to persistent storage
   --  * Makes all writes visible to other processes
   --
   --  ### Error Recovery
   --  * On failure, previous writes may be lost
   --  * Temporary files may be retained for recovery
   --  * File may be left in intermediate state
   --  * Caller should handle recovery or cleanup
   --
   --  ### Performance Impact
   --  * May be expensive operation (disk sync)
   --  * Should be called once after all writes complete
   --  * Avoid frequent commits during processing
   function Commit
     (Writer : in out File_Writer_Interface) return Write_Result.Result
   is abstract;

   --  ## Check Writer Status
   --
   --  Determines if the writer is ready for write operations.
   --  Used to validate writer state before attempting operations.
   --
   --  ### Status Conditions
   --  * True: Writer ready for write operations
   --  * False: Writer closed, failed, or not initialized
   --
   --  ### Usage Pattern
   --  ```ada
   --  if Writer.Is_Open then
   --     Writer.Write_Chunk(Data_Chunk);
   --  else
   --     Handle_Writer_Error;
   --  end if;
   --  ```
   function Is_Open (Writer : File_Writer_Interface) return Boolean
   is abstract;

   --  ## Close Writer
   --
   --  Closes the file writer and releases all associated resources.
   --  Should be called when processing is complete or on error recovery.
   --
   --  ### Resource Cleanup
   --  * Closes file handles and frees system resources
   --  * Cleans up temporary files if applicable
   --  * Releases memory buffers and caches
   --  * Ensures no resource leaks
   --
   --  ### State Changes
   --  * Writer becomes non-operational after close
   --  * Is_Open returns False after successful close
   --  * Subsequent write operations will fail
   --  * Multiple calls to Close are safe (idempotent)
   --
   --  ### Note on RAII
   --  While Close is provided for explicit cleanup, implementations should
   --  also use Ada's controlled types for automatic resource cleanup.
   procedure Close (Writer : in out File_Writer_Interface) is abstract;

end Pipelib.Core.Domain.Ports.File_Writer_Interface;
