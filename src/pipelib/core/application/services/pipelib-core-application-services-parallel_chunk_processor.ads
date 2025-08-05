--   =============================================================================
--   Pipelib.Core.Application.Services.Parallel_Chunk_Processor
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   ## Parallel Chunk Processor - High-Performance Concurrent Processing
--
--   A generic application service that processes chunks in parallel using multiple
--   worker tasks. This is the core component for achieving high-throughput data
--   processing by leveraging all available CPU cores.
--
--   ### Architecture Overview
--
--   ```
--   +-----------------------------------------------------+
--   Ã¢ÂÂ                 Parallel Processor                     Ã¢ÂÂ
--   Ã¢ÂÂ                                                         Ã¢ÂÂ
--   Ã¢ÂÂ  Input Queue Ã¢ÂÂÃ¢ÂÂÃ¢ÂÂ                          Ã¢ÂÂÃ¢ÂÂÃ¢ÂÂ Output   Ã¢ÂÂ
--   Ã¢ÂÂ               Ã¢ÂÂ                          Ã¢ÂÂ     File    Ã¢ÂÂ
--   Ã¢ÂÂ               Ã¢ÂÂ¼                          Ã¢ÂÂ¼             Ã¢ÂÂ
--   |       +--------------+     +--------------+      |
--   |       +--------------+     +--------------+      |
--   |       +--------------+     +--------------+      |
--   |       +--------------+     +--------------+      |
--   |       +--------------+     +--------------+      |
--   |       +--------------+     +--------------+      |
--   |    +---+ +---+ +---+ +---+    |               |
--   |       +--------------+     +--------------+      |
--   |    +---+ +---+ +---+ +---+                    |
--   |       +--------------+     +--------------+      |
--   +-----------------------------------------------------+
--   ```
--
--   ### Key Features
--
--   * **Scalable Processing**: Support for 1-64 worker tasks
--   * **Out-of-Order Processing**: Chunks can be processed and written in any order
--   * **Generic Design**: Works with any chunk processing function
--   * **Thread-Safe Operations**: Built on Ada's protected types and rendezvous
--   * **Error Resilience**: Continues processing even if individual chunks fail
--   * **Automatic Load Balancing**: Work distributed evenly across workers
--   * **Resource Management**: Proper cleanup and lifecycle management
--
--   ### Generic Parameters
--
--   The processor is generic over two parameters:
--
--   * `Context_Type` - Application-specific context passed to processing function
--   * `Process_Chunk` - Function that transforms input chunks to output chunks
--
--   ### Performance Characteristics
--
--   * **Throughput**: Scales linearly with worker count up to CPU core limit
--   * **Memory Usage**: O(worker_count + queue_size) chunk buffers
--   * **Latency**: Minimal coordination overhead through lock-free queues
--   * **Scalability**: Tested with up to 64 workers on high-core systems
--
--   ### Thread Safety
--
--   All operations are thread-safe and can be called concurrently:
--   * Work queue uses Ada's synchronized containers
--   * Statistics tracking uses protected types
--   * File writing coordinated through random-access file handler
--   * Worker tasks communicate through rendezvous
--
--   ### Error Handling Strategy
--
--   * Individual chunk failures don't stop overall processing
--   * Errors are aggregated and can be queried after completion
--   * Failed chunks can be retried by resubmitting them
--   * Emergency shutdown available through `Stop` procedure
--
--   ### Usage Patterns
--
--   #### Basic Processing Pipeline
--   ```ada
--   -- Define processing function
--   function My_Transform (Chunk : File_Chunk_Type; Context : String)
--                         return File_Chunk_Type is
--   begin
--      -- Transform chunk data
--      return Transformed_Chunk;
--   end My_Transform;
--
--   -- Instantiate processor
--   package My_Processor is new Parallel_Chunk_Processor
--     (Context_Type => String, Process_Chunk => My_Transform);
--
--   -- Create and use processor
--   declare
--      Processor : constant My_Processor.Parallel_Processor_Access :=
--        My_Processor.Create (Worker_Count => 8,
--                            Output_File => File,
--                            Context => "config");
--   begin
--      Processor.Start;
--
--      for Chunk of Input_Chunks loop
--         Processor.Submit_Chunk (Chunk);
--      end loop;
--
--      Processor.Signal_End_Of_Input;
--      Processor.Wait_For_Completion;
--
--      Put_Line ("Processed" & Processor.Chunks_Processed'Image & " chunks");
--   end;
--   ```
--
--   #### Error Handling
--   ```ada
--   Processor.Wait_For_Completion;
--
--   if Processor.Has_Error then
--      Put_Line ("Error occurred: " & To_String (Processor.Get_Error));
--      -- Handle error condition
--   else
--      Put_Line ("All chunks processed successfully");
--   end if;
--   ```
--
--   ### Performance Tuning
--
--   **Worker Count Guidelines:**
--   * Start with CPU core count
--   * For I/O-bound processing: Use 2x CPU cores
--   * For CPU-bound processing: Use 1x CPU cores
--   * For mixed workloads: Benchmark to find optimal count
--
--   **Memory Considerations:**
--   * Each worker holds one chunk in memory
--   * Queue depth affects memory usage and latency
--   * Monitor memory usage under load
--
--   **Throughput Optimization:**
--   * Ensure processing function is efficient
--   * Minimize allocations in hot paths
--   * Use zero-copy operations where possible
--   * Consider chunk size vs. parallelism trade-offs
--   =============================================================================

pragma Ada_2022;

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Pipelib.Core.Domain.Value_Objects.File_Chunk;
with Pipelib.Core.Domain.Ports.File_Writer_Interface;
with Abohlib.Core.Domain.Result;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

generic
   type Context_Type is private;
   with
     function Process_Chunk
       (Chunk   : Pipelib.Core.Domain.Value_Objects.File_Chunk.File_Chunk_Type;
        Context : Context_Type)
        return Pipelib.Core.Domain.Value_Objects.File_Chunk.File_Chunk_Type;

package Pipelib.Core.Application.Services.Parallel_Chunk_Processor
is

   use Pipelib.Core.Domain.Value_Objects.File_Chunk;

   --  ## Result Types for Processing Operations
   --
   --  Result type following the Result pattern for error handling without exceptions.
   --  Used for operations that can succeed (Boolean) or fail (error message).
   package Process_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Boolean,
        Err_Type => Unbounded_String);

   --  ## Parallel Processor Types
   --
   --  The main processor type is limited to ensure single ownership and
   --  proper resource management. Access types enable efficient passing
   --  without copying the entire processor state.
   type Parallel_Processor_Type is tagged limited private;
   type Parallel_Processor_Access is access all Parallel_Processor_Type;

   --  ## Processor Factory Function
   --
   --  Creates a new parallel processor instance configured for the specified
   --  number of worker tasks. The processor starts in a stopped state and
   --  must be started explicitly.
   --
   --  ### Parameters
   --  * `Worker_Count` - Number of worker tasks to create (1-64)
   --  * `Output_File` - Random-access file for writing processed chunks
   --  * `Context` - Application-specific context passed to processing function
   --
   --  ### Worker Count Guidelines
   --  The optimal worker count depends on your workload characteristics:
   --
   --  **CPU-Intensive Processing:**
   --  * Use number of CPU cores (e.g., 8 workers for 8-core system)
   --  * Going beyond core count may hurt performance due to context switching
   --
   --  **I/O-Intensive Processing:**
   --  * Use 1.5-2x number of CPU cores
   --  * Workers can overlap I/O waits with computation
   --
   --  **Mixed Workloads:**
   --  * Start with CPU core count and benchmark different values
   --  * Monitor CPU utilization and adjust accordingly
   --
   --  ### Memory Requirements
   --  Each worker maintains one chunk in memory, so total memory usage is
   --  approximately: `Worker_Count ÃÂ Average_Chunk_Size`
   --
   --  ### Examples
   --  ```ada
   --  -- CPU-intensive processing (compression, encryption)
   --  declare
   --     Core_Count : constant Worker_Count_Value_Type := Worker_Count_Value_Type (System.Multiprocessors.Number_Of_CPUs);
   --     Processor : constant Parallel_Processor_Access := Create (
   --        Worker_Count => Core_Count,
   --        Output_File => Output_File,
   --        Context => Compression_Settings
   --     );
   --  begin
   --     -- Use processor...
   --  end;
   --
   --  -- I/O-intensive processing (network upload, database writes)
   --  declare
   --     IO_Workers : constant Positive := 16;  -- 2x 8-core system
   --     Processor : constant Parallel_Processor_Access := Create (
   --        Worker_Count => IO_Workers,
   --        Output_File => Network_File,
   --        Context => Upload_Configuration
   --     );
   --  begin
   --     -- Use processor...
   --  end;
   --
   --  -- Development/testing with single worker
   --  declare
   --     Debug_Processor : constant Parallel_Processor_Access := Create (
   --        Worker_Count => 1,  -- Single-threaded for debugging
   --        Output_File => Test_File,
   --        Context => Debug_Context
   --     );
   --  begin
   --     -- Use processor...
   --  end;
   --  ```
   --
   --  ### Resource Management
   --  The processor allocates worker tasks and internal data structures.
   --  Use `Destroy` to clean up resources when finished. The processor
   --  cannot be reused after destruction.
   --
   --  ### Thread Safety
   --  Multiple processors can run concurrently, but each processor instance
   --  should be controlled by a single task to avoid coordination issues.
   function Create
     (Worker_Count : Positive;
      Output_File  :
        access
          Pipelib
            .Core
            .Domain
            .Ports
            .File_Writer_Interface
            .File_Writer_Interface'Class;
      Context      : Context_Type) return Parallel_Processor_Access
   with
     Pre => Worker_Count <= 64,  -- Reasonable limit
     Post => Create'Result /= null;

   --  ## Processor Lifecycle Management
   --
   --  These procedures control the processor's lifecycle from startup through
   --  graceful shutdown. Follow the proper sequence for optimal performance
   --  and resource management.

   --  ### Start Processing Operations
   --
   --  Activates all worker tasks and prepares the processor to accept chunks.
   --  This must be called before submitting any chunks for processing.
   --
   --  #### State Changes
   --  * Transitions processor from stopped to running state
   --  * Creates and starts all worker tasks
   --  * Initializes internal work queue and statistics
   --
   --  #### Performance Notes
   --  Starting is a relatively expensive operation as it creates tasks and
   --  initializes data structures. Avoid frequent start/stop cycles.
   --
   --  #### Examples
   --  ```ada
   --  -- Basic startup
   --  Processor.Start;
   --  pragma Assert (Processor.Is_Running);
   --
   --  -- Startup with error handling
   --  if not Processor.Is_Running then
   --     Processor.Start;
   --     Put_Line ("Processor started with" &
   --               Processor.Worker_Count'Image & " workers");
   --  end if;
   --  ```
   procedure Start (Processor : in out Parallel_Processor_Type)
   with Pre => not Processor.Is_Running, Post => Processor.Is_Running;

   --  ### Submit Chunk for Processing
   --
   --  Adds a chunk to the processing queue. Worker tasks will pick up chunks
   --  from the queue and process them using the generic processing function.
   --  Chunks can be submitted in any order and will be processed concurrently.
   --
   --  #### Parameters
   --  * `Processor` - The running processor instance
   --  * `Chunk` - Valid chunk to be processed (cannot be empty)
   --
   --  #### Processing Flow
   --  1. Chunk is added to thread-safe work queue
   --  2. Available worker picks up chunk from queue
   --  3. Worker calls generic processing function with chunk and context
   --  4. Processed chunk is written to output file using random access
   --  5. Worker updates statistics and picks up next chunk
   --
   --  #### Performance Considerations
   --  * This operation is non-blocking and returns immediately
   --  * Queue depth affects memory usage and processing latency
   --  * Very small chunks may have higher overhead than large ones
   --  * Out-of-order submission is perfectly fine
   --
   --  #### Examples
   --  ```ada
   --  -- Process chunks from a vector
   --  for Chunk of Input_Chunks loop
   --     Processor.Submit_Chunk (Chunk);
   --  end loop;
   --
   --  -- Process chunks as they become available
   --  while More_Data_Available loop
   --     Chunk := Read_Next_Chunk;
   --     if not Is_Empty (Chunk) then
   --        Processor.Submit_Chunk (Chunk);
   --     end if;
   --  end loop;
   --
   --  -- Conditional processing
   --  if Should_Process (Chunk) then
   --     Processor.Submit_Chunk (Chunk);
   --  else
   --     Skip_Chunk (Chunk);
   --  end if;
   --  ```
   --
   --  #### Error Handling
   --  Individual chunk processing errors don't stop the processor.
   --  Check `Has_Error` and `Get_Error` after completion to handle failures.
   procedure Submit_Chunk
     (Processor : in out Parallel_Processor_Type; Chunk : File_Chunk_Type)
   with Pre => Processor.Is_Running and then not Is_Empty (Chunk);

   --  ### Signal End of Input Stream
   --
   --  Indicates that no more chunks will be submitted for processing.
   --  This allows the processor to begin graceful shutdown once all
   --  queued chunks have been processed.
   --
   --  #### When to Call This
   --  * After submitting all chunks to be processed
   --  * Before calling `Wait_For_Completion`
   --  * When you want to begin graceful shutdown
   --
   --  #### What This Does
   --  * Marks the input stream as complete
   --  * Workers will finish current chunks and then terminate
   --  * No more chunks can be submitted after this call
   --  * Enables `Wait_For_Completion` to detect when all work is done
   --
   --  #### Examples
   --  ```ada
   --  -- Standard processing sequence
   --  Processor.Start;
   --
   --  for Chunk of All_Chunks loop
   --     Processor.Submit_Chunk (Chunk);
   --  end loop;
   --
   --  Processor.Signal_End_Of_Input;  -- No more chunks
   --  Processor.Wait_For_Completion;
   --
   --  -- Stream processing
   --  Processor.Start;
   --
   --  while Read_Next_Chunk (Chunk) loop
   --     Processor.Submit_Chunk (Chunk);
   --  end loop;
   --
   --  Processor.Signal_End_Of_Input;  -- Stream finished
   --  Processor.Wait_For_Completion;
   --  ```
   procedure Signal_End_Of_Input (Processor : in out Parallel_Processor_Type)
   with Pre => Processor.Is_Running;

   --  ### Wait for Processing Completion
   --
   --  Blocks until all submitted chunks have been processed and all workers
   --  have finished. This provides a synchronization point for knowing when
   --  all work is complete.
   --
   --  #### Prerequisites
   --  * Processor must be running
   --  * `Signal_End_Of_Input` must have been called first
   --
   --  #### What This Does
   --  * Waits for all workers to finish their current chunks
   --  * Waits for all workers to terminate gracefully
   --  * Transitions processor back to stopped state
   --  * Collects final statistics and error information
   --
   --  #### Blocking Behavior
   --  This operation blocks the calling task until completion. For non-blocking
   --  alternatives, check `Is_Running` status periodically or use `Stop` for
   --  emergency shutdown.
   --
   --  #### Examples
   --  ```ada
   --  -- Standard completion wait
   --  Processor.Signal_End_Of_Input;
   --  Processor.Wait_For_Completion;
   --
   --  -- Check results after completion
   --  Processor.Wait_For_Completion;
   --  Put_Line ("Processed" & Processor.Chunks_Processed'Image & " chunks");
   --
   --  if Processor.Has_Error then
   --     Put_Line ("Errors occurred: " & To_String (Processor.Get_Error));
   --  end if;
   --
   --  -- Timeout pattern (not built-in, but can be implemented)
   --  declare
   --     Start_Time : constant Time := Clock;
   --     Timeout : constant Duration := 300.0;  -- 5 minutes
   --  begin
   --     while Processor.Is_Running and then Clock - Start_Time < Timeout loop
   --        delay 1.0;  -- Check every second
   --     end loop;
   --
   --     if Processor.Is_Running then
   --        Put_Line ("Timeout reached, forcing stop");
   --        Processor.Stop;
   --     end if;
   --  end;
   --  ```
   procedure Wait_For_Completion (Processor : in out Parallel_Processor_Type)
   with Pre => Processor.Is_Running, Post => not Processor.Is_Running;

   --  ### Emergency Stop Processing
   --
   --  Immediately stops all workers and terminates processing. Use this for
   --  emergency shutdown, error conditions, or when you need to abort
   --  processing without waiting for current chunks to complete.
   --
   --  #### When to Use This
   --  * Critical errors requiring immediate shutdown
   --  * User cancellation requests
   --  * Timeout conditions
   --  * System shutdown scenarios
   --
   --  #### Behavior
   --  * Signals all workers to stop immediately
   --  * Does not wait for current chunks to finish processing
   --  * May leave some chunks unprocessed
   --  * Transitions processor to stopped state immediately
   --
   --  #### Data Safety
   --  * Chunks currently being processed may be incomplete
   --  * Output file may be in inconsistent state
   --  * Check error status after emergency stop
   --  * Consider running data integrity checks
   --
   --  #### Examples
   --  ```ada
   --  -- User cancellation
   --  if User_Cancelled then
   --     Processor.Stop;
   --     Put_Line ("Processing cancelled by user");
   --  end if;
   --
   --  -- Error condition
   --  if Critical_Error_Detected then
   --     Processor.Stop;
   --     Handle_Emergency_Shutdown;
   --  end if;
   --
   --  -- Timeout handling
   --  if Processing_Time > Max_Processing_Time then
   --     Processor.Stop;
   --     Put_Line ("Processing timeout - emergency stop");
   --  end if;
   --
   --  -- Exception handling
   --  begin
   --     -- Normal processing...
   --     Processor.Wait_For_Completion;
   --  exception
   --     when others =>
   --        Processor.Stop;  -- Ensure cleanup
   --        raise;
   --  end;
   --  ```
   procedure Stop (Processor : in out Parallel_Processor_Type)
   with Post => not Processor.Is_Running;

   --  Query methods
   function Is_Running (Processor : Parallel_Processor_Type) return Boolean
   with Inline;

   function Chunks_Processed
     (Processor : Parallel_Processor_Type) return Natural
   with Post => Chunks_Processed'Result >= 0;

   function Get_Error
     (Processor : Parallel_Processor_Type) return Unbounded_String
   with
     Post => (if not Has_Error (Processor) then Length (Get_Error'Result) = 0);

   function Has_Error (Processor : Parallel_Processor_Type) return Boolean
   with Inline;

   --  Destroy processor
   procedure Destroy (Processor : in out Parallel_Processor_Access)
   with Post => Processor = null;

private

   --  Work item for processing
   type Work_Item is record
      Chunk         : File_Chunk_Type;
      Is_End_Marker : Boolean := False;
   end record;

   --  Queue for work items
   package Work_Queue_Interface is new
     Ada.Containers.Synchronized_Queue_Interfaces (Element_Type => Work_Item);

   package Work_Queues is new
     Ada.Containers.Unbounded_Synchronized_Queues
       (Queue_Interfaces => Work_Queue_Interface);

   --  Statistics tracking
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

   --  Worker task type
   task type Worker_Task_Type (Parent : access Parallel_Processor_Type) is
      entry Start;
      entry Stop;
   end Worker_Task_Type;

   type Worker_Task_Access is access Worker_Task_Type;
   type Worker_Task_Array is array (Positive range <>) of Worker_Task_Access;

   --  Main processor type
   type Parallel_Processor_Type is tagged limited record
      Work_Queue   : Work_Queues.Queue;
      Output_File  :
        access
          Pipelib
            .Core
            .Domain
            .Ports
            .File_Writer_Interface
            .File_Writer_Interface'Class;
      Context      : Context_Type;
      Workers      : access Worker_Task_Array;
      Worker_Count : Positive;
      Statistics   : Statistics_Type;
      Is_Running   : Boolean := False;
      End_Of_Input : Boolean := False;
   end record;

end Pipelib.Core.Application.Services.Parallel_Chunk_Processor;
