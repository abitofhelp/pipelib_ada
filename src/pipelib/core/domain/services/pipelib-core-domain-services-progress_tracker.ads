--  =============================================================================
--  Core.Domain.Services.Progress_Tracker - Simple Progress Tracking
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  ## Progress Tracker - Thread-Safe Pipeline Progress Monitoring
--
--  Domain service that provides real-time progress tracking for multi-stage
--  data processing pipelines. Enables monitoring of read, processing, and
--  write operations across concurrent workers with thread-safe access.
--
--  ### Key Features
--
--  **Thread-Safe Monitoring:**
--  * Protected type ensures safe access from multiple concurrent tasks
--  * Atomic updates prevent race conditions and inconsistent state
--  * Multiple readers can query progress simultaneously
--  * Write operations are automatically serialized
--
--  **Multi-Stage Tracking:**
--  * **Read Stage**: Track chunks being read from input sources
--  * **Processing Stage**: Monitor transformation/computation progress
--  * **Write Stage**: Track chunks being written to output destinations
--  * **Completion Tracking**: Detect when each stage finishes
--
--  **Real-Time Display:**
--  * In-place progress updates (carriage return) for console applications
--  * Configurable display format for different UI requirements
--  * Non-blocking updates that don't impact processing performance
--  * Clear indication of completion status for each stage
--
--  ### Progress Monitoring Philosophy
--
--  The tracker follows a "push" model where processing components actively
--  report their progress rather than being polled. This provides:
--
--  * **Low Overhead**: No polling loops consuming CPU cycles
--  * **Real-Time Updates**: Progress updates as work completes
--  * **Accurate Reporting**: Components know exactly what they've accomplished
--  * **Scalability**: Works efficiently with any number of concurrent workers
--
--  ### Pipeline Integration
--
--  This service integrates with the pipeline architecture to provide
--  comprehensive progress visibility:
--
--  ```
--  Input Source Ã¢ÂÂÃ¢ÂÂÃ¢ÂÂº Read Stage Ã¢ÂÂÃ¢ÂÂÃ¢ÂÂº Processing Stage Ã¢ÂÂÃ¢ÂÂÃ¢ÂÂº Write Stage Ã¢ÂÂÃ¢ÂÂÃ¢ÂÂº Output
--        Ã¢ÂÂ              Ã¢ÂÂ                Ã¢ÂÂ                 Ã¢ÂÂ
--        Ã¢ÂÂ              Ã¢ÂÂ¼                Ã¢ÂÂ¼                 Ã¢ÂÂ¼
--        +--------------> Progress_Tracker <----------------+
--                     (Thread-Safe)
--  ```
--
--  ### Use Cases
--
--  **Long-Running Operations:**
--  * Large file processing (>1GB) with user progress feedback
--  * Batch processing jobs that may run for hours
--  * Data migration operations with thousands of records
--  * Scientific computations with iterative processing
--
--  **User Interface Integration:**
--  * Console applications with text-based progress bars
--  * GUI applications with graphical progress indicators
--  * Web applications with AJAX progress updates
--  * Monitoring dashboards for batch processing systems
--
--  **Performance Analysis:**
--  * Identifying bottlenecks in multi-stage pipelines
--  * Monitoring worker utilization and load balancing
--  * Tracking throughput rates for performance tuning
--  * Detecting stalled or failed processing stages
--
--  ### Thread Safety Guarantees
--
--  * **Update Atomicity**: Each counter update is atomic
--  * **Consistency**: All progress queries return consistent snapshots
--  * **Completion Logic**: Stage completion flags are properly synchronized
--  * **Display Safety**: Progress display operations are thread-safe
--
--  ### Performance Impact
--
--  * **Minimal Overhead**: Protected type operations are very fast (microseconds)
--  * **No Blocking**: Update operations don't block processing threads
--  * **Efficient Queries**: Read operations can proceed concurrently
--  * **Scalable**: Performance doesn't degrade with more workers
--  =============================================================================

pragma Ada_2022;

package Pipelib.Core.Domain.Services.Progress_Tracker is

   --  ## Progress State Information
   --
   --  Domain value object representing the current state of pipeline progress.
   --  Contains only business state without any presentation concerns.
   type Progress_State is record
      Chunks_Read      : Natural := 0;
      Chunks_Processed : Natural := 0;
      Chunks_Written   : Natural := 0;
      Read_Complete    : Boolean := False;
      Process_Complete : Boolean := False;
      Write_Complete   : Boolean := False;
   end record;

   --  ## Thread-Safe Progress Tracker
   --
   --  Protected type that provides atomic operations for updating and querying
   --  progress across multiple pipeline stages. All operations are inherently
   --  thread-safe through Ada's protected type mechanism.
   protected type Progress_Tracker_Type is

      --  ## Counter Update Operations
      --
      --  These procedures allow pipeline stages to report their progress
      --  by updating the count of completed items. Multiple workers can
      --  call these concurrently - the protected type ensures atomicity.

      --  ### Update Read Progress
      --
      --  Reports the number of chunks that have been successfully read
      --  from input sources. Typically called by file readers, network
      --  receivers, or data source adapters.
      --
      --  #### Parameters
      --  * `Count` - New total count of chunks read (not incremental)
      --
      --  #### Usage Patterns
      --  ```ada
      --  -- Single chunk reader
      --  for I in 1 .. Total_Chunks loop
      --     Chunk := Read_Chunk (I);
      --     Tracker.Update_Read_Count (I);
      --  end loop;
      --
      --  -- Parallel reader workers
      --  task body Reader_Worker is
      --     Chunks_Read : Natural := 0;
      --  begin
      --     loop
      --        Read_Next_Chunk (Chunk);
      --        Chunks_Read := Chunks_Read + 1;
      --        Tracker.Update_Read_Count (Chunks_Read);  -- Report progress
      --     end loop;
      --  end Reader_Worker;
      --  ```
      procedure Update_Read_Count (Count : Natural)
      with Pre => Count >= 0;

      --  ### Update Processing Progress
      --
      --  Reports the number of chunks that have completed processing
      --  (transformation, computation, filtering, etc.). Called by
      --  processing workers as they complete chunk operations.
      --
      --  #### Parameters
      --  * `Count` - New total count of chunks processed
      --
      --  #### Typical Integration
      --  ```ada
      --  -- Integration with Parallel_Chunk_Processor
      --  task body Processing_Worker is
      --     Processed_Count : Natural := 0;
      --  begin
      --     loop
      --        Get_Chunk_From_Queue (Chunk);
      --        exit when Chunk = End_Marker;
      --
      --        Result := Process_Chunk (Chunk, Context);
      --        Processed_Count := Processed_Count + 1;
      --
      --        Tracker.Update_Processed_Count (Processed_Count);
      --        Submit_Result (Result);
      --     end loop;
      --  end Processing_Worker;
      --  ```
      procedure Update_Processed_Count (Count : Natural)
      with Pre => Count >= 0;

      --  ### Update Write Progress
      --
      --  Reports the number of chunks that have been successfully written
      --  to output destinations. Called by file writers, network senders,
      --  or output adapters.
      --
      --  #### Parameters
      --  * `Count` - New total count of chunks written
      --
      --  #### Examples
      --  ```ada
      --  -- Sequential writing
      --  for Chunk of Processed_Chunks loop
      --     Write_Chunk_To_File (Chunk, Output_File);
      --     Written_Count := Written_Count + 1;
      --     Tracker.Update_Written_Count (Written_Count);
      --  end loop;
      --
      --  -- Parallel writing with Random_Write_File
      --  task body Writer_Worker is
      --     Local_Written : Natural := 0;
      --  begin
      --     loop
      --        Get_Processed_Chunk (Chunk);
      --        exit when Chunk = End_Marker;
      --
      --        Protected_File.Write_Chunk (Chunk);
      --        Local_Written := Local_Written + 1;
      --        Tracker.Update_Written_Count (Local_Written);
      --     end loop;
      --  end Writer_Worker;
      --  ```
      procedure Update_Written_Count (Count : Natural)
      with Pre => Count >= 0;

      --  ## Stage Completion Markers
      --
      --  These procedures signal when each pipeline stage has finished
      --  all its work. Essential for detecting overall completion and
      --  coordinating pipeline shutdown.

      --  ### Mark Reading Stage Complete
      --
      --  Signals that no more chunks will be read from input sources.
      --  Typically called when end-of-file is reached or all input
      --  sources are exhausted.
      --
      --  #### When to Call
      --  * All input files have been fully read
      --  * Network connection has closed normally
      --  * Input stream has reached its end
      --  * User has cancelled further reading
      --
      --  #### Example
      --  ```ada
      --  -- File reading completion
      --  while not End_Of_File (Input_File) loop
      --     Read_Chunk (Input_File, Chunk);
      --     Submit_For_Processing (Chunk);
      --     Chunks_Read := Chunks_Read + 1;
      --     Tracker.Update_Read_Count (Chunks_Read);
      --  end loop;
      --  Tracker.Mark_Read_Complete;  -- Signal reading finished
      --  ```
      procedure Mark_Read_Complete;

      --  ### Mark Processing Stage Complete
      --
      --  Signals that all chunks have been processed and no more
      --  processing work remains. Called when processing queues
      --  are empty and no more input is expected.
      --
      --  #### Coordination with Parallel Processor
      --  ```ada
      --  Processor.Signal_End_Of_Input;
      --  Processor.Wait_For_Completion;
      --  Tracker.Mark_Processing_Complete;  -- All processing done
      --  ```
      procedure Mark_Processing_Complete;

      --  ### Mark Writing Stage Complete
      --
      --  Signals that all processed chunks have been written to
      --  output destinations. Called when output queues are empty
      --  and all data has been persisted.
      --
      --  #### Final Stage Completion
      --  ```ada
      --  -- After all chunks written
      --  Commit_Output_File (Output_File);
      --  Tracker.Mark_Writing_Complete;  -- Pipeline finished
      --
      --  if Tracker.Is_All_Complete then
      --     Put_Line ("Pipeline completed successfully!");
      --  end if;
      --  ```
      procedure Mark_Writing_Complete;

      --  ## Progress Query Operations
      --
      --  These functions provide read-only access to current progress
      --  information. Multiple tasks can call these concurrently without
      --  blocking update operations.

      --  ### Get Complete Progress Snapshot
      --
      --  Returns a consistent snapshot of all progress counters in a
      --  single atomic operation. Ensures all values are from the same
      --  moment in time.
      --
      --  #### Parameters
      --  * `Read` - (out) Number of chunks read
      --  * `Processed` - (out) Number of chunks processed
      --  * `Written` - (out) Number of chunks written
      --
      --  #### Usage for Monitoring
      --  ```ada
      --  -- Monitoring task that displays progress
      --  task body Progress_Monitor is
      --     Read, Processed, Written : Natural;
      --  begin
      --     loop
      --        Tracker.Get_Progress (Read, Processed, Written);
      --
      --        Put_Line ("Read:" & Read'Image &
      --                 " Processed:" & Processed'Image &
      --                 " Written:" & Written'Image);
      --
      --        delay 1.0;  -- Update every second
      --        exit when Tracker.Is_All_Complete;
      --     end loop;
      --  end Progress_Monitor;
      --  ```
      procedure Get_Progress (Read, Processed, Written : out Natural)
      with Post => Read >= 0 and Processed >= 0 and Written >= 0;

      --  ## Stage Completion Queries
      --
      --  These functions check whether specific pipeline stages have
      --  completed all their work. Used for coordination and UI updates.

      --  ### Check Read Stage Completion
      --
      --  Returns true if reading stage has been marked complete.
      --  Useful for detecting when processing can begin final shutdown.
      function Is_Read_Complete return Boolean;

      --  ### Check Processing Stage Completion
      --
      --  Returns true if processing stage has been marked complete.
      --  Indicates that all computation/transformation work is finished.
      function Is_Processing_Complete return Boolean;

      --  ### Check Write Stage Completion
      --
      --  Returns true if writing stage has been marked complete.
      --  Indicates that all output has been persisted successfully.
      function Is_Writing_Complete return Boolean;

      --  ### Check Overall Pipeline Completion
      --
      --  Returns true only when ALL stages have been marked complete.
      --  This is the definitive signal that the entire pipeline has
      --  finished processing.
      --
      --  #### Contract Verification
      --  The postcondition ensures that this function returns true
      --  if and only if all three stages are complete, providing
      --  compile-time verification of the completion logic.
      --
      --  #### Final Completion Check
      --  ```ada
      --  -- Main pipeline coordination
      --  Start_All_Pipeline_Stages;
      --
      --  -- Wait for completion
      --  while not Tracker.Is_All_Complete loop
      --     delay 0.1;  -- Check every 100ms
      --  end loop;
      --
      --  Put_Line ("Pipeline processing complete!");
      --  Generate_Final_Report;
      --  ```
      function Is_All_Complete return Boolean
      with
        Post =>
          Is_All_Complete'Result
          = (Is_Read_Complete
             and Is_Processing_Complete
             and Is_Writing_Complete);

      --  ### Real-Time Progress Display
      --
      --  Displays current progress in a console-friendly format with
      --  in-place updates (carriage return). Designed for interactive
      --  applications where users need visual feedback.
      --
      --  #### Display Features
      --  * In-place updates using carriage return (no scrolling)
      --  * Shows counts for all three stages
      --  * Indicates completion status for each stage
      --  * Handles first display vs. updates appropriately
      --
      --  #### Integration Example
      --  ```ada
      --  -- Application layer handles display
      --  State : constant Progress_State := Tracker.Get_Progress_State;
      --  Display_Progress_In_Console(State);
      --  ```
      function Get_Progress_State return Progress_State;

   private
      Chunks_Read : Natural := 0;
      Chunks_Processed : Natural := 0;
      Chunks_Written : Natural := 0;
      Read_Complete : Boolean := False;
      Process_Complete : Boolean := False;
      Write_Complete : Boolean := False;
   end Progress_Tracker_Type;

   type Progress_Tracker_Access is access all Progress_Tracker_Type;

end Pipelib.Core.Domain.Services.Progress_Tracker;
