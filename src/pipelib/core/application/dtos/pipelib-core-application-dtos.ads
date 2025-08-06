--  =============================================================================
--  Pipelib.Core.Application.DTOs - Application Data Transfer Objects
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  ## Application Layer Data Transfer Objects
--
--  This package defines Data Transfer Objects (DTOs) that serve as the
--  application layer's public interface, preventing domain objects from
--  leaking through architectural boundaries.
--
--  ### Clean Architecture Compliance
--
--  **Boundary Protection**: DTOs act as a protective layer that:
--  * Prevents direct coupling between external clients and domain objects
--  * Allows domain objects to evolve without breaking application interfaces
--  * Provides stable contracts for external system integration
--  * Enables independent evolution of domain and application concerns
--
--  **Dependency Direction**: DTOs depend only on standard types and avoid
--  domain object dependencies, maintaining proper architectural flow.
--
--  **Interface Stability**: Application services use these DTOs for all
--  external communication, providing stable APIs regardless of internal changes.
--
--  ### DTO Design Principles
--
--  **Simple Data Containers**: DTOs contain only data and basic validation:
--  * No business logic or behavior
--  * Simple field validation only
--  * Immutable once constructed where possible
--  * Clear, self-documenting field names
--
--  **Translation Focus**: DTOs facilitate translation between layers:
--  * External formats â Application DTOs â Domain Objects
--  * Network protocols â Application DTOs â Domain Objects
--  * Storage formats â Application DTOs â Domain Objects
--
--  **Performance Considerations**: DTOs are designed for efficiency:
--  * Minimal memory footprint
--  * Fast construction and copying
--  * No expensive operations in accessors
--  * Suitable for high-frequency operations
--
--  ### Usage Patterns
--
--  **Request DTOs**: For incoming data to application services
--  ```ada
--  -- Client sends processing request
--  Request : constant Process_Chunk_Request := (
--     Data_Size => 1024,
--     Position  => 4096,
--     Priority  => Normal
--  );
--
--  -- Application service translates to domain objects
--  Domain_Chunk := Convert_To_Domain_Chunk(Request);
--  ```
--
--  **Response DTOs**: For outgoing data from application services
--  ```ada
--  -- Application service translates from domain objects
--  Response : constant Process_Chunk_Response := (
--     Success      => Result.Is_Ok,
--     Chunks_Processed => Statistics.Total_Processed,
--     Error_Message    => (if Result.Is_Ok then "" else Result.Error)
--  );
--  ```
--
--  ### Error Handling
--
--  DTOs use simple, application-appropriate error handling:
--  * Basic validation for field constraints
--  * String-based error messages for client consumption
--  * No domain-specific error types exposed
--  * Clear success/failure indicators
--
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded;
with System.Storage_Elements;
with Pipelib.Core.Domain.Constants;
with Abohlib.Core.Domain.Types.Bytes;
with Abohlib.Core.Domain.Types.Counts;
with Abohlib.Core.Domain.Types.Time;

package Pipelib.Core.Application.DTOs is

   use Ada.Strings.Unbounded;
   use System.Storage_Elements;
   use Pipelib.Core.Domain.Constants;
   use Abohlib.Core.Domain.Types.Bytes;

   --  ## Processing Priority Levels
   --
   --  Simple enumeration for chunk processing priority without exposing
   --  domain-specific priority algorithms or complex scheduling logic.
   type Processing_Priority is (Low, Normal, High, Critical);

   --  ## Chunk Processing Request DTO
   --
   --  Request data for submitting chunks to the parallel processing pipeline.
   --  Contains only the essential information needed from external clients.
   --
   --  ### Field Descriptions
   --  * `Data_Address` - Memory address of chunk data (zero-copy operation)
   --  * `Data_Size` - Size of data in bytes (must be > 0)
   --  * `File_Position` - Target position in output file (0-based offset)
   --  * `Sequence_Number` - Optional sequence for ordering (0 = auto-assign)
   --  * `Priority` - Processing priority level
   --  * `Is_Final_Chunk` - True if this is the last chunk in the stream
   type Process_Chunk_Request is record
      Data_Address    : System.Address;
      Data_Size       : Abohlib.Core.Domain.Types.Bytes.Buffer_Size_Type;
      File_Position   : File_Position_Type;
      Sequence_Number : Sequence_Number_Type := 0;
      Priority        : Processing_Priority := Normal;
      Is_Final_Chunk  : Boolean := False;
   end record;

   --  ## Chunk Processing Response DTO
   --
   --  Response data returned after chunk processing completion or failure.
   --  Provides essential feedback without exposing internal domain state.
   --
   --  ### Field Descriptions
   --  * `Success` - True if processing completed successfully
   --  * `Actual_Sequence` - The sequence number assigned to the chunk
   --  * `Bytes_Processed` - Number of bytes actually processed
   --  * `Processing_Time_Ms` - Processing duration in milliseconds
   --  * `Error_Message` - Human-readable error description (if not successful)
   type Process_Chunk_Response is record
      Success            : Boolean;
      Actual_Sequence    : Sequence_Number_Type;
      Bytes_Processed    : Abohlib.Core.Domain.Types.Bytes.SI_Bytes_Type;
      Processing_Time_Ms : Processing_Time_Ms_Type;
      Error_Message      : Unbounded_String;
   end record;

   --  ## Pipeline Statistics DTO
   --
   --  Statistical information about pipeline processing performance.
   --  Suitable for monitoring, logging, and client reporting.
   --
   --  ### Field Descriptions
   --  * `Total_Chunks_Submitted` - Total chunks submitted for processing
   --  * `Total_Chunks_Completed` - Total chunks successfully processed
   --  * `Total_Chunks_Failed` - Total chunks that failed processing
   --  * `Total_Bytes_Processed` - Total bytes successfully processed
   --  * `Average_Processing_Time_Ms` - Average processing time per chunk
   --  * `Peak_Throughput_MBps` - Peak throughput in megabytes per second
   --  * `Active_Workers` - Number of currently active worker threads
   --  * `Queue_Depth` - Number of chunks waiting for processing
   type Pipeline_Statistics is record
      Total_Chunks_Submitted     : Chunk_Count_Type := 0;
      Total_Chunks_Completed     : Chunk_Count_Type := 0;
      Total_Chunks_Failed        : Chunk_Count_Type := 0;
      Total_Bytes_Processed      : Abohlib.Core.Domain.Types.Bytes.SI_Bytes_Type := 0;
      Average_Processing_Time_Ms : Processing_Time_Ms_Type := 0;
      Peak_Throughput_MBps       : Throughput_MBps_Type := 0.0;
      Active_Workers             : Abohlib.Core.Domain.Types.Counts.Element_Count_Type := 0;
      Queue_Depth                : Abohlib.Core.Domain.Types.Counts.Element_Count_Type := 0;
   end record;

   --  ## Pipeline Configuration DTO
   --
   --  Configuration parameters for pipeline setup and initialization.
   --  Encapsulates complex domain configuration in simple application terms.
   --
   --  ### Field Descriptions
   --  * `Worker_Count` - Number of parallel worker threads (1-64)
   --  * `Chunk_Size_Bytes` - Default chunk size in bytes
   --  * `Max_Queue_Depth` - Maximum chunks queued before backpressure
   --  * `Enable_Statistics` - Whether to collect detailed statistics
   --  * `Output_File_Path` - Path to output file
   --  * `Use_Temporary_File` - Whether to use temporary file for atomic writes
   type Pipeline_Configuration is record
      Worker_Count       : Abohlib.Core.Domain.Types.Counts.Worker_Count_Type
                            range 1 .. 64 := 4;
      Chunk_Size_Bytes   : Abohlib.Core.Domain.Types.Bytes.Buffer_Size_Type :=
         Abohlib.Core.Domain.Types.Bytes.Buffer_Size_Type
            (Pipelib.Core.Domain.Constants.Default_Chunk_Size);
      Max_Queue_Depth    : Abohlib.Core.Domain.Types.Counts.Element_Count_Type :=
         Abohlib.Core.Domain.Types.Counts.Element_Count_Type
            (Pipelib.Core.Domain.Constants.Default_Max_Queue_Depth);
      Enable_Statistics  : Boolean := True;
      Output_File_Path   : Unbounded_String;
      Use_Temporary_File : Boolean := True;
   end record;

   --  ## Processing Status DTO
   --
   --  Current status of the processing pipeline for monitoring and control.
   --  Provides high-level status without exposing internal state machines.
   type Processing_Status is
     (Not_Started,        -- Pipeline not yet initialized
      Starting,           -- Pipeline initializing resources
      Running,            -- Pipeline actively processing chunks
      Completing,         -- Pipeline finishing remaining work
      Completed,          -- Pipeline successfully completed
      Failed,             -- Pipeline failed with error
      Cancelled           -- Pipeline cancelled by request
     );

   --  ## Pipeline Status DTO
   --
   --  Complete status information for the processing pipeline.
   --  Combines processing status with current statistics for monitoring.
   type Pipeline_Status is record
      Current_Status : Processing_Status := Not_Started;
      Statistics     : Pipeline_Statistics;
      Status_Message : Unbounded_String;
      Last_Updated   : Abohlib.Core.Domain.Types.Time.Timestamp_Type := 0;
   end record;

   --  ## DTO Construction and Validation
   --
   --  Utility functions for creating and validating DTOs with proper
   --  error handling and constraint checking.

   --  Create a processing request with validation
   function Create_Process_Request
     (Data_Address    : System.Address;
      Data_Size       : Storage_Count;
      File_Position   : File_Position_Type;
      Sequence_Number : Sequence_Number_Type := 0;
      Priority        : Processing_Priority := Normal;
      Is_Final        : Boolean := False) return Process_Chunk_Request
   with
     Pre => Data_Size > 0 and File_Position >= 0,
     Post =>
       Create_Process_Request'Result.Data_Size = Buffer_Size_Type (Data_Size)
       and Create_Process_Request'Result.File_Position = File_Position;

   --  Create a success response
   function Create_Success_Response
     (Sequence        : Sequence_Number_Type;
      Bytes_Processed : Storage_Count;
      Processing_Time : Processing_Time_Ms_Type) return Process_Chunk_Response
   with Post => Create_Success_Response'Result.Success = True;

   --  Create an error response
   function Create_Error_Response
     (Error_Message : String) return Process_Chunk_Response
   with Post => Create_Error_Response'Result.Success = False;

   --  Validate pipeline configuration
   function Is_Valid_Configuration
     (Config : Pipeline_Configuration) return Boolean;

   --  Get human-readable status description
   function Status_Description (Status : Processing_Status) return String;

end Pipelib.Core.Application.DTOs;
