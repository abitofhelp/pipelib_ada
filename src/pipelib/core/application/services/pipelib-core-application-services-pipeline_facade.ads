--  =============================================================================
--  Pipelib.Core.Application.Services.Pipeline_Facade - Clean Application Interface
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  ## Pipeline Facade Application Service
--
--  This service provides a clean, DTO-based interface to the pipeline
--  processing capabilities while maintaining proper Clean Architecture
--  boundaries. It serves as an example of how application services should
--  expose domain functionality through stable, client-friendly APIs.
--
--  ### Architecture Compliance
--
--  **Clean Boundaries**: This facade demonstrates proper Clean Architecture:
--  * Uses DTOs for all external communication (no domain object leakage)
--  * Translates between application DTOs and domain objects internally
--  * Provides stable interfaces that can evolve independently of domain changes
--  * Maintains single responsibility (orchestration, not business logic)
--
--  **Dependency Direction**: Follows proper dependency flow:
--  * Depends on domain layer for business logic
--  * Uses domain ports for infrastructure abstraction
--  * Exposes only application-layer concerns to clients
--  * No infrastructure dependencies (follows DIP)
--
--  **Interface Segregation**: Provides focused, client-specific interfaces:
--  * Simple configuration API for pipeline setup
--  * Clear processing API for chunk submission
--  * Separate monitoring API for status and statistics
--  * Distinct lifecycle management operations
--
--  ### Use Cases Implemented
--
--  **Pipeline Configuration**: Set up processing pipeline with validation
--  * Worker thread configuration
--  * Output file specification
--  * Performance tuning parameters
--  * Error handling configuration
--
--  **Chunk Processing**: Submit data for parallel processing
--  * Asynchronous chunk submission with immediate response
--  * Priority-based processing queues
--  * Progress monitoring and completion notification
--  * Error handling and recovery options
--
--  **Status Monitoring**: Monitor pipeline health and performance
--  * Real-time statistics and metrics
--  * Processing status and completion tracking
--  * Error reporting and diagnostics
--  * Performance profiling data
--
--  ### Error Handling
--
--  Application-level error handling with clear client feedback:
--  * Structured error responses with human-readable messages
--  * Error categorization (configuration, processing, system)
--  * Recovery suggestions where applicable
--  * No domain exceptions exposed to clients
--
--  ### Usage Example
--
--  ```ada
--  -- Configure pipeline
--  Config : constant Pipeline_Configuration := (
--     Worker_Count => 8,
--     Chunk_Size_Bytes => 65536,
--     Output_File_Path => To_Unbounded_String("/output/processed.dat"),
--     others => <>
--  );
--
--  Facade : Pipeline_Facade_Type;
--
--  -- Initialize pipeline
--  if Facade.Configure_Pipeline(Config).Success then
--     -- Submit chunks for processing
--     for Chunk_Data of Input_Chunks loop
--        Request : constant Process_Chunk_Request := Create_Process_Request(
--           Data_Address => Chunk_Data'Address,
--           Data_Size => Chunk_Data'Length,
--           File_Position => Calculate_Position(Chunk_Data)
--        );
--
--        Response : constant Process_Chunk_Response := Facade.Process_Chunk(Request);
--        if not Response.Success then
--           Handle_Processing_Error(Response.Error_Message);
--        end if;
--     end loop;
--
--     -- Monitor completion
--     Status : constant Pipeline_Status := Facade.Get_Pipeline_Status;
--     Wait_For_Completion(Status);
--  else
--     Handle_Configuration_Error;
--  end if;
--  ```
--
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded;
with Pipelib.Core.Application.DTOs;
with Pipelib.Core.Domain.Services.Progress_Tracker;

package Pipelib.Core.Application.Services.Pipeline_Facade is

   use Pipelib.Core.Application.DTOs;

   --  ## Pipeline Facade Service Type
   --
   --  Main application service that orchestrates pipeline operations using
   --  domain services while exposing only DTO-based interfaces to clients.
   type Pipeline_Facade_Type is tagged limited private;

   --  ## Configuration Response DTO
   --
   --  Response from pipeline configuration operations indicating success
   --  or failure with descriptive error information.
   type Configuration_Response is record
      Success       : Boolean;
      Error_Message : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --  ## Configure Pipeline
   --
   --  Initializes the processing pipeline with the specified configuration.
   --  Validates all parameters and sets up internal resources.
   --
   --  ### Parameters
   --  * `Facade` - The facade service instance
   --  * `Config` - Pipeline configuration DTO with all settings
   --
   --  ### Returns
   --  Configuration response indicating success or specific error details.
   --
   --  ### Validation
   --  * Worker count within supported range (1-64)
   --  * Valid output file path and permissions
   --  * Reasonable chunk size and queue depth settings
   --  * Compatible configuration parameter combinations
   --
   --  ### Error Conditions
   --  * Invalid worker count or configuration parameters
   --  * Inaccessible output file path or insufficient permissions
   --  * Resource allocation failures (memory, file handles)
   --  * Conflicting configuration settings
   function Configure_Pipeline
     (Facade : in out Pipeline_Facade_Type; Config : Pipeline_Configuration)
      return Configuration_Response;

   --  ## Process Chunk
   --
   --  Submits a data chunk for asynchronous processing by the pipeline.
   --  Returns immediately with status information.
   --
   --  ### Parameters
   --  * `Facade` - The facade service instance (must be configured)
   --  * `Request` - Chunk processing request DTO with data and parameters
   --
   --  ### Returns
   --  Processing response with success status and relevant details.
   --
   --  ### Behavior
   --  * Validates request parameters (data size, position, etc.)
   --  * Translates DTO to domain objects internally
   --  * Submits to domain processing pipeline
   --  * Returns immediate response (asynchronous processing)
   --
   --  ### Error Conditions
   --  * Pipeline not configured or in invalid state
   --  * Invalid request parameters (size, position, etc.)
   --  * Pipeline queue full (backpressure)
   --  * Memory allocation failures
   function Process_Chunk
     (Facade : in out Pipeline_Facade_Type; Request : Process_Chunk_Request)
      return Process_Chunk_Response;

   --  ## Get Pipeline Status
   --
   --  Retrieves current pipeline status and statistics for monitoring.
   --  Provides comprehensive view of pipeline health and performance.
   --
   --  ### Parameters
   --  * `Facade` - The facade service instance
   --
   --  ### Returns
   --  Complete pipeline status DTO with current state and statistics.
   --
   --  ### Status Information
   --  * Current processing status (running, completed, failed, etc.)
   --  * Performance statistics (throughput, completion counts)
   --  * Queue depth and worker utilization
   --  * Error counts and recent error messages
   --
   --  ### Thread Safety
   --  Safe to call from any thread - provides atomic snapshot of status.
   function Get_Pipeline_Status
     (Facade : Pipeline_Facade_Type) return Pipeline_Status;

   --  ## Wait for Completion
   --
   --  Blocks until all submitted chunks have been processed or an error occurs.
   --  Useful for synchronous processing scenarios.
   --
   --  ### Parameters
   --  * `Facade` - The facade service instance
   --  * `Timeout_Seconds` - Maximum wait time (0 = infinite)
   --
   --  ### Returns
   --  Final pipeline status after completion or timeout.
   --
   --  ### Behavior
   --  * Waits for all pending chunks to complete processing
   --  * Monitors for error conditions and early termination
   --  * Respects timeout and returns current status if exceeded
   --  * Thread-safe operation (can be called concurrently)
   function Wait_For_Completion
     (Facade : Pipeline_Facade_Type; Timeout_Seconds : Natural := 0)
      return Pipeline_Status;

   --  ## Shutdown Pipeline
   --
   --  Gracefully shuts down the pipeline and releases all resources.
   --  Completes pending work before termination.
   --
   --  ### Parameters
   --  * `Facade` - The facade service instance
   --  * `Force_Immediate` - If true, cancels pending work immediately
   --
   --  ### Returns
   --  Shutdown response indicating success and final statistics.
   --
   --  ### Behavior
   --  * Graceful shutdown: Completes pending chunks before stopping
   --  * Forced shutdown: Cancels pending work and terminates immediately
   --  * Releases all resources (file handles, worker threads, memory)
   --  * Commits any buffered output to final destination
   function Shutdown_Pipeline
     (Facade : in out Pipeline_Facade_Type; Force_Immediate : Boolean := False)
      return Configuration_Response;

   --  ## Get Processing Statistics
   --
   --  Retrieves detailed processing statistics for performance analysis.
   --  Separate from status to allow focused performance monitoring.
   --
   --  ### Parameters
   --  * `Facade` - The facade service instance
   --
   --  ### Returns
   --  Detailed statistics DTO with performance metrics.
   --
   --  ### Statistics Included
   --  * Throughput measurements (current and peak)
   --  * Processing time distributions
   --  * Worker utilization and efficiency metrics
   --  * Memory usage and queue depth statistics
   function Get_Statistics
     (Facade : Pipeline_Facade_Type) return Pipeline_Statistics;

private

   type Pipeline_Facade_Type is tagged limited record
      Is_Configured    : Boolean := False;
      Progress_Tracker :
        Pipelib.Core.Domain.Services.Progress_Tracker.Progress_Tracker_Access;
      -- Additional internal state would be defined here
      -- (e.g., processor instances, configuration, etc.)
   end record;

end Pipelib.Core.Application.Services.Pipeline_Facade;
