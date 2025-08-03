--   =============================================================================
--   Pipelib.Core.Domain.Services.Stages.Stage_Interface - Pipeline Stage Interface
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Common interface for all pipeline stages, defining the contract that
--   all stages must implement for consistent pipeline processing.
--   =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Core.Domain.Result;

generic
   type Input_Type is private;
   type Output_Type is private;
package Pipelib.Core.Domain.Services.Stages.Stage_Interface is

   --  Result types for stage operations
   package Process_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Output_Type,
        Err_Type => Unbounded_String);

   package Status_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Boolean,
        Err_Type => Unbounded_String);

   --  Stage interface type
   type Stage_Interface is interface;

   --  Core operations that all stages must implement
   function Process
     (Stage : in out Stage_Interface; Input : Input_Type)
      return Process_Result.Result
   is abstract;
   --  Process input and produce output

   function Initialize
     (Stage : in out Stage_Interface) return Status_Result.Result
   is abstract;
   --  Initialize the stage before processing

   function Cleanup
     (Stage : in out Stage_Interface) return Status_Result.Result
   is abstract;
   --  Clean up resources after processing

   --  Query operations
   function Name (Stage : Stage_Interface) return String is abstract;
   --  Return the stage name for identification

   function Is_Ready (Stage : Stage_Interface) return Boolean is abstract;
   --  Check if the stage is ready to process

   function Items_Processed (Stage : Stage_Interface) return Natural
   is abstract;
   --  Return the number of items processed

   function Bytes_Processed (Stage : Stage_Interface) return Long_Long_Integer
   is abstract;
   --  Return the number of bytes processed

   --  Optional operations (implementations can provide their own behavior)
   function Can_Process_In_Parallel (Stage : Stage_Interface) return Boolean
   is abstract;
   --  Indicates if this stage can process multiple items in parallel

   function Estimated_Throughput_MB_Per_Sec
     (Stage : Stage_Interface) return Float
   is abstract;
   --  Estimated processing throughput

end Pipelib.Core.Domain.Services.Stages.Stage_Interface;
