--   =============================================================================
--   Pipelib.Core.Application.Services.Generic_Pipeline_Processor - Generic Pipeline Framework
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Generic pipeline processor that can compose multiple stages with type-safe
--   data flow and comprehensive error handling using Ada 2022 features.
--   =============================================================================

pragma Ada_2022;

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Core.Domain.Result;

generic
   --  Initial input type
   type Input_Type is private;

   --  Final output type
   type Output_Type is private;

   --  Pipeline configuration
   type Config_Type is private;

   --  Intermediate data type used between stages
   type Stage_Data_Type is private;

   --  Conversion functions
   with function Input_To_Stage_Data (Input : Input_Type) return Stage_Data_Type;
   with function Stage_Data_To_Output (Data : Stage_Data_Type) return Output_Type;

   --  Validation functions
   with function Is_Valid_Input (Input : Input_Type) return Boolean is <>;
   with function Is_Valid_Output (Output : Output_Type) return Boolean is <>;

package Pipelib.Core.Application.Services.Generic_Pipeline_Processor is
   pragma Elaborate_Body;

   --  Result types
   package Output_Result is new Abohlib.Core.Domain.Result.Result_Package
      (Ok_Type => Output_Type,
       Err_Type => Unbounded_String);

   package Status_Result is new Abohlib.Core.Domain.Result.Result_Package
      (Ok_Type => Boolean,
       Err_Type => Unbounded_String);

   --  Stage interface that stages must implement
   type Stage_Interface is interface;

   function Process_Item (
      Stage : Stage_Interface;
      Item  : in out Stage_Data_Type) return Status_Result.Result is abstract;

   --  Stage registration record
   type Stage_Registration is record
      Stage : access Stage_Interface'Class;
      Name  : Unbounded_String;
   end record;

   --  Error handler type
   type Error_Handler_Access is access procedure (Error : Unbounded_String);

   --  Pipeline processor type
   type Pipeline_Processor is tagged private;

   --  Constructor with contracts
   function Create (Config : Config_Type) return Pipeline_Processor
   with Post => Stage_Count (Create'Result) = 0 and then
                not Is_Processing (Create'Result);

   --  Stage management with contracts
   procedure Add_Stage (
      Processor : in out Pipeline_Processor;
      Stage     : access Stage_Interface'Class;
      Name      : String)
   with Pre => not Is_Processing (Processor) and then
               Stage /= null and then
               Name'Length > 0,
        Post => Stage_Count (Processor) = Stage_Count (Processor'Old) + 1;

   procedure Remove_Stage (
      Processor : in out Pipeline_Processor;
      Name      : String)
   with Pre => not Is_Processing (Processor) and then
               Has_Stage (Processor, Name),
        Post => Stage_Count (Processor) = Stage_Count (Processor'Old) - 1;

   function Has_Stage (
      Processor : Pipeline_Processor;
      Name      : String) return Boolean;

   function Stage_Count (Processor : Pipeline_Processor) return Natural
   with Post => Stage_Count'Result >= 0;

   --  Process single item through pipeline
   function Process (
      Processor : in out Pipeline_Processor;
      Input     : Input_Type) return Output_Result.Result
   with Pre => Stage_Count (Processor) > 0 and then
               not Is_Processing (Processor) and then
               Is_Valid_Input (Input),
        Post => not Is_Processing (Processor) and then
                (if Output_Result.Is_Ok (Process'Result) then
                   Is_Valid_Output (Output_Result.Get_Ok (Process'Result)));

   --  Process batch with Ada 2022 parallel features
   type Input_Array is array (Positive range <>) of Input_Type;
   type Output_Array is array (Positive range <>) of Output_Type;

   function Process_Batch (
      Processor : in out Pipeline_Processor;
      Inputs    : Input_Array) return Output_Array
   with Pre => Stage_Count (Processor) > 0 and then
               not Is_Processing (Processor) and then
               Inputs'Length > 0 and then
               (for all I in Inputs'Range => Is_Valid_Input (Inputs(I))),
        Post => Process_Batch'Result'Length = Inputs'Length and then
                not Is_Processing (Processor);

   --  Parallel batch processing
   function Process_Batch_Parallel (
      Processor    : in out Pipeline_Processor;
      Inputs       : Input_Array;
      Worker_Count : Positive := 4) return Output_Array
   with Pre => Stage_Count (Processor) > 0 and then
               not Is_Processing (Processor) and then
               Inputs'Length > 0 and then
               Worker_Count > 0 and then
               (for all I in Inputs'Range => Is_Valid_Input (Inputs(I))),
        Post => Process_Batch_Parallel'Result'Length = Inputs'Length;

   --  Pipeline control
   procedure Initialize_Pipeline (Processor : in out Pipeline_Processor)
   with Pre => Stage_Count (Processor) > 0 and then
               not Is_Processing (Processor),
        Post => Is_Initialized (Processor);

   procedure Finalize_Pipeline (Processor : in out Pipeline_Processor)
   with Pre => Is_Initialized (Processor) and then
               not Is_Processing (Processor),
        Post => not Is_Initialized (Processor);

   --  Query methods
   function Is_Processing (Processor : Pipeline_Processor) return Boolean
   with Inline;

   function Is_Initialized (Processor : Pipeline_Processor) return Boolean
   with Inline;

   function Get_Config (Processor : Pipeline_Processor) return Config_Type;

   --  Statistics
   type Pipeline_Statistics is record
      Total_Processed   : Natural;
      Total_Errors      : Natural;
      Success_Rate      : Float range 0.0 .. 100.0;
      Average_Time_Ms   : Duration;
      Stage_Statistics  : Unbounded_String;  -- JSON format
   end record;

   function Get_Statistics (Processor : Pipeline_Processor) return Pipeline_Statistics
   with Pre => Is_Initialized (Processor);

   --  Error recovery with Ada 2022 features
   procedure Set_Error_Handler (
      Processor : in out Pipeline_Processor;
      Handler   : Error_Handler_Access)
   with Pre => not Is_Processing (Processor);

   --  String representation
   function Image (Processor : Pipeline_Processor) return String;

private
   package Stage_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Stage_Registration,
       "="          => "=");

   type Pipeline_Processor is tagged record
      Config           : Config_Type;
      Stages           : Stage_Vectors.Vector;
      Is_Processing    : Boolean := False;
      Is_Initialized   : Boolean := False;
      Total_Processed  : Natural := 0;
      Total_Errors     : Natural := 0;
      Total_Time       : Duration := 0.0;
      Error_Handler    : Error_Handler_Access;
   end record;

end Pipelib.Core.Application.Services.Generic_Pipeline_Processor;
