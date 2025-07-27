--   =============================================================================
--   Pipelib.Core.Domain.Services.Stages.Generic_Pipeline_Stage - Generic Pipeline Stage Framework
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Generic framework for creating type-safe pipeline stages with comprehensive
--   contracts and Ada 2022 features.
--   =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Core.Domain.Result;

generic
   type Input_Type is private;
   type Output_Type is private;
   type State_Type is private;
   type Config_Type is private;

   --  Stage identification
   Stage_Name : String := "Generic Stage";

   --  Processing function provided by instantiation
   with function Process_Element (
      State  : in out State_Type;
      Config : Config_Type;
      Input  : Input_Type) return Output_Type;

   --  State initialization
   with function Initialize_State (Config : Config_Type) return State_Type;

   --  Validation functions
   with function Is_Valid_Input (Input : Input_Type) return Boolean is <>;
   with function Is_Valid_Output (Output : Output_Type) return Boolean is <>;
   with function Is_Valid_State (State : State_Type) return Boolean is <>;

package Pipelib.Core.Domain.Services.Stages.Generic_Pipeline_Stage is
   pragma Elaborate_Body;

   --  Result types for stage operations
   package Output_Result is new Abohlib.Core.Domain.Result.Result_Package
      (Ok_Type => Output_Type,
       Err_Type => Unbounded_String);

   package Status_Result is new Abohlib.Core.Domain.Result.Result_Package
      (Ok_Type => Boolean,
       Err_Type => Unbounded_String);

   --  Generic pipeline stage type
   type Pipeline_Stage is tagged private;

   --  Constructor with contracts
   function Create (Config : Config_Type) return Pipeline_Stage
   with Post => Create'Result.Is_Initialized and then
                Create'Result.Name = Stage_Name and then
                Create'Result.Items_Processed = 0;

   --  Process a single item with comprehensive contracts
   function Process (
      Stage : in out Pipeline_Stage;
      Input : Input_Type) return Output_Result.Result
   with Pre => Stage.Is_Initialized and then
               Is_Valid_Input (Input) and then
               Is_Valid_State (Stage.Get_State),
        Post => (if Output_Result.Is_Ok (Process'Result) then
                   Is_Valid_Output (Output_Result.Get_Ok (Process'Result)) and then
                   Stage.Items_Processed = Stage'Old.Items_Processed + 1
                 else
                   Stage.Items_Processed = Stage'Old.Items_Processed);

   --  Batch processing using Ada 2022 features
   type Input_Array is array (Positive range <>) of Input_Type;
   type Output_Array is array (Positive range <>) of Output_Type;

   function Process_Batch (
      Stage : in out Pipeline_Stage;
      Inputs : Input_Array) return Output_Array
   with Pre => Stage.Is_Initialized and then
               Inputs'Length > 0 and then
               (for all I in Inputs'Range => Is_Valid_Input (Inputs(I))),
        Post => Process_Batch'Result'Length = Inputs'Length and then
                (for all I in Process_Batch'Result'Range =>
                   Is_Valid_Output (Process_Batch'Result(I))) and then
                Stage.Items_Processed = Stage'Old.Items_Processed + Inputs'Length;

   --  Parallel processing with Ada 2022 parallel blocks
   function Process_Parallel (
      Stage : in out Pipeline_Stage;
      Inputs : Input_Array;
      Worker_Count : Positive := 4) return Output_Array
   with Pre => Stage.Is_Initialized and then
               Inputs'Length > 0 and then
               Worker_Count > 0 and then
               (for all I in Inputs'Range => Is_Valid_Input (Inputs(I))),
        Post => Process_Parallel'Result'Length = Inputs'Length;

   --  Query methods with contracts
   function Is_Initialized (Stage : Pipeline_Stage) return Boolean
   with Inline;

   function Name (Stage : Pipeline_Stage) return String
   with Post => Name'Result = Stage_Name;

   function Items_Processed (Stage : Pipeline_Stage) return Natural
   with Post => Items_Processed'Result >= 0;

   function Get_State (Stage : Pipeline_Stage) return State_Type
   with Pre => Stage.Is_Initialized;

   function Get_Config (Stage : Pipeline_Stage) return Config_Type
   with Pre => Stage.Is_Initialized;

   --  State management with contracts
   procedure Reset (Stage : in out Pipeline_Stage)
   with Pre => Stage.Is_Initialized,
        Post => Stage.Is_Initialized and then
                Stage.Items_Processed = 0;

   procedure Update_State (
      Stage : in out Pipeline_Stage;
      New_State : State_Type)
   with Pre => Stage.Is_Initialized and then
               Is_Valid_State (New_State),
        Post => Stage.Get_State = New_State;

   --  Statistics and monitoring
   type Stage_Statistics is record
      Items_Processed : Natural;
      Errors_Count    : Natural;
      Success_Rate    : Float range 0.0 .. 100.0;
      Average_Time_Ms : Duration;
   end record;

   function Get_Statistics (Stage : Pipeline_Stage) return Stage_Statistics
   with Pre => Stage.Is_Initialized;

   --  String representation
   function Image (Stage : Pipeline_Stage) return String;

private
   type Pipeline_Stage is tagged record
      Name            : Unbounded_String;
      Config          : Config_Type;
      State           : State_Type;
      Is_Initialized  : Boolean := False;
      Items_Processed : Natural := 0;
      Errors_Count    : Natural := 0;
      Total_Time      : Duration := 0.0;
   end record;

end Pipelib.Core.Domain.Services.Stages.Generic_Pipeline_Stage;
