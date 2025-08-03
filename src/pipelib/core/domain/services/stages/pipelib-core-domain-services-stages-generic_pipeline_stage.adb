--   =============================================================================
--   Pipelib.Core.Domain.Services.Stages.Generic_Pipeline_Stage - Implementation
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;

with Ada.Calendar; use Ada.Calendar;
with Ada.Exceptions;

package body Pipelib.Core.Domain.Services.Stages.Generic_Pipeline_Stage is

   -- ----------
   --  Create
   -- ----------

   function Create (Config : Config_Type) return Pipeline_Stage is
   begin
      return Stage : Pipeline_Stage do
         Stage.Name := To_Unbounded_String (Stage_Name);
         Stage.Config := Config;
         Stage.State := Initialize_State (Config);
         Stage.Is_Initialized := True;
         Stage.Items_Processed := 0;
         Stage.Errors_Count := 0;
         Stage.Total_Time := 0.0;
      end return;
   end Create;

   -- -----------
   --  Process
   -- -----------

   function Process
     (Stage : in out Pipeline_Stage; Input : Input_Type)
      return Output_Result.Result
   is
      Start_Time : constant Time := Clock;
   begin
      begin
         declare
            Output : constant Output_Type :=
              Process_Element (Stage.State, Stage.Config, Input);
         begin
            Stage.Items_Processed := Stage.Items_Processed + 1;
            Stage.Total_Time := Stage.Total_Time + (Clock - Start_Time);
            return Output_Result.Ok (Output);
         end;
      exception
         when E : others =>
            Stage.Errors_Count := Stage.Errors_Count + 1;
            return
              Output_Result.Err
                (To_Unbounded_String (Ada.Exceptions.Exception_Message (E)));
      end;
   end Process;

   -- -----------------
   --  Process_Batch
   -- -----------------

   function Process_Batch
     (Stage : in out Pipeline_Stage; Inputs : Input_Array) return Output_Array
   is
      Results : Output_Array (Inputs'Range);
   begin
      for I in Inputs'Range loop
         declare
            Result : constant Output_Result.Result :=
              Process (Stage, Inputs (I));
         begin
            if Output_Result.Is_Ok (Result) then
               Results (I) := Output_Result.Get_Ok (Result);
            else
               --  For batch processing, we'll use a default value on error
               --  In a real implementation, you might want to handle this differently
               raise Program_Error
                 with
                   "Batch processing failed at index"
                   & I'Image
                   & ": "
                   & To_String (Output_Result.Get_Err (Result));
            end if;
         end;
      end loop;
      return Results;
   end Process_Batch;

   -- --------------------
   --  Process_Parallel
   -- --------------------

   function Process_Parallel
     (Stage        : in out Pipeline_Stage;
      Inputs       : Input_Array;
      Worker_Count : Positive := 4) return Output_Array
   is
      Results : Output_Array (Inputs'Range);
      pragma
        Unreferenced
          (Worker_Count); -- Will be used when parallel processing is implemented
   begin
      --  For now, use sequential processing
      --  (parallel syntax requires specific compiler support)
      for I in Inputs'Range loop
         declare
            Result : constant Output_Result.Result :=
              Process (Stage, Inputs (I));
         begin
            if Output_Result.Is_Ok (Result) then
               Results (I) := Output_Result.Get_Ok (Result);
            else
               raise Program_Error
                 with "Parallel processing failed at index" & I'Image;
            end if;
         end;
      end loop;

      return Results;
   end Process_Parallel;

   -- -------------------
   --  Is_Initialized
   -- -------------------

   function Is_Initialized (Stage : Pipeline_Stage) return Boolean is
   begin
      return Stage.Is_Initialized;
   end Is_Initialized;

   -- --------
   --  Name
   -- --------

   function Name (Stage : Pipeline_Stage) return String is
   begin
      return To_String (Stage.Name);
   end Name;

   -- --------------------
   --  Items_Processed
   -- --------------------

   function Items_Processed (Stage : Pipeline_Stage) return Natural is
   begin
      return Stage.Items_Processed;
   end Items_Processed;

   -- --------------
   --  Get_State
   -- --------------

   function Get_State (Stage : Pipeline_Stage) return State_Type is
   begin
      return Stage.State;
   end Get_State;

   -- --------------
   --  Get_Config
   -- --------------

   function Get_Config (Stage : Pipeline_Stage) return Config_Type is
   begin
      return Stage.Config;
   end Get_Config;

   -- ---------
   --  Reset
   -- ---------

   procedure Reset (Stage : in out Pipeline_Stage) is
   begin
      Stage.State := Initialize_State (Stage.Config);
      Stage.Items_Processed := 0;
      Stage.Errors_Count := 0;
      Stage.Total_Time := 0.0;
   end Reset;

   -- ----------------
   --  Update_State
   -- ----------------

   procedure Update_State
     (Stage : in out Pipeline_Stage; New_State : State_Type) is
   begin
      Stage.State := New_State;
   end Update_State;

   -- -------------------
   --  Get_Statistics
   -- -------------------

   function Get_Statistics (Stage : Pipeline_Stage) return Stage_Statistics is
      Success_Rate : Float := 0.0;
      Average_Time : Duration := 0.0;
   begin
      if Stage.Items_Processed > 0 then
         Success_Rate :=
           Float (Stage.Items_Processed - Stage.Errors_Count) * 100.0
           / Float (Stage.Items_Processed);
         Average_Time := Stage.Total_Time / Stage.Items_Processed;
      end if;

      return
        (Items_Processed => Stage.Items_Processed,
         Errors_Count    => Stage.Errors_Count,
         Success_Rate    => Success_Rate,
         Average_Time_Ms => Average_Time);
   end Get_Statistics;

   -- ---------
   --  Image
   -- ---------

   function Image (Stage : Pipeline_Stage) return String is
      Stats : constant Stage_Statistics := Get_Statistics (Stage);
   begin
      return
        Stage_Name
        & "[items="
        & Stage.Items_Processed'Image
        & ", errors="
        & Stage.Errors_Count'Image
        & ", success_rate="
        & Stats.Success_Rate'Image
        & "%]";
   end Image;

end Pipelib.Core.Domain.Services.Stages.Generic_Pipeline_Stage;
