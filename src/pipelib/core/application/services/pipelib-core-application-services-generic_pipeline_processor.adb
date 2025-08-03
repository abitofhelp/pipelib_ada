--   =============================================================================
--   Pipelib.Core.Application.Services.Generic_Pipeline_Processor - Implementation
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;

with Ada.Calendar; use Ada.Calendar;
with Ada.Exceptions;

package body Pipelib.Core.Application.Services.Generic_Pipeline_Processor is

   -- ----------
   --  Create
   -- ----------

   function Create (Config : Config_Type) return Pipeline_Processor is
   begin
      return Processor : Pipeline_Processor do
         Processor.Config := Config;
         Processor.Is_Processing := False;
         Processor.Is_Initialized := False;
         Processor.Total_Processed := 0;
         Processor.Total_Errors := 0;
         Processor.Total_Time := 0.0;
         Processor.Error_Handler := null;
      end return;
   end Create;

   -- --------------
   --  Add_Stage
   -- --------------

   procedure Add_Stage
     (Processor : in out Pipeline_Processor;
      Stage     : access Stage_Interface'Class;
      Name      : String) is
   begin
      Processor.Stages.Append
        (Stage_Registration'
           (Stage => Stage, Name => To_Unbounded_String (Name)));
   end Add_Stage;

   -- -----------------
   --  Remove_Stage
   -- -----------------

   procedure Remove_Stage
     (Processor : in out Pipeline_Processor; Name : String)
   is
      Name_UB : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      for I in Processor.Stages.First_Index .. Processor.Stages.Last_Index loop
         if Processor.Stages (I).Name = Name_UB then
            Processor.Stages.Delete (I);
            return;
         end if;
      end loop;
   end Remove_Stage;

   -- --------------
   --  Has_Stage
   -- --------------

   function Has_Stage
     (Processor : Pipeline_Processor; Name : String) return Boolean
   is
      Name_UB : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      for Stage_Reg of Processor.Stages loop
         if Stage_Reg.Name = Name_UB then
            return True;
         end if;
      end loop;
      return False;
   end Has_Stage;

   -- ----------------
   --  Stage_Count
   -- ----------------

   function Stage_Count (Processor : Pipeline_Processor) return Natural is
   begin
      return Natural (Processor.Stages.Length);
   end Stage_Count;

   -- -----------
   --  Process
   -- -----------

   function Process
     (Processor : in out Pipeline_Processor; Input : Input_Type)
      return Output_Result.Result
   is
      Start_Time   : constant Time := Clock;
      Current_Data : Stage_Data_Type := Input_To_Stage_Data (Input);
   begin
      Processor.Is_Processing := True;

      begin
         -- Process through each stage
         for Stage_Reg of Processor.Stages loop
            declare
               Result : constant Status_Result.Result :=
                 Stage_Reg.Stage.Process_Item (Current_Data);
            begin
               if not Status_Result.Is_Ok (Result) then
                  Processor.Total_Errors := Processor.Total_Errors + 1;
                  Processor.Is_Processing := False;

                  if Processor.Error_Handler /= null then
                     Processor.Error_Handler (Status_Result.Get_Err (Result));
                  end if;

                  return Output_Result.Err (Status_Result.Get_Err (Result));
               end if;
            end;
         end loop;

         -- Convert the processed data to output type
         declare
            Output : constant Output_Type :=
              Stage_Data_To_Output (Current_Data);
         begin
            -- Validate output
            if not Is_Valid_Output (Output) then
               Processor.Total_Errors := Processor.Total_Errors + 1;
               Processor.Is_Processing := False;
               return
                 Output_Result.Err
                   (To_Unbounded_String ("Invalid output produced"));
            end if;

            Processor.Total_Processed := Processor.Total_Processed + 1;
            Processor.Total_Time :=
              Processor.Total_Time + (Clock - Start_Time);
            Processor.Is_Processing := False;
            return Output_Result.Ok (Output);
         end;

      exception
         when E : others =>
            Processor.Total_Errors := Processor.Total_Errors + 1;
            Processor.Is_Processing := False;

            declare
               Error_Msg : constant Unbounded_String :=
                 To_Unbounded_String (Ada.Exceptions.Exception_Message (E));
            begin
               if Processor.Error_Handler /= null then
                  Processor.Error_Handler (Error_Msg);
               end if;
               return Output_Result.Err (Error_Msg);
            end;
      end;
   end Process;

   -- -----------------
   --  Process_Batch
   -- -----------------

   function Process_Batch
     (Processor : in out Pipeline_Processor; Inputs : Input_Array)
      return Output_Array
   is
      Results : Output_Array (Inputs'Range);
   begin
      for I in Inputs'Range loop
         declare
            Result : constant Output_Result.Result :=
              Process (Processor, Inputs (I));
         begin
            if Output_Result.Is_Ok (Result) then
               Results (I) := Output_Result.Get_Ok (Result);
            else
               -- For batch processing, raise error
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

   -- -------------------------
   --  Process_Batch_Parallel
   -- -------------------------

   function Process_Batch_Parallel
     (Processor    : in out Pipeline_Processor;
      Inputs       : Input_Array;
      Worker_Count : Positive := 4) return Output_Array
   is
      pragma Unreferenced (Worker_Count);
      Results : Output_Array (Inputs'Range);
   begin
      -- For now, use sequential processing
      -- Parallel implementation would require task-safe stages
      for I in Inputs'Range loop
         declare
            Result : constant Output_Result.Result :=
              Process (Processor, Inputs (I));
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
   end Process_Batch_Parallel;

   -- ------------------------
   --  Initialize_Pipeline
   -- ------------------------

   procedure Initialize_Pipeline (Processor : in out Pipeline_Processor) is
   begin
      Processor.Is_Initialized := True;
      -- Initialize each stage if needed
   end Initialize_Pipeline;

   -- ----------------------
   --  Finalize_Pipeline
   -- ----------------------

   procedure Finalize_Pipeline (Processor : in out Pipeline_Processor) is
   begin
      Processor.Is_Initialized := False;
      -- Finalize each stage if needed
   end Finalize_Pipeline;

   -- ------------------
   --  Is_Processing
   -- ------------------

   function Is_Processing (Processor : Pipeline_Processor) return Boolean is
   begin
      return Processor.Is_Processing;
   end Is_Processing;

   -- -------------------
   --  Is_Initialized
   -- -------------------

   function Is_Initialized (Processor : Pipeline_Processor) return Boolean is
   begin
      return Processor.Is_Initialized;
   end Is_Initialized;

   -- ---------------
   --  Get_Config
   -- ---------------

   function Get_Config (Processor : Pipeline_Processor) return Config_Type is
   begin
      return Processor.Config;
   end Get_Config;

   -- -------------------
   --  Get_Statistics
   -- -------------------

   function Get_Statistics
     (Processor : Pipeline_Processor) return Pipeline_Statistics
   is
      Success_Rate : Float := 0.0;
      Average_Time : Duration := 0.0;
   begin
      if Processor.Total_Processed > 0 then
         Success_Rate :=
           Float (Processor.Total_Processed - Processor.Total_Errors) * 100.0
           / Float (Processor.Total_Processed);
         Average_Time := Processor.Total_Time / Processor.Total_Processed;
      end if;

      return
        (Total_Processed  => Processor.Total_Processed,
         Total_Errors     => Processor.Total_Errors,
         Success_Rate     => Success_Rate,
         Average_Time_Ms  => Average_Time,
         Stage_Statistics =>
           To_Unbounded_String ("{}"));  -- Empty JSON for now
   end Get_Statistics;

   -- ----------------------
   --  Set_Error_Handler
   -- ----------------------

   procedure Set_Error_Handler
     (Processor : in out Pipeline_Processor; Handler : Error_Handler_Access) is
   begin
      Processor.Error_Handler := Handler;
   end Set_Error_Handler;

   -- ---------
   --  Image
   -- ---------

   function Image (Processor : Pipeline_Processor) return String is
      Stats : constant Pipeline_Statistics := Get_Statistics (Processor);
   begin
      return
        "Pipeline[stages="
        & Stage_Count (Processor)'Image
        & ", processed="
        & Stats.Total_Processed'Image
        & ", errors="
        & Stats.Total_Errors'Image
        & ", success_rate="
        & Stats.Success_Rate'Image
        & "%]";
   end Image;

end Pipelib.Core.Application.Services.Generic_Pipeline_Processor;
