--  =============================================================================
--  Test_Generic_Pipeline_Stage - Generic Pipeline Stage Unit Tests Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Pipelib.Core.Domain.Services.Stages.Generic_Pipeline_Stage;

package body Test_Generic_Pipeline_Stage is

   --  Test types for pipeline stage instantiation
   type Test_Input_Type is new Integer;
   type Test_Output_Type is new Integer;
   type Test_State_Type is record
      Multiplier : Integer := 1;
      Is_Valid   : Boolean := True;
   end record;
   type Test_Config_Type is record
      Default_Multiplier : Integer := 2;
      Max_Value         : Integer := 1000;
   end record;

   --  Test processing function that doubles the input
   function Test_Process_Element (
      State  : in out Test_State_Type;
      Config : Test_Config_Type;
      Input  : Test_Input_Type) return Test_Output_Type
   is
      pragma Unreferenced (Config);
   begin
      return Test_Output_Type (Integer (Input) * State.Multiplier);
   end Test_Process_Element;

   --  Test state initialization
   function Test_Initialize_State (Config : Test_Config_Type) return Test_State_Type is
   begin
      return (Multiplier => Config.Default_Multiplier, Is_Valid => True);
   end Test_Initialize_State;

   --  Validation functions
   function Test_Is_Valid_Input (Input : Test_Input_Type) return Boolean is
   begin
      return Integer (Input) >= 0;
   end Test_Is_Valid_Input;

   function Test_Is_Valid_Output (Output : Test_Output_Type) return Boolean is
   begin
      return Integer (Output) >= 0;
   end Test_Is_Valid_Output;

   function Test_Is_Valid_State (State : Test_State_Type) return Boolean is
   begin
      return State.Is_Valid and State.Multiplier > 0;
   end Test_Is_Valid_State;

   --  Instantiate the pipeline stage for testing
   package Test_Pipeline is new Pipelib.Core.Domain.Services.Stages.Generic_Pipeline_Stage (
      Input_Type       => Test_Input_Type,
      Output_Type      => Test_Output_Type,
      State_Type       => Test_State_Type,
      Config_Type      => Test_Config_Type,
      Stage_Name       => "Test_Pipeline_Stage",
      Process_Element  => Test_Process_Element,
      Initialize_State => Test_Initialize_State,
      Is_Valid_Input   => Test_Is_Valid_Input,
      Is_Valid_Output  => Test_Is_Valid_Output,
      Is_Valid_State   => Test_Is_Valid_State
   );
   use Test_Pipeline;

   --  ==========================================================================
   --  Test Implementation
   --  ==========================================================================

   function Test_Create_Pipeline_Stage return Void_Result.Result is
      Config : constant Test_Config_Type := (Default_Multiplier => 3, Max_Value => 500);
      Stage : constant Pipeline_Stage := Create (Config);
   begin
      -- Test initial state after creation
      if not Is_Initialized (Stage) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Stage should be initialized"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_Pipeline_Stage")
         ));
      end if;

      if Name (Stage) /= "Test_Pipeline_Stage" then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Stage name incorrect"),
            Details     => To_Unbounded_String ("Expected: Test_Pipeline_Stage, Got: " & Name (Stage)),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_Pipeline_Stage")
         ));
      end if;

      if Items_Processed (Stage) /= 0 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Initial items processed should be 0"),
            Details     => To_Unbounded_String ("Got: " & Items_Processed (Stage)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_Pipeline_Stage")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Create_Pipeline_Stage;

   function Test_Process_Single_Item return Void_Result.Result is
      Config : constant Test_Config_Type := (Default_Multiplier => 2, Max_Value => 1000);
      Stage : Pipeline_Stage := Create (Config);
      Input : constant Test_Input_Type := 5;
   begin
      -- Process the input
      declare
         Result : constant Output_Result.Result := Process (Stage, Input);
      begin
         if not Result.Is_Ok then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Process failed"),
               Details     => Result.Get_Err,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Process_Single_Item")
            ));
         end if;

         -- Verify output (5 * 2 = 10)
         declare
            Output : constant Test_Output_Type := Result.Get_Ok;
         begin
            if Integer (Output) /= 10 then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Output incorrect"),
                  Details     => To_Unbounded_String ("Expected: 10, Got: " & Integer (Output)'Image),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Process_Single_Item")
               ));
            end if;
         end;
      end;

      -- Verify items processed count updated
      if Items_Processed (Stage) /= 1 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Items processed count incorrect"),
            Details     => To_Unbounded_String ("Expected: 1, Got: " & Items_Processed (Stage)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Process_Single_Item")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Process_Single_Item;

   function Test_Process_Multiple_Items return Void_Result.Result is
      Config : constant Test_Config_Type := (Default_Multiplier => 3, Max_Value => 1000);
      Stage : Pipeline_Stage := Create (Config);
   begin
      -- Process multiple items
      for I in 1 .. 5 loop
         declare
            Input : constant Test_Input_Type := Test_Input_Type (I);
            Result : constant Output_Result.Result := Process (Stage, Input);
         begin
            if not Result.Is_Ok then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Process failed for item " & I'Image),
                  Details     => Result.Get_Err,
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Process_Multiple_Items")
               ));
            end if;

            -- Verify output (I * 3)
            declare
               Output : constant Test_Output_Type := Result.Get_Ok;
               Expected : constant Integer := I * 3;
            begin
               if Integer (Output) /= Expected then
                  return Void_Result.Err (Test_Error'(
                     Kind        => Assertion_Failed,
                     Message     => To_Unbounded_String ("Output incorrect for item " & I'Image),
                     Details     => To_Unbounded_String ("Expected: " & Expected'Image &
                                                        ", Got: " & Integer (Output)'Image),
                     Line_Number => 0,
                     Test_Name   => To_Unbounded_String ("Test_Process_Multiple_Items")
                  ));
               end if;
            end;
         end;
      end loop;

      -- Verify final items processed count
      if Items_Processed (Stage) /= 5 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Final items processed count incorrect"),
            Details     => To_Unbounded_String ("Expected: 5, Got: " & Items_Processed (Stage)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Process_Multiple_Items")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Process_Multiple_Items;

   function Test_Process_Batch return Void_Result.Result is
      Config : constant Test_Config_Type := (Default_Multiplier => 4, Max_Value => 1000);
      Stage : Pipeline_Stage := Create (Config);
      Inputs : constant Input_Array (1 .. 3) := [Test_Input_Type (1), Test_Input_Type (2), Test_Input_Type (3)];
   begin
      -- Process batch
      declare
         Outputs : constant Output_Array := Process_Batch (Stage, Inputs);
      begin
         -- Verify array size
         if Outputs'Length /= Inputs'Length then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Output array size incorrect"),
               Details     => To_Unbounded_String ("Expected: " & Inputs'Length'Image &
                                                  ", Got: " & Outputs'Length'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Process_Batch")
            ));
         end if;

         -- Verify outputs
         for I in Outputs'Range loop
            declare
               Expected : constant Integer := Integer (Inputs (I)) * 4;
            begin
               if Integer (Outputs (I)) /= Expected then
                  return Void_Result.Err (Test_Error'(
                     Kind        => Assertion_Failed,
                     Message     => To_Unbounded_String ("Batch output incorrect at index " & I'Image),
                     Details     => To_Unbounded_String ("Expected: " & Expected'Image &
                                                        ", Got: " & Integer (Outputs (I))'Image),
                     Line_Number => 0,
                     Test_Name   => To_Unbounded_String ("Test_Process_Batch")
                  ));
               end if;
            end;
         end loop;
      end;

      return Void_Result.Ok (True);
   end Test_Process_Batch;

   function Test_Process_Parallel return Void_Result.Result is
      Config : constant Test_Config_Type := (Default_Multiplier => 2, Max_Value => 1000);
      Stage : Pipeline_Stage := Create (Config);
      Inputs : constant Input_Array (1 .. 4) :=
         [Test_Input_Type (10), Test_Input_Type (20), Test_Input_Type (30), Test_Input_Type (40)];
   begin
      -- Process parallel (currently sequential implementation)
      declare
         Outputs : constant Output_Array := Process_Parallel (Stage, Inputs, Worker_Count => 2);
      begin
         -- Verify outputs
         for I in Outputs'Range loop
            declare
               Expected : constant Integer := Integer (Inputs (I)) * 2;
            begin
               if Integer (Outputs (I)) /= Expected then
                  return Void_Result.Err (Test_Error'(
                     Kind        => Assertion_Failed,
                     Message     => To_Unbounded_String ("Parallel output incorrect at index " & I'Image),
                     Details     => To_Unbounded_String ("Expected: " & Expected'Image &
                                                        ", Got: " & Integer (Outputs (I))'Image),
                     Line_Number => 0,
                     Test_Name   => To_Unbounded_String ("Test_Process_Parallel")
                  ));
               end if;
            end;
         end loop;
      end;

      return Void_Result.Ok (True);
   end Test_Process_Parallel;

   function Test_Statistics_Tracking return Void_Result.Result is
      Config : constant Test_Config_Type := (Default_Multiplier => 1, Max_Value => 1000);
      Stage : Pipeline_Stage := Create (Config);
   begin
      -- Verify initial statistics
      declare
         Stats : constant Stage_Statistics := Get_Statistics (Stage);
      begin
         if Stats.Items_Processed /= 0 or Stats.Errors_Count /= 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Initial statistics should be zero"),
               Details     => To_Unbounded_String ("Items: " & Stats.Items_Processed'Image &
                                                  ", Errors: " & Stats.Errors_Count'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Statistics_Tracking")
            ));
         end if;
      end;

      -- Process some items successfully
      for I in 1 .. 3 loop
         declare
            Input : constant Test_Input_Type := Test_Input_Type (I);
            Result : constant Output_Result.Result := Process (Stage, Input);
         begin
            if not Result.Is_Ok then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Process should succeed"),
                  Details     => Result.Get_Err,
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Statistics_Tracking")
               ));
            end if;
         end;
      end loop;

      -- Verify updated statistics
      declare
         Stats : constant Stage_Statistics := Get_Statistics (Stage);
      begin
         if Stats.Items_Processed /= 3 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Items processed count incorrect"),
               Details     => To_Unbounded_String ("Expected: 3, Got: " & Stats.Items_Processed'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Statistics_Tracking")
            ));
         end if;

         if Stats.Success_Rate /= 100.0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Success rate should be 100%"),
               Details     => To_Unbounded_String ("Got: " & Stats.Success_Rate'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Statistics_Tracking")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Statistics_Tracking;

   function Test_Error_Handling return Void_Result.Result is
   begin
      -- Error handling structure exists, tested through statistics
      declare
         Config : constant Test_Config_Type := (Default_Multiplier => 1, Max_Value => 1000);
         Stage : Pipeline_Stage := Create (Config);
         Stats : constant Stage_Statistics := Get_Statistics (Stage);
      begin
         if Stats.Errors_Count /= 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Initial error count should be 0"),
               Details     => To_Unbounded_String ("Got: " & Stats.Errors_Count'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Error_Handling")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Error_Handling;

   function Test_State_Management return Void_Result.Result is
      Config : constant Test_Config_Type := (Default_Multiplier => 5, Max_Value => 1000);
      Stage : Pipeline_Stage := Create (Config);
   begin
      -- Test getting initial state
      declare
         Initial_State : constant Test_State_Type := Get_State (Stage);
      begin
         if Initial_State.Multiplier /= 5 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Initial state incorrect"),
               Details     => To_Unbounded_String ("Expected multiplier: 5, Got: " & Initial_State.Multiplier'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_State_Management")
            ));
         end if;
      end;

      -- Test updating state
      declare
         New_State : constant Test_State_Type := (Multiplier => 10, Is_Valid => True);
      begin
         Update_State (Stage, New_State);

         declare
            Updated_State : constant Test_State_Type := Get_State (Stage);
         begin
            if Updated_State.Multiplier /= 10 then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("State not updated correctly"),
                  Details     => To_Unbounded_String ("Expected multiplier: 10, Got: " & Updated_State.Multiplier'Image),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_State_Management")
               ));
            end if;
         end;
      end;

      return Void_Result.Ok (True);
   end Test_State_Management;

   function Test_Reset_Stage return Void_Result.Result is
      Config : constant Test_Config_Type := (Default_Multiplier => 7, Max_Value => 1000);
      Stage : Pipeline_Stage := Create (Config);
   begin
      -- Process some items first
      for I in 1 .. 3 loop
         declare
            Input : constant Test_Input_Type := Test_Input_Type (I);
            Result : constant Output_Result.Result := Process (Stage, Input);
         begin
            if not Result.Is_Ok then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Initial processing failed"),
                  Details     => Result.Get_Err,
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Reset_Stage")
               ));
            end if;
         end;
      end loop;

      -- Reset the stage
      Reset (Stage);

      -- Verify reset state
      if Items_Processed (Stage) /= 0 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Items processed not reset"),
            Details     => To_Unbounded_String ("Got: " & Items_Processed (Stage)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Reset_Stage")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Reset_Stage;

   function Test_Contract_Validation return Void_Result.Result is
      Config : constant Test_Config_Type := (Default_Multiplier => 2, Max_Value => 1000);
      Stage : Pipeline_Stage := Create (Config);
   begin
      -- Test preconditions
      if not Is_Initialized (Stage) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Precondition violated: stage should be initialized"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Contract_Validation")
         ));
      end if;

      -- Test processing with valid input
      declare
         Valid_Input : constant Test_Input_Type := 5; -- Valid (>= 0)
         Result : constant Output_Result.Result := Process (Stage, Valid_Input);
      begin
         if not Result.Is_Ok then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Processing valid input failed"),
               Details     => Result.Get_Err,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Contract_Validation")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Contract_Validation;

   function Test_Configuration_Management return Void_Result.Result is
      Config : constant Test_Config_Type := (Default_Multiplier => 6, Max_Value => 2000);
      Stage : constant Pipeline_Stage := Create (Config);
   begin
      -- Test getting configuration
      declare
         Retrieved_Config : constant Test_Config_Type := Get_Config (Stage);
      begin
         if Retrieved_Config.Default_Multiplier /= 6 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Configuration not stored correctly"),
               Details     => To_Unbounded_String ("Expected multiplier: 6, Got: " & Retrieved_Config.Default_Multiplier'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Configuration_Management")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Configuration_Management;

   function Test_Image_Representation return Void_Result.Result is
      Config : constant Test_Config_Type := (Default_Multiplier => 1, Max_Value => 1000);
      Stage : Pipeline_Stage := Create (Config);
   begin
      -- Test initial image
      declare
         Initial_Image : constant String := Image (Stage);
      begin
         if Initial_Image'Length = 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Initial image should not be empty"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Image_Representation")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Image_Representation;

   function Test_Validation_Functions return Void_Result.Result is
   begin
      -- Test input validation
      if not Test_Is_Valid_Input (Test_Input_Type (5)) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Valid input should pass validation"),
            Details     => To_Unbounded_String ("Input: 5"),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Validation_Functions")
         ));
      end if;

      if Test_Is_Valid_Input (Test_Input_Type (-1)) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Invalid input should fail validation"),
            Details     => To_Unbounded_String ("Input: -1"),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Validation_Functions")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Validation_Functions;

   --  ==========================================================================
   --  Test Suite Runner
   --  ==========================================================================

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Tests : Test_Results_Array (1 .. 13);
      Index : Positive := 1;

      procedure Add_Test_Result
        (Name : String;
         Test_Func : Test_Function_Access)
      is
         Result : constant Test_Result_Pkg.Result :=
            Run_Test (Name, Test_Func, Output);
      begin
         if Result.Is_Ok then
            Tests (Index) := Result.Get_Ok;
            Print_Test_Result (Tests (Index), Output);
            Index := Index + 1;
         else
            -- Handle test execution error
            declare
               Err : constant Test_Error := Result.Get_Err;
            begin
               Tests (Index) := Test_Result'(
                  Name           => To_Unbounded_String (Name),
                  Status         => Error,
                  Message        => Err.Message,
                  Elapsed_Time   => 0.0,
                  Line_Number    => Err.Line_Number,
                  Correlation_ID => To_Unbounded_String ("TEST-" & Name)
               );
               Print_Test_Result (Tests (Index), Output);
               Index := Index + 1;
            end;
         end if;
      end Add_Test_Result;
   begin
      Output.Write_Line ("=== Running Generic Pipeline Stage Unit Tests ===");
      Output.Write_Line ("");

      -- Run all tests
      Add_Test_Result ("Test_Create_Pipeline_Stage", Test_Create_Pipeline_Stage'Access);
      Add_Test_Result ("Test_Process_Single_Item", Test_Process_Single_Item'Access);
      Add_Test_Result ("Test_Process_Multiple_Items", Test_Process_Multiple_Items'Access);
      Add_Test_Result ("Test_Process_Batch", Test_Process_Batch'Access);
      Add_Test_Result ("Test_Process_Parallel", Test_Process_Parallel'Access);
      Add_Test_Result ("Test_Statistics_Tracking", Test_Statistics_Tracking'Access);
      Add_Test_Result ("Test_Error_Handling", Test_Error_Handling'Access);
      Add_Test_Result ("Test_State_Management", Test_State_Management'Access);
      Add_Test_Result ("Test_Reset_Stage", Test_Reset_Stage'Access);
      Add_Test_Result ("Test_Contract_Validation", Test_Contract_Validation'Access);
      Add_Test_Result ("Test_Configuration_Management", Test_Configuration_Management'Access);
      Add_Test_Result ("Test_Image_Representation", Test_Image_Representation'Access);
      Add_Test_Result ("Test_Validation_Functions", Test_Validation_Functions'Access);

      -- Generate summary
      declare
         Stats_Result : constant Test_Stats_Result.Result :=
            Run_Test_Suite ("Generic_Pipeline_Stage_Tests", Tests (1 .. Index - 1), Output);
      begin
         if Stats_Result.Is_Ok then
            declare
               Stats : constant Test_Statistics := Stats_Result.Get_Ok;
            begin
               Output.Write_Line ("");
               Print_Test_Summary ("Generic Pipeline Stage Unit Tests", Stats, Output);
               return Test_Stats_Result.Ok (Stats);
            end;
         else
            return Stats_Result;
         end if;
      end;
   end Run_All_Tests;

end Test_Generic_Pipeline_Stage;
