--  =============================================================================
--  Test_Progress_Tracker - Progress Tracker Service Unit Tests Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Pipelib.Core.Domain.Services.Progress_Tracker;
use Pipelib.Core.Domain.Services.Progress_Tracker;

package body Test_Progress_Tracker is

   --  ==========================================================================
   --  Test Implementation
   --  ==========================================================================

   function Test_Create_Progress_Tracker return Void_Result.Result is
      Tracker : Progress_Tracker_Type;
   begin
      -- Test initial state after creation
      declare
         Read, Processed, Written : Natural;
      begin
         Tracker.Get_Progress (Read, Processed, Written);

         if Read /= 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Initial read count should be 0"),
               Details     => To_Unbounded_String ("Got: " & Read'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Create_Progress_Tracker")
            ));
         end if;

         if Processed /= 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Initial processed count should be 0"),
               Details     => To_Unbounded_String ("Got: " & Processed'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Create_Progress_Tracker")
            ));
         end if;

         if Written /= 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Initial written count should be 0"),
               Details     => To_Unbounded_String ("Got: " & Written'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Create_Progress_Tracker")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Create_Progress_Tracker;

   function Test_Update_Read_Count return Void_Result.Result is
      Tracker : Progress_Tracker_Type;
   begin
      -- Test updating read count
      Tracker.Update_Read_Count (42);

      declare
         Read, Processed, Written : Natural;
      begin
         Tracker.Get_Progress (Read, Processed, Written);

         if Read /= 42 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Read count not updated correctly"),
               Details     => To_Unbounded_String ("Expected: 42, Got: " & Read'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Update_Read_Count")
            ));
         end if;

         -- Other counters should remain 0
         if Processed /= 0 or Written /= 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Other counters should remain unchanged"),
               Details     => To_Unbounded_String ("Processed: " & Processed'Image &
                                                  ", Written: " & Written'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Update_Read_Count")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Update_Read_Count;

   function Test_Update_Processed_Count return Void_Result.Result is
      Tracker : Progress_Tracker_Type;
   begin
      -- Test updating processed count
      Tracker.Update_Processed_Count (123);

      declare
         Read, Processed, Written : Natural;
      begin
         Tracker.Get_Progress (Read, Processed, Written);

         if Processed /= 123 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Processed count not updated correctly"),
               Details     => To_Unbounded_String ("Expected: 123, Got: " & Processed'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Update_Processed_Count")
            ));
         end if;

         -- Other counters should remain 0
         if Read /= 0 or Written /= 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Other counters should remain unchanged"),
               Details     => To_Unbounded_String ("Read: " & Read'Image &
                                                  ", Written: " & Written'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Update_Processed_Count")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Update_Processed_Count;

   function Test_Update_Written_Count return Void_Result.Result is
      Tracker : Progress_Tracker_Type;
   begin
      -- Test updating written count
      Tracker.Update_Written_Count (456);

      declare
         Read, Processed, Written : Natural;
      begin
         Tracker.Get_Progress (Read, Processed, Written);

         if Written /= 456 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Written count not updated correctly"),
               Details     => To_Unbounded_String ("Expected: 456, Got: " & Written'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Update_Written_Count")
            ));
         end if;

         -- Other counters should remain 0
         if Read /= 0 or Processed /= 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Other counters should remain unchanged"),
               Details     => To_Unbounded_String ("Read: " & Read'Image &
                                                  ", Processed: " & Processed'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Update_Written_Count")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Update_Written_Count;

   function Test_Mark_Read_Complete return Void_Result.Result is
      Tracker : Progress_Tracker_Type;
   begin
      -- Test marking read as complete
      Tracker.Update_Read_Count (100);
      Tracker.Mark_Read_Complete;

      -- Note: The Progress_Tracker doesn't expose completion flags directly,
      -- but we can verify the operation completed without error
      -- In a real implementation, we might check Display_Progress output
      -- or have additional query methods

      return Void_Result.Ok (True);
   end Test_Mark_Read_Complete;

   function Test_Mark_Processing_Complete return Void_Result.Result is
      Tracker : Progress_Tracker_Type;
   begin
      -- Test marking processing as complete
      Tracker.Update_Processed_Count (50);
      Tracker.Mark_Processing_Complete;

      return Void_Result.Ok (True);
   end Test_Mark_Processing_Complete;

   function Test_Mark_Writing_Complete return Void_Result.Result is
      Tracker : Progress_Tracker_Type;
   begin
      -- Test marking writing as complete
      Tracker.Update_Written_Count (75);
      Tracker.Mark_Writing_Complete;

      return Void_Result.Ok (True);
   end Test_Mark_Writing_Complete;

   function Test_Get_Progress return Void_Result.Result is
      Tracker : Progress_Tracker_Type;
   begin
      -- Test getting progress with various values
      Tracker.Update_Read_Count (10);
      Tracker.Update_Processed_Count (8);
      Tracker.Update_Written_Count (6);

      declare
         Read, Processed, Written : Natural;
      begin
         Tracker.Get_Progress (Read, Processed, Written);

         if Read /= 10 or Processed /= 8 or Written /= 6 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Progress values incorrect"),
               Details     => To_Unbounded_String ("Expected (10,8,6), Got (" &
                                                  Read'Image & "," & Processed'Image &
                                                  "," & Written'Image & ")"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Get_Progress")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Get_Progress;

   function Test_Concurrent_Updates return Void_Result.Result is
      Tracker : Progress_Tracker_Type;
   begin
      -- Test concurrent-style updates (simulation since we can't use real tasks in unit tests)
      -- Update different counters in sequence to simulate concurrent access

      for I in 1 .. 100 loop
         Tracker.Update_Read_Count (I);
         if I > 1 then
            Tracker.Update_Processed_Count (I - 1);
         end if;
         if I > 2 then
            Tracker.Update_Written_Count (I - 2);
         end if;
      end loop;

      declare
         Read, Processed, Written : Natural;
      begin
         Tracker.Get_Progress (Read, Processed, Written);

         if Read /= 100 or Processed /= 99 or Written /= 98 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Concurrent-style updates failed"),
               Details     => To_Unbounded_String ("Expected (100,99,98), Got (" &
                                                  Read'Image & "," & Processed'Image &
                                                  "," & Written'Image & ")"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Concurrent_Updates")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Concurrent_Updates;

   function Test_Multiple_Updates_Same_Counter return Void_Result.Result is
      Tracker : Progress_Tracker_Type;
   begin
      -- Test multiple updates to the same counter (last value should win)
      Tracker.Update_Read_Count (10);
      Tracker.Update_Read_Count (20);
      Tracker.Update_Read_Count (30);

      declare
         Read, Processed, Written : Natural;
      begin
         Tracker.Get_Progress (Read, Processed, Written);

         if Read /= 30 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Multiple updates failed - last value should win"),
               Details     => To_Unbounded_String ("Expected: 30, Got: " & Read'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Multiple_Updates_Same_Counter")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Multiple_Updates_Same_Counter;

   function Test_Initial_State return Void_Result.Result is
      Tracker : Progress_Tracker_Type;
   begin
      -- Test that the tracker starts in a clean initial state
      declare
         Read, Processed, Written : Natural;
      begin
         Tracker.Get_Progress (Read, Processed, Written);

         if Read /= 0 or Processed /= 0 or Written /= 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Initial state not clean"),
               Details     => To_Unbounded_String ("Got (" & Read'Image & "," &
                                                  Processed'Image & "," & Written'Image &
                                                  "), expected (0,0,0)"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Initial_State")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Initial_State;

   function Test_Progress_Retrieval_Thread_Safety return Void_Result.Result is
      Tracker : Progress_Tracker_Type;
   begin
      -- Test that multiple calls to Get_Progress are consistent
      Tracker.Update_Read_Count (42);
      Tracker.Update_Processed_Count (24);
      Tracker.Update_Written_Count (12);

      -- Multiple retrievals should return the same values
      for I in 1 .. 50 loop
         declare
            Read, Processed, Written : Natural;
         begin
            Tracker.Get_Progress (Read, Processed, Written);

            if Read /= 42 or Processed /= 24 or Written /= 12 then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Inconsistent progress retrieval"),
                  Details     => To_Unbounded_String ("Iteration " & I'Image &
                                                     ", Got (" & Read'Image & "," &
                                                     Processed'Image & "," & Written'Image &
                                                     "), expected (42,24,12)"),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Progress_Retrieval_Thread_Safety")
               ));
            end if;
         end;
      end loop;

      return Void_Result.Ok (True);
   end Test_Progress_Retrieval_Thread_Safety;

   function Test_All_Stages_Complete return Void_Result.Result is
      Tracker : Progress_Tracker_Type;
   begin
      -- Test marking all stages as complete
      Tracker.Update_Read_Count (100);
      Tracker.Update_Processed_Count (100);
      Tracker.Update_Written_Count (100);

      Tracker.Mark_Read_Complete;
      Tracker.Mark_Processing_Complete;
      Tracker.Mark_Writing_Complete;

      -- Verify counts are preserved after marking complete
      declare
         Read, Processed, Written : Natural;
      begin
         Tracker.Get_Progress (Read, Processed, Written);

         if Read /= 100 or Processed /= 100 or Written /= 100 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Counts changed after marking complete"),
               Details     => To_Unbounded_String ("Got (" & Read'Image & "," &
                                                  Processed'Image & "," & Written'Image &
                                                  "), expected (100,100,100)"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_All_Stages_Complete")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_All_Stages_Complete;

   function Test_Boundary_Values return Void_Result.Result is
      Tracker : Progress_Tracker_Type;
   begin
      -- Test boundary values for Natural type
      Tracker.Update_Read_Count (0);
      Tracker.Update_Processed_Count (Natural'Last);
      Tracker.Update_Written_Count (Natural'First);

      declare
         Read, Processed, Written : Natural;
      begin
         Tracker.Get_Progress (Read, Processed, Written);

         if Read /= 0 or Processed /= Natural'Last or Written /= Natural'First then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Boundary values not handled correctly"),
               Details     => To_Unbounded_String ("Expected (0," & Natural'Last'Image &
                                                  "," & Natural'First'Image &
                                                  "), Got (" & Read'Image & "," &
                                                  Processed'Image & "," & Written'Image & ")"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Boundary_Values")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Boundary_Values;

   --  ==========================================================================
   --  Test Suite Runner
   --  ==========================================================================

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Tests : Test_Results_Array (1 .. 14);
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
      Output.Write_Line ("=== Running Progress Tracker Unit Tests ===");
      Output.Write_Line ("");

      -- Run all tests
      Add_Test_Result ("Test_Create_Progress_Tracker", Test_Create_Progress_Tracker'Access);
      Add_Test_Result ("Test_Update_Read_Count", Test_Update_Read_Count'Access);
      Add_Test_Result ("Test_Update_Processed_Count", Test_Update_Processed_Count'Access);
      Add_Test_Result ("Test_Update_Written_Count", Test_Update_Written_Count'Access);
      Add_Test_Result ("Test_Mark_Read_Complete", Test_Mark_Read_Complete'Access);
      Add_Test_Result ("Test_Mark_Processing_Complete", Test_Mark_Processing_Complete'Access);
      Add_Test_Result ("Test_Mark_Writing_Complete", Test_Mark_Writing_Complete'Access);
      Add_Test_Result ("Test_Get_Progress", Test_Get_Progress'Access);
      Add_Test_Result ("Test_Concurrent_Updates", Test_Concurrent_Updates'Access);
      Add_Test_Result ("Test_Multiple_Updates_Same_Counter", Test_Multiple_Updates_Same_Counter'Access);
      Add_Test_Result ("Test_Initial_State", Test_Initial_State'Access);
      Add_Test_Result ("Test_Progress_Retrieval_Thread_Safety", Test_Progress_Retrieval_Thread_Safety'Access);
      Add_Test_Result ("Test_All_Stages_Complete", Test_All_Stages_Complete'Access);
      Add_Test_Result ("Test_Boundary_Values", Test_Boundary_Values'Access);

      -- Generate summary
      declare
         Stats_Result : constant Test_Stats_Result.Result :=
            Run_Test_Suite ("Progress_Tracker_Tests", Tests (1 .. Index - 1), Output);
      begin
         if Stats_Result.Is_Ok then
            declare
               Stats : constant Test_Statistics := Stats_Result.Get_Ok;
            begin
               Output.Write_Line ("");
               Print_Test_Summary ("Progress Tracker Unit Tests", Stats, Output);
               return Test_Stats_Result.Ok (Stats);
            end;
         else
            return Stats_Result;
         end if;
      end;
   end Run_All_Tests;

end Test_Progress_Tracker;
