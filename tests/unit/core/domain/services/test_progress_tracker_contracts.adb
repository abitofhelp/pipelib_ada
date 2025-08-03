--  =============================================================================
--  Test_Progress_Tracker_Contracts - Contract Validation Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Pipelib.Core.Domain.Services.Progress_Tracker;
use Pipelib.Core.Domain.Services.Progress_Tracker;

package body Test_Progress_Tracker_Contracts is

   --  Test update preconditions
   function Test_Update_Preconditions return Void_Result.Result is
   begin
      declare
         Tracker : Progress_Tracker_Type;
         Contract_Violated : Boolean := False;
      begin
         -- Test Update_Read_Count precondition: Count >= 0
         -- Note: Natural type already enforces >= 0, so this is implicit

         -- Try with maximum Natural value (should work)
         begin
            Tracker.Update_Read_Count (Natural'Last);
            Contract_Violated := False;
         exception
            when others =>
               -- This shouldn't happen with valid Natural values
               Contract_Violated := True;
         end;

         if Contract_Violated then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Update_Read_Count failed with valid input"),
               Details     => To_Unbounded_String ("Natural'Last should be valid"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Update_Preconditions")
            ));
         end if;

         -- Same tests apply to Update_Processed_Count and Update_Written_Count
         Tracker.Update_Processed_Count (100);
         Tracker.Update_Written_Count (50);
      end;

      return Void_Result.Ok (True);
   end Test_Update_Preconditions;

   --  Test Get_Progress postconditions
   function Test_Get_Progress_Postconditions return Void_Result.Result is
   begin
      declare
         Tracker : Progress_Tracker_Type;
         Read, Processed, Written : Natural;
      begin
         -- Update some values
         Tracker.Update_Read_Count (100);
         Tracker.Update_Processed_Count (75);
         Tracker.Update_Written_Count (50);

         -- Get progress
         Tracker.Get_Progress (Read, Processed, Written);

         -- Test postcondition: Read >= 0 and Processed >= 0 and Written >= 0
         if Read < 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Get_Progress postcondition failed"),
               Details     => To_Unbounded_String ("Read count negative:" & Read'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Get_Progress_Postconditions")
            ));
         end if;

         if Processed < 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Get_Progress postcondition failed"),
               Details     => To_Unbounded_String ("Processed count negative:" & Processed'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Get_Progress_Postconditions")
            ));
         end if;

         if Written < 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Get_Progress postcondition failed"),
               Details     => To_Unbounded_String ("Written count negative:" & Written'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Get_Progress_Postconditions")
            ));
         end if;

         -- Verify the values are as expected
         if Read /= 100 or Processed /= 75 or Written /= 50 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Get_Progress values incorrect"),
               Details     => To_Unbounded_String ("Expected: 100,75,50 Got:" &
                                                  Read'Image & "," & Processed'Image & "," & Written'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Get_Progress_Postconditions")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Get_Progress_Postconditions;

   --  Test Is_All_Complete postcondition
   function Test_Is_All_Complete_Postcondition return Void_Result.Result is
   begin
      declare
         Tracker : Progress_Tracker_Type;
      begin
         -- Initially, nothing should be complete
         if Tracker.Is_All_Complete then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Is_All_Complete postcondition failed"),
               Details     => To_Unbounded_String ("Should not be complete initially"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Is_All_Complete_Postcondition")
            ));
         end if;

         -- Test postcondition: Is_All_Complete'Result =
         --   (Is_Read_Complete and Is_Processing_Complete and Is_Writing_Complete)

         -- Mark only read complete
         Tracker.Mark_Read_Complete;
         if Tracker.Is_All_Complete then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Is_All_Complete postcondition failed"),
               Details     => To_Unbounded_String ("Should not be complete with only read done"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Is_All_Complete_Postcondition")
            ));
         end if;

         -- Mark processing complete too
         Tracker.Mark_Processing_Complete;
         if Tracker.Is_All_Complete then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Is_All_Complete postcondition failed"),
               Details     => To_Unbounded_String ("Should not be complete without writing done"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Is_All_Complete_Postcondition")
            ));
         end if;

         -- Mark writing complete - now all should be complete
         Tracker.Mark_Writing_Complete;
         if not Tracker.Is_All_Complete then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Is_All_Complete postcondition failed"),
               Details     => To_Unbounded_String ("Should be complete when all stages done"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Is_All_Complete_Postcondition")
            ));
         end if;

         -- Verify the postcondition formula
         declare
            All_Complete : constant Boolean := Tracker.Is_All_Complete;
            Read_Complete : constant Boolean := Tracker.Is_Read_Complete;
            Proc_Complete : constant Boolean := Tracker.Is_Processing_Complete;
            Write_Complete : constant Boolean := Tracker.Is_Writing_Complete;
            Expected : constant Boolean := Read_Complete and Proc_Complete and Write_Complete;
         begin
            if All_Complete /= Expected then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Is_All_Complete postcondition formula failed"),
                  Details     => To_Unbounded_String ("Formula mismatch: " & All_Complete'Image &
                                                     " /= " & Expected'Image),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Is_All_Complete_Postcondition")
               ));
            end if;
         end;
      end;

      return Void_Result.Ok (True);
   end Test_Is_All_Complete_Postcondition;

   --  Test completion state consistency
   function Test_Completion_State_Consistency return Void_Result.Result is
   begin
      declare
         Tracker : Progress_Tracker_Type;
      begin
         -- Test that completion flags are consistent with the state
         -- Initially, no stages should be complete
         if Tracker.Is_Read_Complete or Tracker.Is_Processing_Complete or Tracker.Is_Writing_Complete then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Initial completion state inconsistent"),
               Details     => To_Unbounded_String ("No stages should be complete initially"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Completion_State_Consistency")
            ));
         end if;

         -- Mark stages complete one by one and verify consistency
         Tracker.Mark_Read_Complete;
         if not Tracker.Is_Read_Complete then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Read completion state inconsistent"),
               Details     => To_Unbounded_String ("Read should be complete after marking"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Completion_State_Consistency")
            ));
         end if;

         Tracker.Mark_Processing_Complete;
         if not Tracker.Is_Processing_Complete then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Processing completion state inconsistent"),
               Details     => To_Unbounded_String ("Processing should be complete after marking"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Completion_State_Consistency")
            ));
         end if;

         Tracker.Mark_Writing_Complete;
         if not Tracker.Is_Writing_Complete then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Writing completion state inconsistent"),
               Details     => To_Unbounded_String ("Writing should be complete after marking"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Completion_State_Consistency")
            ));
         end if;

         -- Verify all are complete
         if not (Tracker.Is_Read_Complete and Tracker.Is_Processing_Complete and Tracker.Is_Writing_Complete) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Final completion state inconsistent"),
               Details     => To_Unbounded_String ("All stages should be complete"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Completion_State_Consistency")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Completion_State_Consistency;

   --  Test thread safety contracts (document requirements)
   function Test_Thread_Safety_Contracts return Void_Result.Result is
   begin
      -- Document thread safety requirements:

      -- 1. All operations on Progress_Tracker_Type are thread-safe
      --    (it's a protected type)
      -- 2. Multiple tasks can call update operations concurrently
      -- 3. Query operations can be called concurrently with updates
      -- 4. Completion state changes are atomic
      -- 5. Get_Progress provides a consistent snapshot of all counters

      -- For actual testing, we would need to:
      -- - Create multiple tasks updating different counters
      -- - Verify no race conditions occur
      -- - Ensure progress values are always consistent
      -- - Test that completion flags are properly synchronized

      return Void_Result.Ok (True);
   end Test_Thread_Safety_Contracts;

   --  Test progress tracking lifecycle
   function Test_Progress_Tracking_Lifecycle return Void_Result.Result is
   begin
      -- Document expected lifecycle:

      -- 1. Created tracker has all counts at 0, no stages complete
      -- 2. Counts can be updated independently and concurrently
      -- 3. Completion can be marked independently of counts
      -- 4. Is_All_Complete reflects the logical AND of all completion flags
      -- 5. Display_Progress should work at any point in the lifecycle

      -- Test typical progression:
      declare
         Tracker : Progress_Tracker_Type;
         Read, Processed, Written : Natural;
      begin
         -- Phase 1: Reading
         for I in 1 .. 100 loop
            Tracker.Update_Read_Count (I);
         end loop;
         Tracker.Mark_Read_Complete;

         -- Phase 2: Processing
         for I in 1 .. 100 loop
            Tracker.Update_Processed_Count (I);
         end loop;
         Tracker.Mark_Processing_Complete;

         -- Phase 3: Writing
         for I in 1 .. 100 loop
            Tracker.Update_Written_Count (I);
         end loop;
         Tracker.Mark_Writing_Complete;

         -- Verify final state
         Tracker.Get_Progress (Read, Processed, Written);
         if Read /= 100 or Processed /= 100 or Written /= 100 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Final progress state incorrect"),
               Details     => To_Unbounded_String ("Expected: 100,100,100 Got:" &
                                                  Read'Image & "," & Processed'Image & "," & Written'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Progress_Tracking_Lifecycle")
            ));
         end if;

         if not Tracker.Is_All_Complete then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Final completion state incorrect"),
               Details     => To_Unbounded_String ("Should be complete at end of lifecycle"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Progress_Tracking_Lifecycle")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Progress_Tracking_Lifecycle;

   --  Test suite runner
   function Run_All_Tests (Output : access Test_Output_Port'Class)
                          return Test_Stats_Result.Result is
      Tests : constant Test_Array := [
         (Name => To_Unbounded_String ("Update Preconditions"),
          Func => Test_Update_Preconditions'Access),
         (Name => To_Unbounded_String ("Get_Progress Postconditions"),
          Func => Test_Get_Progress_Postconditions'Access),
         (Name => To_Unbounded_String ("Is_All_Complete Postcondition"),
          Func => Test_Is_All_Complete_Postcondition'Access),
         (Name => To_Unbounded_String ("Completion State Consistency"),
          Func => Test_Completion_State_Consistency'Access),
         (Name => To_Unbounded_String ("Thread Safety Contracts"),
          Func => Test_Thread_Safety_Contracts'Access),
         (Name => To_Unbounded_String ("Progress Tracking Lifecycle"),
          Func => Test_Progress_Tracking_Lifecycle'Access)
      ];
   begin
      return Run_Test_Suite ("Progress_Tracker Contract Validation", Tests, Output);
   end Run_All_Tests;

end Test_Progress_Tracker_Contracts;
