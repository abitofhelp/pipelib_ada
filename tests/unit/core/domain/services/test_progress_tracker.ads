--  =============================================================================
--  Test_Progress_Tracker - Progress Tracker Service Unit Tests Specification
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Comprehensive unit tests for the Progress Tracker service using the abohlib
--    testing framework with thread-safety validation and contract verification.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Progress_Tracker is

   use Abohlib.Infrastructure.Testing.Test_Framework;

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   with Post => Run_All_Tests'Result.Is_Ok or Run_All_Tests'Result.Is_Err;
   --  Execute all Progress Tracker tests and return statistics

private

   --  Test functions with contracts
   function Test_Create_Progress_Tracker return Void_Result.Result
   with Post => Test_Create_Progress_Tracker'Result.Is_Ok or
                Test_Create_Progress_Tracker'Result.Is_Err;

   function Test_Update_Read_Count return Void_Result.Result
   with Post => Test_Update_Read_Count'Result.Is_Ok or
                Test_Update_Read_Count'Result.Is_Err;

   function Test_Update_Processed_Count return Void_Result.Result
   with Post => Test_Update_Processed_Count'Result.Is_Ok or
                Test_Update_Processed_Count'Result.Is_Err;

   function Test_Update_Written_Count return Void_Result.Result
   with Post => Test_Update_Written_Count'Result.Is_Ok or
                Test_Update_Written_Count'Result.Is_Err;

   function Test_Mark_Read_Complete return Void_Result.Result
   with Post => Test_Mark_Read_Complete'Result.Is_Ok or
                Test_Mark_Read_Complete'Result.Is_Err;

   function Test_Mark_Processing_Complete return Void_Result.Result
   with Post => Test_Mark_Processing_Complete'Result.Is_Ok or
                Test_Mark_Processing_Complete'Result.Is_Err;

   function Test_Mark_Writing_Complete return Void_Result.Result
   with Post => Test_Mark_Writing_Complete'Result.Is_Ok or
                Test_Mark_Writing_Complete'Result.Is_Err;

   function Test_Get_Progress return Void_Result.Result
   with Post => Test_Get_Progress'Result.Is_Ok or
                Test_Get_Progress'Result.Is_Err;

   function Test_Concurrent_Updates return Void_Result.Result
   with Post => Test_Concurrent_Updates'Result.Is_Ok or
                Test_Concurrent_Updates'Result.Is_Err;

   function Test_Multiple_Updates_Same_Counter return Void_Result.Result
   with Post => Test_Multiple_Updates_Same_Counter'Result.Is_Ok or
                Test_Multiple_Updates_Same_Counter'Result.Is_Err;

   function Test_Initial_State return Void_Result.Result
   with Post => Test_Initial_State'Result.Is_Ok or
                Test_Initial_State'Result.Is_Err;

   function Test_Progress_Retrieval_Thread_Safety return Void_Result.Result
   with Post => Test_Progress_Retrieval_Thread_Safety'Result.Is_Ok or
                Test_Progress_Retrieval_Thread_Safety'Result.Is_Err;

   function Test_All_Stages_Complete return Void_Result.Result
   with Post => Test_All_Stages_Complete'Result.Is_Ok or
                Test_All_Stages_Complete'Result.Is_Err;

   function Test_Boundary_Values return Void_Result.Result
   with Post => Test_Boundary_Values'Result.Is_Ok or
                Test_Boundary_Values'Result.Is_Err;

end Test_Progress_Tracker;
