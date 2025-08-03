--  =============================================================================
--  Test_Progress_Tracker_Contracts - Contract Validation Tests
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Contract validation tests for Progress_Tracker completeness contracts
--  =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;
with Abohlib.Core.Domain.Result;

package Test_Progress_Tracker_Contracts is

   use Abohlib.Infrastructure.Testing.Test_Framework;

   --  Result types for testing
   package Void_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => Boolean,
      Err_Type => Test_Error);

   --  Test suite runner
   function Run_All_Tests (Output : access Test_Output_Port'Class)
                          return Test_Stats_Result.Result;

private

   --  Individual test functions
   function Test_Update_Preconditions return Void_Result.Result;
   function Test_Get_Progress_Postconditions return Void_Result.Result;
   function Test_Is_All_Complete_Postcondition return Void_Result.Result;
   function Test_Completion_State_Consistency return Void_Result.Result;
   function Test_Thread_Safety_Contracts return Void_Result.Result;
   function Test_Progress_Tracking_Lifecycle return Void_Result.Result;

end Test_Progress_Tracker_Contracts;
