--  =============================================================================
--  Test_Chunk_Contracts - Contract Validation Tests for Chunk Entity
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Contract validation tests for Chunk entity state transitions and invariants
--  =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;
with Abohlib.Core.Domain.Result;

package Test_Chunk_Contracts is

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
   function Test_Valid_State_Transitions return Void_Result.Result;
   function Test_Invalid_State_Transitions return Void_Result.Result;
   function Test_Set_State_Precondition return Void_Result.Result;
   function Test_Is_Valid_Postcondition return Void_Result.Result;
   function Test_Create_Postconditions return Void_Result.Result;
   function Test_Set_Data_Ownership_Transfer return Void_Result.Result;
   function Test_Increment_Retry_Count_Contracts return Void_Result.Result;
   function Test_Reset_Postconditions return Void_Result.Result;

end Test_Chunk_Contracts;
