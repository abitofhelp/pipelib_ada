--  =============================================================================
--  Test_Random_Write_File_Contracts - Contract Validation Tests
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Contract validation tests for Random_Write_File resource management
--  =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;
with Abohlib.Core.Domain.Result;

package Test_Random_Write_File_Contracts is

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
   function Test_Create_Contracts return Void_Result.Result;
   function Test_Write_Operations_Contracts return Void_Result.Result;
   function Test_Is_Open_And_Size_Contracts return Void_Result.Result;
   function Test_Commit_Rollback_Close_Contracts return Void_Result.Result;
   function Test_Destroy_Contracts return Void_Result.Result;
   function Test_Protected_Type_Contracts return Void_Result.Result;
   function Test_Resource_Lifecycle return Void_Result.Result;

end Test_Random_Write_File_Contracts;
