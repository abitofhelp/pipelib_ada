--  =============================================================================
--  Test_Memory_Mapped_File_Contracts - Contract Validation Tests
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Contract validation tests for Memory_Mapped_File helper functions
--  =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;
with Abohlib.Core.Domain.Result;

package Test_Memory_Mapped_File_Contracts is

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
   function Test_Map_File_Contracts return Void_Result.Result;
   function Test_Unmap_Contracts return Void_Result.Result;
   function Test_Get_View_And_Size_Contracts return Void_Result.Result;
   function Test_Create_Subview_Contracts return Void_Result.Result;
   function Test_Sync_And_Advise_Contracts return Void_Result.Result;
   function Test_Utility_Function_Contracts return Void_Result.Result;
   function Test_Memory_Mapping_Lifecycle return Void_Result.Result;

end Test_Memory_Mapped_File_Contracts;
