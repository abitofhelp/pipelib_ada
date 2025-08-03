--  =============================================================================
--  Test_Algorithm_Contracts - Contract Validation Tests
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Contract validation tests for Algorithm value object operations
--  =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;
with Abohlib.Core.Domain.Result;

package Test_Algorithm_Contracts is

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
   function Test_Name_Accessor_Contracts return Void_Result.Result;
   function Test_Is_Valid_Format_Contracts return Void_Result.Result;
   function Test_Factory_Function_Contracts return Void_Result.Result;
   function Test_Type_Invariant return Void_Result.Result;
   function Test_Value_Object_Semantics return Void_Result.Result;

end Test_Algorithm_Contracts;
