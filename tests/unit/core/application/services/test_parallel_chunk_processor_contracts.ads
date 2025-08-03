--  =============================================================================
--  Test_Parallel_Chunk_Processor_Contracts - Contract Validation Tests
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Contract validation tests for Parallel_Chunk_Processor lifecycle management
--  =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;
with Abohlib.Core.Domain.Result;

package Test_Parallel_Chunk_Processor_Contracts is

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
   function Test_Start_Stop_Contracts return Void_Result.Result;
   function Test_Submit_Chunk_Contracts return Void_Result.Result;
   function Test_Wait_For_Completion_Contracts return Void_Result.Result;
   function Test_Destroy_Contracts return Void_Result.Result;
   function Test_Query_Function_Contracts return Void_Result.Result;
   function Test_Lifecycle_State_Transitions return Void_Result.Result;

end Test_Parallel_Chunk_Processor_Contracts;
