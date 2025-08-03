--  =============================================================================
--  Test_Memory_Mapped_Chunk_Adapter_Contracts - Contract Validation Tests
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Contract validation tests for Memory_Mapped_Chunk_Adapter operations
--  =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;
with Abohlib.Core.Domain.Result;

package Test_Memory_Mapped_Chunk_Adapter_Contracts is

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
   function Test_Calculate_Optimal_Chunk_Size_Contracts return Void_Result.Result;
   function Test_Should_Use_Memory_Mapping_Contracts return Void_Result.Result;
   function Test_Create_Stream_Array_Access_Contracts return Void_Result.Result;
   function Test_Memory_To_Stream_Elements_Contracts return Void_Result.Result;
   function Test_Create_Single_Chunk_Contracts return Void_Result.Result;
   function Test_Create_Chunks_From_Memory_Map_Contracts return Void_Result.Result;

end Test_Memory_Mapped_Chunk_Adapter_Contracts;
