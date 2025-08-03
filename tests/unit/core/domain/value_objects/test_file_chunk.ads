--  =============================================================================
--  Test_File_Chunk - File Chunk Value Object Unit Tests
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_File_Chunk is

   use Abohlib.Infrastructure.Testing.Test_Framework;

   --  Test Functions
   function Test_Create_Valid_Chunk return Void_Result.Result;
   function Test_Create_With_Checksum return Void_Result.Result;
   function Test_Data_Ownership_Transfer return Void_Result.Result;
   function Test_Chunk_Sequence_Number return Void_Result.Result;
   function Test_Chunk_Offset_Validation return Void_Result.Result;
   function Test_Final_Chunk_Flag return Void_Result.Result;
   function Test_Data_Size_Limits return Void_Result.Result;
   function Test_Checksum_Operations return Void_Result.Result;
   function Test_Chunk_Equality return Void_Result.Result;
   function Test_Chunk_Validation return Void_Result.Result;
   function Test_Chunk_Image_Representation return Void_Result.Result;
   function Test_Contract_Violations return Void_Result.Result;
   function Test_Zero_Copy_Semantics return Void_Result.Result;
   function Test_Concurrent_Access_Safety return Void_Result.Result;

   --  Test Runner
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

end Test_File_Chunk;
