--  =============================================================================
--  Test_Boundary_Conditions - Boundary Condition Tests for Value Objects
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Boundary_Conditions is

   use Abohlib.Infrastructure.Testing.Test_Framework;

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   with Post => Run_All_Tests'Result.Is_Ok or Run_All_Tests'Result.Is_Err;
   --  Execute all boundary condition tests and return statistics

private

   --  Chunk Size boundary tests
   function Test_Chunk_Size_Boundaries return Void_Result.Result
   with Post => Test_Chunk_Size_Boundaries'Result.Is_Ok or
                Test_Chunk_Size_Boundaries'Result.Is_Err;

   function Test_Chunk_Size_Edge_Cases return Void_Result.Result
   with Post => Test_Chunk_Size_Edge_Cases'Result.Is_Ok or
                Test_Chunk_Size_Edge_Cases'Result.Is_Err;

   --  File Chunk boundary tests
   function Test_File_Chunk_Size_Boundaries return Void_Result.Result
   with Post => Test_File_Chunk_Size_Boundaries'Result.Is_Ok or
                Test_File_Chunk_Size_Boundaries'Result.Is_Err;

   function Test_File_Chunk_Offset_Boundaries return Void_Result.Result
   with Post => Test_File_Chunk_Offset_Boundaries'Result.Is_Ok or
                Test_File_Chunk_Offset_Boundaries'Result.Is_Err;

   function Test_File_Chunk_Sequence_Boundaries return Void_Result.Result
   with Post => Test_File_Chunk_Sequence_Boundaries'Result.Is_Ok or
                Test_File_Chunk_Sequence_Boundaries'Result.Is_Err;

   --  Stage Order boundary tests
   function Test_Stage_Order_Boundaries return Void_Result.Result
   with Post => Test_Stage_Order_Boundaries'Result.Is_Ok or
                Test_Stage_Order_Boundaries'Result.Is_Err;

   --  Algorithm name boundary tests
   function Test_Algorithm_Name_Boundaries return Void_Result.Result
   with Post => Test_Algorithm_Name_Boundaries'Result.Is_Ok or
                Test_Algorithm_Name_Boundaries'Result.Is_Err;

end Test_Boundary_Conditions;
