--  =============================================================================
--  Test_Pipeline_E2E - End-to-End Pipeline Tests
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Pipeline_E2E is

   use Abohlib.Infrastructure.Testing.Test_Framework;

   --  End-to-End Test Functions
   function Test_Simple_File_Hashing return Void_Result.Result;
   function Test_Parallel_Pipeline_Processing return Void_Result.Result;
   function Test_Large_File_Pipeline return Void_Result.Result;
   function Test_Multi_Stage_Pipeline return Void_Result.Result;
   function Test_Pipeline_Error_Recovery return Void_Result.Result;
   function Test_Pipeline_Resource_Management return Void_Result.Result;
   function Test_Memory_Mapped_Pipeline return Void_Result.Result;
   function Test_Concurrent_Pipeline_Operations return Void_Result.Result;

   --  Test Runner
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

end Test_Pipeline_E2E;
