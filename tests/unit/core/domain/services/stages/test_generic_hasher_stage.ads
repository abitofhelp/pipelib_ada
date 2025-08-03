--  =============================================================================
--  Test_Generic_Hasher_Stage - Generic Hasher Stage Unit Tests
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Generic_Hasher_Stage is

   use Abohlib.Infrastructure.Testing.Test_Framework;

   --  Test Functions
   function Test_Create_Hasher_Stage return Void_Result.Result;
   function Test_Process_Single_Chunk return Void_Result.Result;
   function Test_Process_Multiple_Chunks return Void_Result.Result;
   function Test_Finalize_Hash return Void_Result.Result;
   function Test_Reset_Stage return Void_Result.Result;
   function Test_Statistics_Tracking return Void_Result.Result;
   function Test_Invalid_Operations return Void_Result.Result;
   function Test_Empty_Data_Handling return Void_Result.Result;

   --  Test Runner
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

end Test_Generic_Hasher_Stage;
