--  =============================================================================
--  Test_Performance_Suite - Performance and Stress Tests
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Performance_Suite is

   use Abohlib.Infrastructure.Testing.Test_Framework;

   --  Performance Test Functions
   function Test_Chunk_Creation_Performance return Void_Result.Result;
   function Test_Pipeline_Throughput return Void_Result.Result;
   function Test_Memory_Mapped_Performance return Void_Result.Result;
   function Test_Hasher_Throughput return Void_Result.Result;
   function Test_Concurrent_Pipeline_Processing return Void_Result.Result;

   --  Stress Test Functions
   function Test_Large_File_Processing return Void_Result.Result;
   function Test_Many_Small_Chunks return Void_Result.Result;
   function Test_Memory_Pressure return Void_Result.Result;
   function Test_Pipeline_Saturation return Void_Result.Result;
   function Test_Error_Recovery_Under_Load return Void_Result.Result;

   --  Test Runner
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

end Test_Performance_Suite;
