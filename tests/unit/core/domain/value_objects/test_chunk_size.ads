--  =============================================================================
--  Test_Chunk_Size - Chunk Size Value Object Unit Tests Specification
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Comprehensive unit tests for the Chunk_Size value object using the
--    abohlib testing framework with contract verification.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Chunk_Size is

   use Abohlib.Infrastructure.Testing.Test_Framework;

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   with Post => Run_All_Tests'Result.Is_Ok or Run_All_Tests'Result.Is_Err;
   --  Execute all Chunk_Size tests and return statistics

private

   --  Test functions with contracts
   function Test_Create_Valid_Size return Void_Result.Result
   with Post => Test_Create_Valid_Size'Result.Is_Ok or
                Test_Create_Valid_Size'Result.Is_Err;

   function Test_Create_Invalid_Size return Void_Result.Result
   with Post => Test_Create_Invalid_Size'Result.Is_Ok or
                Test_Create_Invalid_Size'Result.Is_Err;

   function Test_Factory_Methods return Void_Result.Result
   with Post => Test_Factory_Methods'Result.Is_Ok or
                Test_Factory_Methods'Result.Is_Err;

   function Test_Adaptive_Sizing return Void_Result.Result
   with Post => Test_Adaptive_Sizing'Result.Is_Ok or
                Test_Adaptive_Sizing'Result.Is_Err;

   function Test_Common_Sizes return Void_Result.Result
   with Post => Test_Common_Sizes'Result.Is_Ok or
                Test_Common_Sizes'Result.Is_Err;

   function Test_Comparisons return Void_Result.Result
   with Post => Test_Comparisons'Result.Is_Ok or
                Test_Comparisons'Result.Is_Err;

   function Test_Is_Valid return Void_Result.Result
   with Post => Test_Is_Valid'Result.Is_Ok or
                Test_Is_Valid'Result.Is_Err;

   function Test_Boundary_Values return Void_Result.Result
   with Post => Test_Boundary_Values'Result.Is_Ok or
                Test_Boundary_Values'Result.Is_Err;

   function Test_Power_Of_Two_Sizes return Void_Result.Result
   with Post => Test_Power_Of_Two_Sizes'Result.Is_Ok or
                Test_Power_Of_Two_Sizes'Result.Is_Err;

   function Test_Adaptive_Edge_Cases return Void_Result.Result
   with Post => Test_Adaptive_Edge_Cases'Result.Is_Ok or
                Test_Adaptive_Edge_Cases'Result.Is_Err;

   function Test_Conversion_Accuracy return Void_Result.Result
   with Post => Test_Conversion_Accuracy'Result.Is_Ok or
                Test_Conversion_Accuracy'Result.Is_Err;

   function Test_Named_Size_Relationships return Void_Result.Result
   with Post => Test_Named_Size_Relationships'Result.Is_Ok or
                Test_Named_Size_Relationships'Result.Is_Err;

   function Test_Contract_Violations return Void_Result.Result
   with Post => Test_Contract_Violations'Result.Is_Ok or
                Test_Contract_Violations'Result.Is_Err;

   function Test_Performance_Characteristics return Void_Result.Result
   with Post => Test_Performance_Characteristics'Result.Is_Ok or
                Test_Performance_Characteristics'Result.Is_Err;

end Test_Chunk_Size;
