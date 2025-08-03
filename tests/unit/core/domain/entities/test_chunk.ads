--  =============================================================================
--  Test_Chunk - Chunk Entity Unit Tests Specification
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Comprehensive unit tests for the Chunk entity using the abohlib testing
--    framework with contract verification and state machine validation.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Chunk is

   use Abohlib.Infrastructure.Testing.Test_Framework;

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   with Post => Run_All_Tests'Result.Is_Ok or Run_All_Tests'Result.Is_Err;
   --  Execute all Chunk entity tests and return statistics

private

   --  Test functions with contracts
   function Test_Create_Chunk return Void_Result.Result
   with Post => Test_Create_Chunk'Result.Is_Ok or
                Test_Create_Chunk'Result.Is_Err;

   function Test_Identity_Immutability return Void_Result.Result
   with Post => Test_Identity_Immutability'Result.Is_Ok or
                Test_Identity_Immutability'Result.Is_Err;

   function Test_State_Machine_Transitions return Void_Result.Result
   with Post => Test_State_Machine_Transitions'Result.Is_Ok or
                Test_State_Machine_Transitions'Result.Is_Err;

   function Test_Invalid_State_Transitions return Void_Result.Result
   with Post => Test_Invalid_State_Transitions'Result.Is_Ok or
                Test_Invalid_State_Transitions'Result.Is_Err;

   function Test_Data_Management return Void_Result.Result
   with Post => Test_Data_Management'Result.Is_Ok or
                Test_Data_Management'Result.Is_Err;

   function Test_Zero_Copy_Operations return Void_Result.Result
   with Post => Test_Zero_Copy_Operations'Result.Is_Ok or
                Test_Zero_Copy_Operations'Result.Is_Err;

   function Test_Compression_Info return Void_Result.Result
   with Post => Test_Compression_Info'Result.Is_Ok or
                Test_Compression_Info'Result.Is_Err;

   function Test_Processing_Metrics return Void_Result.Result
   with Post => Test_Processing_Metrics'Result.Is_Ok or
                Test_Processing_Metrics'Result.Is_Err;

   function Test_Chunk_Validation return Void_Result.Result
   with Post => Test_Chunk_Validation'Result.Is_Ok or
                Test_Chunk_Validation'Result.Is_Err;

   function Test_Chunk_Reset return Void_Result.Result
   with Post => Test_Chunk_Reset'Result.Is_Ok or
                Test_Chunk_Reset'Result.Is_Err;

   function Test_Contract_Violations return Void_Result.Result
   with Post => Test_Contract_Violations'Result.Is_Ok or
                Test_Contract_Violations'Result.Is_Err;

   function Test_Entity_Equality return Void_Result.Result
   with Post => Test_Entity_Equality'Result.Is_Ok or
                Test_Entity_Equality'Result.Is_Err;

   function Test_State_Transition_Matrix return Void_Result.Result
   with Post => Test_State_Transition_Matrix'Result.Is_Ok or
                Test_State_Transition_Matrix'Result.Is_Err;

   function Test_State_Transition_Contract_Violations return Void_Result.Result
   with Post => Test_State_Transition_Contract_Violations'Result.Is_Ok or
                Test_State_Transition_Contract_Violations'Result.Is_Err;

   function Test_Data_Ownership_Transfer return Void_Result.Result
   with Post => Test_Data_Ownership_Transfer'Result.Is_Ok or
                Test_Data_Ownership_Transfer'Result.Is_Err;

   function Test_Concurrent_Access_Safety return Void_Result.Result
   with Post => Test_Concurrent_Access_Safety'Result.Is_Ok or
                Test_Concurrent_Access_Safety'Result.Is_Err;

end Test_Chunk;
