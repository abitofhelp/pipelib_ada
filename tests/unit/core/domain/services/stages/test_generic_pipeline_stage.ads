--  =============================================================================
--  Test_Generic_Pipeline_Stage - Generic Pipeline Stage Unit Tests Specification
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Comprehensive unit tests for the Generic Pipeline Stage using the abohlib
--    testing framework with contract verification and Ada 2022 features.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Generic_Pipeline_Stage is

   use Abohlib.Infrastructure.Testing.Test_Framework;

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   with Post => Run_All_Tests'Result.Is_Ok or Run_All_Tests'Result.Is_Err;
   --  Execute all Generic Pipeline Stage tests and return statistics

private

   --  Test functions with contracts
   function Test_Create_Pipeline_Stage return Void_Result.Result
   with Post => Test_Create_Pipeline_Stage'Result.Is_Ok or
                Test_Create_Pipeline_Stage'Result.Is_Err;

   function Test_Process_Single_Item return Void_Result.Result
   with Post => Test_Process_Single_Item'Result.Is_Ok or
                Test_Process_Single_Item'Result.Is_Err;

   function Test_Process_Multiple_Items return Void_Result.Result
   with Post => Test_Process_Multiple_Items'Result.Is_Ok or
                Test_Process_Multiple_Items'Result.Is_Err;

   function Test_Process_Batch return Void_Result.Result
   with Post => Test_Process_Batch'Result.Is_Ok or
                Test_Process_Batch'Result.Is_Err;

   function Test_Process_Parallel return Void_Result.Result
   with Post => Test_Process_Parallel'Result.Is_Ok or
                Test_Process_Parallel'Result.Is_Err;

   function Test_Statistics_Tracking return Void_Result.Result
   with Post => Test_Statistics_Tracking'Result.Is_Ok or
                Test_Statistics_Tracking'Result.Is_Err;

   function Test_Error_Handling return Void_Result.Result
   with Post => Test_Error_Handling'Result.Is_Ok or
                Test_Error_Handling'Result.Is_Err;

   function Test_State_Management return Void_Result.Result
   with Post => Test_State_Management'Result.Is_Ok or
                Test_State_Management'Result.Is_Err;

   function Test_Reset_Stage return Void_Result.Result
   with Post => Test_Reset_Stage'Result.Is_Ok or
                Test_Reset_Stage'Result.Is_Err;

   function Test_Contract_Validation return Void_Result.Result
   with Post => Test_Contract_Validation'Result.Is_Ok or
                Test_Contract_Validation'Result.Is_Err;

   function Test_Configuration_Management return Void_Result.Result
   with Post => Test_Configuration_Management'Result.Is_Ok or
                Test_Configuration_Management'Result.Is_Err;

   function Test_Image_Representation return Void_Result.Result
   with Post => Test_Image_Representation'Result.Is_Ok or
                Test_Image_Representation'Result.Is_Err;

   function Test_Validation_Functions return Void_Result.Result
   with Post => Test_Validation_Functions'Result.Is_Ok or
                Test_Validation_Functions'Result.Is_Err;

end Test_Generic_Pipeline_Stage;
