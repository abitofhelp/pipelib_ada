--  =============================================================================
--  Test_Stage_Interface - Stage Interface Unit Tests Specification
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Comprehensive unit tests for the Stage Interface using the abohlib testing
--    framework with interface contract verification and polymorphism testing.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Stage_Interface is

   use Abohlib.Infrastructure.Testing.Test_Framework;

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   with Post => Run_All_Tests'Result.Is_Ok or Run_All_Tests'Result.Is_Err;
   --  Execute all Stage Interface tests and return statistics

private

   --  Test functions with contracts
   function Test_Interface_Instantiation return Void_Result.Result
   with Post => Test_Interface_Instantiation'Result.Is_Ok or
                Test_Interface_Instantiation'Result.Is_Err;

   function Test_Concrete_Implementation return Void_Result.Result
   with Post => Test_Concrete_Implementation'Result.Is_Ok or
                Test_Concrete_Implementation'Result.Is_Err;

   function Test_Interface_Polymorphism return Void_Result.Result
   with Post => Test_Interface_Polymorphism'Result.Is_Ok or
                Test_Interface_Polymorphism'Result.Is_Err;

   function Test_Default_Implementations return Void_Result.Result
   with Post => Test_Default_Implementations'Result.Is_Ok or
                Test_Default_Implementations'Result.Is_Err;

   function Test_Multiple_Implementations return Void_Result.Result
   with Post => Test_Multiple_Implementations'Result.Is_Ok or
                Test_Multiple_Implementations'Result.Is_Err;

   function Test_Interface_Contract_Validation return Void_Result.Result
   with Post => Test_Interface_Contract_Validation'Result.Is_Ok or
                Test_Interface_Contract_Validation'Result.Is_Err;

end Test_Stage_Interface;
