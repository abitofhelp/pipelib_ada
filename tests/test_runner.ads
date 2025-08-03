--  =============================================================================
--  Test_Runner - Main Test Suite Runner Specification
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Main entry point for running all tests in the pipelib test suite.
--    Uses the abohlib testing framework exclusively.
--  =============================================================================

pragma Ada_2022;

package Test_Runner is

   procedure Run_All_Tests;
   --  Execute all test suites and report results

   function Tests_Passed return Boolean;
   --  Returns True if all tests passed, False otherwise

end Test_Runner;
