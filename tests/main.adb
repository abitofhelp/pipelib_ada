--  =============================================================================
--  Main - Test Suite Entry Point
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Test_Runner;
with Ada.Command_Line;

procedure Main is
   use Ada.Command_Line;
begin
   Test_Runner.Run_All_Tests;

   -- Exit with appropriate status code
   if Test_Runner.Tests_Passed then
      Set_Exit_Status (Success);
   else
      Set_Exit_Status (Failure);
   end if;
end Main;
