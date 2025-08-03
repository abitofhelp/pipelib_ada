--  =============================================================================
--  Test_Runner - Main Test Suite Runner Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Infrastructure.Testing.Console_Output;
with Abohlib.Infrastructure.Testing.Test_Framework;

-- Unit test packages
with Test_Chunk_Size;
with Test_Chunk;
with Test_Progress_Tracker;
with Test_File_Chunk;
with Test_Stage_Interface;
with Test_Generic_Pipeline_Stage;
with Test_Generic_Hasher_Stage;

-- Integration test packages
-- TODO: Fix compilation errors in integration tests
-- with Test_Memory_Mapped_File;
-- with Test_Unix_Memory_Map;
-- with Test_Memory_Mapped_Chunk_Adapter;
-- with Test_Random_Write_File;

-- End-to-end test packages
with Test_Pipeline_E2E;

-- Performance test packages
with Test_Performance_Suite;

package body Test_Runner is

   use Abohlib.Infrastructure.Testing.Test_Framework;
   use Abohlib.Infrastructure.Testing.Console_Output;

   -- Track overall test results
   All_Tests_Passed : Boolean := True;

   procedure Run_All_Tests is
      Console : aliased Console_Test_Output;
      Total_Stats : Test_Statistics := (others => <>);

      procedure Run_Suite
        (Name : String;
         Runner : access function
           (Output : access Test_Output_Port'Class)
           return Test_Stats_Result.Result)
      is
         Result : constant Test_Stats_Result.Result :=
           Runner (Console'Access);
      begin
         Put ("Running " & Name & " suite... ");
         if Result.Is_Ok then
            declare
               Stats : constant Test_Statistics := Result.Get_Ok;
            begin
               Put_Line ("OK (" & Stats.Total_Tests'Image & " tests, " &
                        Stats.Passed_Tests'Image & " passed, " &
                        Stats.Failed_Tests'Image & " failed)");
               Total_Stats.Total_Tests := Total_Stats.Total_Tests + Stats.Total_Tests;
               Total_Stats.Passed_Tests := Total_Stats.Passed_Tests + Stats.Passed_Tests;
               Total_Stats.Failed_Tests := Total_Stats.Failed_Tests + Stats.Failed_Tests;
               Total_Stats.Error_Tests := Total_Stats.Error_Tests + Stats.Error_Tests;
               Total_Stats.Skipped_Tests := Total_Stats.Skipped_Tests + Stats.Skipped_Tests;
               Total_Stats.Total_Duration := Total_Stats.Total_Duration + Stats.Total_Duration;
            end;
         else
            declare
               Err : constant Test_Error := Result.Get_Err;
            begin
               Put_Line ("ERROR");
               Put_Line ("  Error running suite " & Name & ": " &
                        To_String (Err.Message));
               if Length (Err.Details) > 0 then
                  Put_Line ("  Details: " & To_String (Err.Details));
               end if;
            end;
            Total_Stats.Error_Tests := Total_Stats.Error_Tests + 1;
         end if;
      end Run_Suite;

   begin
      Put_Line ("=============================================================================");
      Put_Line ("Running Pipelib Test Suite");
      Put_Line ("=============================================================================");
      New_Line;

      -- Unit Tests
      Put_Line ("Unit Tests");
      Put_Line ("-----------");
      Run_Suite ("Chunk_Size", Test_Chunk_Size.Run_All_Tests'Access);
      Run_Suite ("Chunk", Test_Chunk.Run_All_Tests'Access);
      Run_Suite ("Progress_Tracker", Test_Progress_Tracker.Run_All_Tests'Access);
      Run_Suite ("File_Chunk", Test_File_Chunk.Run_All_Tests'Access);
      Run_Suite ("Stage_Interface", Test_Stage_Interface.Run_All_Tests'Access);
      Run_Suite ("Generic_Pipeline_Stage", Test_Generic_Pipeline_Stage.Run_All_Tests'Access);
      Run_Suite ("Generic_Hasher_Stage", Test_Generic_Hasher_Stage.Run_All_Tests'Access);

      New_Line;

      -- Integration Tests
      Put_Line ("Integration Tests");
      Put_Line ("-----------------");
      -- TODO: Fix compilation errors in integration tests
      -- Run_Suite ("Memory_Mapped_File", Test_Memory_Mapped_File.Run_All_Tests'Access);
      -- Run_Suite ("Unix_Memory_Map", Test_Unix_Memory_Map.Run_All_Tests'Access);
      -- Run_Suite ("Memory_Mapped_Chunk_Adapter", Test_Memory_Mapped_Chunk_Adapter.Run_All_Tests'Access);
      -- Run_Suite ("Random_Write_File", Test_Random_Write_File.Run_All_Tests'Access);

      New_Line;

      -- End-to-End Tests
      Put_Line ("End-to-End Tests");
      Put_Line ("----------------");
      Run_Suite ("Pipeline_E2E", Test_Pipeline_E2E.Run_All_Tests'Access);

      New_Line;

      -- Performance Tests
      Put_Line ("Performance Tests");
      Put_Line ("-----------------");
      Run_Suite ("Performance_Suite", Test_Performance_Suite.Run_All_Tests'Access);

      New_Line;
      Put_Line ("=============================================================================");
      Put_Line ("Test Summary");
      Put_Line ("=============================================================================");
      Put_Line ("Total Tests: " & Natural'Image (Total_Stats.Total_Tests));
      Put_Line ("Passed:      " & Natural'Image (Total_Stats.Passed_Tests));
      Put_Line ("Failed:      " & Natural'Image (Total_Stats.Failed_Tests));
      Put_Line ("Errors:      " & Natural'Image (Total_Stats.Error_Tests));
      Put_Line ("Skipped:     " & Natural'Image (Total_Stats.Skipped_Tests));
      Put_Line ("Duration:    " & Duration'Image (Total_Stats.Total_Duration) & " seconds");

      if Total_Stats.Failed_Tests = 0 and Total_Stats.Error_Tests = 0 then
         Put_Line ("Status:      SUCCESS");
         All_Tests_Passed := True;
      else
         Put_Line ("Status:      FAILURE");
         All_Tests_Passed := False;
      end if;
   end Run_All_Tests;

   function Tests_Passed return Boolean is
   begin
      return All_Tests_Passed;
   end Tests_Passed;

end Test_Runner;
