--  =============================================================================
--  Core.Domain.Services.Progress_Tracker - Simple Progress Tracking
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Thread-safe progress tracker for pipeline stages
--  =============================================================================

pragma Ada_2022;

package Pipelib.Core.Domain.Services.Progress_Tracker is

   protected type Progress_Tracker_Type is
      --  Update counters
      procedure Update_Read_Count (Count : Natural);
      procedure Update_Processed_Count (Count : Natural);
      procedure Update_Written_Count (Count : Natural);

      --  Mark stages as complete
      procedure Mark_Read_Complete;
      procedure Mark_Processing_Complete;
      procedure Mark_Writing_Complete;

      --  Get current progress
      procedure Get_Progress (Read, Processed, Written : out Natural);

      --  Display current progress (with carriage return for updating in place)
      procedure Display_Progress;

   private
      Chunks_Read      : Natural := 0;
      Chunks_Processed : Natural := 0;
      Chunks_Written   : Natural := 0;
      Read_Complete    : Boolean := False;
      Process_Complete : Boolean := False;
      Write_Complete   : Boolean := False;
      First_Display    : Boolean := True;
   end Progress_Tracker_Type;

   type Progress_Tracker_Access is access all Progress_Tracker_Type;

end Pipelib.Core.Domain.Services.Progress_Tracker;
