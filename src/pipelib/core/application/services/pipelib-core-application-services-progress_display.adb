--  =============================================================================
--  Pipelib.Core.Application.Services.Progress_Display - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Text_IO;

package body Pipelib.Core.Application.Services.Progress_Display is

   --  ANSI escape code constants for console formatting
   ESC       : constant Character := Character'Val (27);
   Green     : constant String := ESC & "[32m";
   Reset     : constant String := ESC & "[0m";
   Checkmark : constant String := Green & "[OK]" & Reset;

   --  ## Format Number with Fixed Width
   --
   --  Formats a natural number with consistent right-aligned spacing
   --  to ensure clean visual presentation in progress display.
   function Format_Count (N : Natural) return String is
      Img : constant String := Natural'Image (N);
   begin
      -- Remove leading space and pad to 4 characters for alignment
      if N < 10 then
         return "   " & Img (Img'First + 1 .. Img'Last);
      elsif N < 100 then
         return "  " & Img (Img'First + 1 .. Img'Last);
      elsif N < 1000 then
         return " " & Img (Img'First + 1 .. Img'Last);
      else
         return Img (Img'First + 1 .. Img'Last);
      end if;
   end Format_Count;

   procedure Initialize_Display (Display : in out Progress_Display_Type) is
   begin
      if not Display.Is_Initialized then
         Display.Is_Initialized := True;
         Display.First_Display := True;
      end if;
   end Initialize_Display;

   procedure Display_Progress
     (Display : in out Progress_Display_Type;
      State   : Pipelib.Core.Domain.Services.Progress_Tracker.Progress_State)
   is
   begin
      -- Ensure display is initialized
      if not Display.Is_Initialized then
         Initialize_Display (Display);
      end if;

      -- Handle cursor positioning for in-place updates
      if Display.First_Display then
         Display.First_Display := False;
         -- Save cursor position on first display
         Ada.Text_IO.Put (ESC & "[s");
      else
         -- Restore cursor position for updates
         Ada.Text_IO.Put (ESC & "[u");
      end if;

      -- Display read stage progress
      Ada.Text_IO.Put ("  Read:      " & Format_Count (State.Chunks_Read));
      if State.Read_Complete then
         Ada.Text_IO.Put (" " & Checkmark);
      end if;
      Ada.Text_IO.Put_Line (ESC & "[K"); -- Clear to end of line

      -- Display processing stage progress
      Ada.Text_IO.Put
        ("  Processed: " & Format_Count (State.Chunks_Processed));
      if State.Process_Complete then
         Ada.Text_IO.Put (" " & Checkmark);
      end if;
      Ada.Text_IO.Put_Line (ESC & "[K"); -- Clear to end of line

      -- Display write stage progress
      Ada.Text_IO.Put ("  Written:   " & Format_Count (State.Chunks_Written));
      if State.Write_Complete then
         Ada.Text_IO.Put (" " & Checkmark);
      end if;
      Ada.Text_IO.Put (ESC & "[K"); -- Clear to end of line

      -- Hide cursor to avoid visual artifacts during updates
      Ada.Text_IO.Put (ESC & "[?25l");
      Ada.Text_IO.Flush;
   end Display_Progress;

   procedure Finalize_Display (Display : in out Progress_Display_Type) is
   begin
      if Display.Is_Initialized then
         -- Show cursor and move to next line
         Ada.Text_IO.Put_Line (ESC & "[?25h"); -- Show cursor
         Ada.Text_IO.New_Line; -- Move to next line for subsequent output
         Display.Is_Initialized := False;
      end if;
   end Finalize_Display;

end Pipelib.Core.Application.Services.Progress_Display;
