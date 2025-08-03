--  =============================================================================
--  Core.Domain.Services.Progress_Tracker - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Text_IO;

package body Pipelib.Core.Domain.Services.Progress_Tracker is

   protected body Progress_Tracker_Type is

      procedure Update_Read_Count (Count : Natural) is
      begin
         Chunks_Read := Count;
      end Update_Read_Count;

      procedure Update_Processed_Count (Count : Natural) is
      begin
         Chunks_Processed := Count;
      end Update_Processed_Count;

      procedure Update_Written_Count (Count : Natural) is
      begin
         Chunks_Written := Count;
      end Update_Written_Count;

      procedure Mark_Read_Complete is
      begin
         Read_Complete := True;
      end Mark_Read_Complete;

      procedure Mark_Processing_Complete is
      begin
         Process_Complete := True;
      end Mark_Processing_Complete;

      procedure Mark_Writing_Complete is
      begin
         Write_Complete := True;
      end Mark_Writing_Complete;

      procedure Get_Progress (Read, Processed, Written : out Natural) is
      begin
         Read := Chunks_Read;
         Processed := Chunks_Processed;
         Written := Chunks_Written;
      end Get_Progress;

      procedure Display_Progress is
         -- ANSI escape codes
         ESC        : constant Character := Character'Val(27);
         Green      : constant String := ESC & "[32m";
         Reset      : constant String := ESC & "[0m";
         -- Simple ASCII checkmark since UTF-8 isn't working properly
         Checkmark  : constant String := Green & "[OK]" & Reset;

         -- Format number with fixed width (4 digits)
         function Format_Count (N : Natural) return String is
            Img : constant String := Natural'Image(N);
         begin
            -- Remove leading space and pad to 4 characters
            if N < 10 then
               return "   " & Img(Img'First + 1 .. Img'Last);
            elsif N < 100 then
               return "  " & Img(Img'First + 1 .. Img'Last);
            elsif N < 1000 then
               return " " & Img(Img'First + 1 .. Img'Last);
            else
               return Img(Img'First + 1 .. Img'Last);
            end if;
         end Format_Count;

      begin
         if First_Display then
            First_Display := False;
            -- Save cursor position on first display
            Ada.Text_IO.Put (ESC & "[s");
         else
            -- Restore cursor position for updates
            Ada.Text_IO.Put (ESC & "[u");
         end if;

         -- Display each line, clearing to end of line
         Ada.Text_IO.Put ("  Read:      " & Format_Count(Chunks_Read));
         if Read_Complete then
            Ada.Text_IO.Put (" " & Checkmark);
         end if;
         Ada.Text_IO.Put_Line (ESC & "[K"); -- Clear to end of line

         Ada.Text_IO.Put ("  Processed: " & Format_Count(Chunks_Processed));
         if Process_Complete then
            Ada.Text_IO.Put (" " & Checkmark);
         end if;
         Ada.Text_IO.Put_Line (ESC & "[K"); -- Clear to end of line

         Ada.Text_IO.Put ("  Written:   " & Format_Count(Chunks_Written));
         if Write_Complete then
            Ada.Text_IO.Put (" " & Checkmark);
         end if;
         Ada.Text_IO.Put (ESC & "[K"); -- Clear to end of line
         -- Hide cursor to avoid the white rectangle
         Ada.Text_IO.Put (ESC & "[?25l");
         Ada.Text_IO.Flush;
      end Display_Progress;

   end Progress_Tracker_Type;

end Pipelib.Core.Domain.Services.Progress_Tracker;
