--  =============================================================================
--  Core.Domain.Services.Progress_Tracker - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

package body Pipelib.Core.Domain.Services.Progress_Tracker is

   protected body Progress_Tracker_Type is

      procedure Update_Read_Count (Count : Read_Count_Type) is
      begin
         Chunks_Read := Count;
      end Update_Read_Count;

      procedure Update_Processed_Count (Count : Processed_Count_Type) is
      begin
         Chunks_Processed := Count;
      end Update_Processed_Count;

      procedure Update_Written_Count (Count : Written_Count_Type) is
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

      procedure Get_Progress
        (Read : out Read_Count_Type;
         Processed : out Processed_Count_Type;
         Written : out Written_Count_Type) is
      begin
         Read := Chunks_Read;
         Processed := Chunks_Processed;
         Written := Chunks_Written;
      end Get_Progress;

      function Is_Read_Complete return Boolean is
      begin
         return Read_Complete;
      end Is_Read_Complete;

      function Is_Processing_Complete return Boolean is
      begin
         return Process_Complete;
      end Is_Processing_Complete;

      function Is_Writing_Complete return Boolean is
      begin
         return Write_Complete;
      end Is_Writing_Complete;

      function Is_All_Complete return Boolean is
      begin
         return Read_Complete and Process_Complete and Write_Complete;
      end Is_All_Complete;

      function Get_Progress_State return Progress_State is
      begin
         return
           (Chunks_Read      => Chunks_Read,
            Chunks_Processed => Chunks_Processed,
            Chunks_Written   => Chunks_Written,
            Read_Complete    => Read_Complete,
            Process_Complete => Process_Complete,
            Write_Complete   => Write_Complete);
      end Get_Progress_State;

   end Progress_Tracker_Type;

end Pipelib.Core.Domain.Services.Progress_Tracker;
