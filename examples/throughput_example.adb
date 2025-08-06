--  =============================================================================
--  Example: Using Abohlib Performance Helper Functions
--  =============================================================================

pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Abohlib.Core.Domain.Types.Bytes; use Abohlib.Core.Domain.Types.Bytes;
with Abohlib.Core.Domain.Types.Performance; use Abohlib.Core.Domain.Types.Performance;
with Abohlib.Core.Domain.Constants.Bytes; use Abohlib.Core.Domain.Constants.Bytes;

procedure Throughput_Example is

   -- Example data
   Total_Bytes     : constant SI_Bytes_Type := From_Natural (100) * SI_MB;  -- 100 MB
   Bytes_Processed : constant SI_Bytes_Type := From_Natural (75) * SI_MB;   -- 75 MB

   -- Timing
   Start_Time : constant Time := Clock;
   -- Simulate some processing time
   End_Time   : constant Time := Start_Time + 2.5;  -- 2.5 seconds
   Duration   : constant Standard.Duration := End_Time - Start_Time;

begin
   -- Calculate throughput using the helper function
   declare
      Throughput : constant MB_Per_Second_Type :=
         Calculate_MB_Per_Second (Bytes_Processed, Duration);

      -- Calculate progress percentage
      Progress : constant Percentage_Type :=
         Calculate_Percentage (Bytes_Processed, Total_Bytes);
   begin
      Put_Line ("Processed: " & To_Natural (Bytes_Processed / SI_MB)'Image & " MB");
      Put_Line ("Duration: " & Duration'Image & " seconds");
      Put_Line ("Throughput: " & Float (Throughput)'Image & " MB/s");
      Put_Line ("Progress: " & Float (Progress)'Image & "%");
   end;

   -- Example with compression ratio
   declare
      Original_Size   : constant SI_Bytes_Type := From_Natural (1000) * SI_KB;
      Compressed_Size : constant SI_Bytes_Type := From_Natural (350) * SI_KB;

      Ratio : constant Compression_Ratio_Type :=
         Calculate_Compression_Ratio (Original_Size, Compressed_Size);

      Saved : constant Percentage_Type :=
         Compression_Percentage_Saved (Ratio);
   begin
      Put_Line ("Compression ratio: " & Float (Ratio)'Image);
      Put_Line ("Space saved: " & Float (Saved)'Image & "%");
   end;
end Throughput_Example;
