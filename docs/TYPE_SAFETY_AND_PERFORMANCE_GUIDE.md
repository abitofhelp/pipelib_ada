# Type Safety and Performance Guide

## Overview

This guide explains how Pipelib uses strong typing from Abohlib to create safer, more maintainable code while providing comprehensive performance monitoring capabilities. By using distinct types for different concepts, we prevent common programming errors and make code self-documenting.

## Why Strong Types Matter

Consider this common bug:
```ada
-- Without strong types - easy to mix up parameters
procedure Process_Data (Size : Natural; Count : Natural; Timeout : Natural) is
begin
   -- Oops! Wrong parameter order
   Process_Data (Count, Timeout, Size);  -- Compiles but wrong!
end;

-- With strong types - compiler catches the error
procedure Process_Data (Size : SI_Bytes_Type;
                       Count : Chunk_Count_Type;
                       Timeout : Milliseconds_Type) is
begin
   -- This won't compile - types don't match
   Process_Data (Count, Timeout, Size);  -- Compilation error!
end;
```

## Byte Size Types

Pipelib uses Abohlib's byte types for all size measurements:

```ada
with Abohlib.Core.Domain.Types.Bytes; use Abohlib.Core.Domain.Types.Bytes;
with Pipelib.Core.Domain.Value_Objects.Chunk_Size;
use Pipelib.Core.Domain.Value_Objects.Chunk_Size;

-- Creating chunk sizes using predefined constants
Small_Chunk : Chunk_Size_Type := SIZE_1MB;    -- 1 MB chunks
Default_Chunk : Chunk_Size_Type := SIZE_16MB; -- 16 MB chunks
Large_Chunk : Chunk_Size_Type := SIZE_64MB;   -- 64 MB chunks

-- Creating custom sizes
Custom_Size : SI_Bytes_Type := From_MB(32);   -- 32 MB
Tiny_Size : SI_Bytes_Type := From_KB(512);    -- 512 KB

-- Converting from system calls
File_Size_Raw : Long_Long_Integer := Get_File_Size(Path);
File_Size : SI_Bytes_Type := From_Long_Long_Integer(File_Size_Raw);

-- Arithmetic with byte types
Total_Size : SI_Bytes_Type := File_Size + Custom_Size;
Half_Size : SI_Bytes_Type := File_Size / 2;

-- Converting back when needed
Size_For_Display : Natural := To_Natural(Custom_Size / SI_MB);
Put_Line("Size: " & Size_For_Display'Image & " MB");
```

## Count Types and Arithmetic

Pipelib tracks different kinds of counts separately to prevent confusion:

```ada
with Pipelib.Core.Domain.Constants.Count_Arithmetic;
use Pipelib.Core.Domain.Constants.Count_Arithmetic;

-- Different count types for different purposes
Chunks_Read : Read_Count_Type := 0;
Chunks_Processed : Processed_Count_Type := 0;
Chunks_Written : Written_Count_Type := 0;
Errors_Found : Error_Count_Type := 0;

-- Increment operations
Chunks_Read := Increment(Chunks_Read);
Chunks_Processed := Increment(Chunks_Processed);

-- Decrement operations
if Chunks_Written > 0 then
   Chunks_Written := Decrement(Chunks_Written);
end if;

-- Addition and subtraction
Chunks_Read := Chunks_Read + Read_Count_Type(10);
Remaining := Total_Chunks - Chunks_Processed;

-- Comparisons work naturally
if Chunks_Processed >= Chunks_Read then
   Put_Line("All chunks processed!");
end if;
```

## Performance Monitoring

### Throughput Calculations

```ada
with Ada.Calendar; use Ada.Calendar;
with Abohlib.Core.Domain.Types.Performance;
use Abohlib.Core.Domain.Types.Performance;

-- Track processing performance
Start_Time : constant Time := Clock;
Bytes_Processed : SI_Bytes_Type := 0;

-- Process chunks...
for Chunk of Chunks loop
   Process_Chunk(Chunk);
   Bytes_Processed := Bytes_Processed + Chunk.Size;
end loop;

-- Calculate throughput
Elapsed : constant Duration := Clock - Start_Time;
Throughput : MB_Per_Second_Type :=
   Calculate_MB_Per_Second(Bytes_Processed, Elapsed);

Put_Line("Processing speed: " & Float(Throughput)'Image & " MB/s");

-- For file positions (Long_Long_Integer)
File_Position : Long_Long_Integer := 0;
-- ... read file ...
Read_Speed : MB_Per_Second_Type :=
   Calculate_MB_Per_Second(File_Position, Elapsed);
```

### Progress Tracking

```ada
with Abohlib.Core.Domain.Math; use Abohlib.Core.Domain.Math;

-- Calculate completion percentage
Total_Chunks : Chunk_Count_Type := 1000;
Completed : Chunk_Count_Type := 750;

Progress : Percentage_Type := Calculate_Percentage(
   Natural(Completed),
   Natural(Total_Chunks)
);

Put_Line("Progress: " & Float(Progress)'Image & "%");

-- For byte-based progress
Bytes_Done : SI_Bytes_Type := From_GB(3);
Total_Bytes : SI_Bytes_Type := From_GB(10);

File_Progress : Percentage_Type :=
   Abohlib.Core.Domain.Types.Performance.Calculate_Percentage(
      Bytes_Done, Total_Bytes);
```

### Performance Statistics

```ada
-- Complete performance tracking example
declare
   Start_Time : constant Time := Clock;
   Chunks_Processed : Processed_Count_Type := 0;
   Bytes_Processed : SI_Bytes_Type := 0;
   Total_Bytes : constant SI_Bytes_Type := From_GB(5);
begin
   -- Process data
   while Bytes_Processed < Total_Bytes loop
      Process_Next_Chunk;
      Chunks_Processed := Increment(Chunks_Processed);
      Bytes_Processed := Bytes_Processed + SIZE_16MB;

      -- Report progress every 10 chunks
      if Natural(Chunks_Processed) mod 10 = 0 then
         declare
            Elapsed : constant Duration := Clock - Start_Time;
            Speed : constant MB_Per_Second_Type :=
               Calculate_MB_Per_Second(Bytes_Processed, Elapsed);
            Progress : constant Percentage_Type :=
               Calculate_Percentage(Bytes_Processed, Total_Bytes);
         begin
            Put_Line("Processed: " & Natural(Chunks_Processed)'Image & " chunks");
            Put_Line("Speed: " & Float(Speed)'Image & " MB/s");
            Put_Line("Progress: " & Float(Progress)'Image & "%");

            -- Estimate time remaining
            if Speed > 0.0 then
               Bytes_Remaining : constant SI_Bytes_Type :=
                  Total_Bytes - Bytes_Processed;
               Time_Remaining : constant Duration :=
                  Duration(Float(Bytes_Remaining) / Float(SI_MB) / Float(Speed));
               Put_Line("ETA: " & Duration'Image(Time_Remaining) & " seconds");
            end if;
         end;
      end if;
   end loop;
end;
```

## Best Practices

### 1. Use Type-Specific Constants

```ada
-- Good: Clear and type-safe
Chunk_Size : constant Chunk_Size_Type := SIZE_16MB;

-- Avoid: Magic numbers
Chunk_Size : constant Natural := 16777216;  -- What unit is this?
```

### 2. Convert at System Boundaries

```ada
-- Convert immediately when getting external data
procedure Read_File_Info (Path : String) is
   -- Get raw data from system
   Raw_Size : Long_Long_Integer := POSIX_Get_File_Size(Path);
   Raw_Modified : Long_Long_Integer := POSIX_Get_Modified_Time(Path);

   -- Convert to strong types immediately
   File_Size : SI_Bytes_Type := From_Long_Long_Integer(Raw_Size);
   Modified_Time : Time := From_Unix_Timestamp(Raw_Modified);
begin
   -- Use strong types throughout your code
   if File_Size > SIZE_1GB then
      Use_Large_File_Strategy(File_Size);
   end if;
end Read_File_Info;
```

### 3. Use Arithmetic Operations

```ada
-- Good: Type-safe arithmetic
Total_Read := Chunks_Read + Read_Count_Type(10);
Chunks_Read := Increment(Chunks_Read);

-- Avoid: Manual conversions
Total_Read := Read_Count_Type(Natural(Chunks_Read) + 10);
```

### 4. Leverage Helper Functions

```ada
-- Good: Use provided helper functions
Speed := Calculate_MB_Per_Second(Bytes_Processed, Elapsed);
Progress := Calculate_Percentage(Done, Total);

-- Avoid: Manual calculations
Speed := Float(Bytes_Processed) / Float(SI_MB) / Float(Elapsed);
Progress := Float(Done) / Float(Total) * 100.0;
```

## Common Patterns

### Progress Reporting

```ada
procedure Report_Progress
  (Tracker : Progress_Tracker_Type;
   Start_Time : Time) is

   Stats : constant Progress_Info := Tracker.Get_Progress;
   Elapsed : constant Duration := Clock - Start_Time;

   -- Calculate metrics using helper functions
   Read_Speed : constant MB_Per_Second_Type :=
      Calculate_MB_Per_Second(Stats.Bytes_Read, Elapsed);

   Process_Progress : constant Percentage_Type :=
      Calculate_Percentage(Stats.Chunks_Processed, Stats.Total_Chunks);

   Write_Speed : constant MB_Per_Second_Type :=
      Calculate_MB_Per_Second(Stats.Bytes_Written, Elapsed);
begin
   Put_Line("Read: " & Float(Read_Speed)'Image & " MB/s");
   Put_Line("Processing: " & Float(Process_Progress)'Image & "%");
   Put_Line("Write: " & Float(Write_Speed)'Image & " MB/s");
end Report_Progress;
```

### Adaptive Chunk Sizing

```ada
procedure Configure_Chunk_Size
  (File_Size : SI_Bytes_Type;
   Config : in out Pipeline_Config) is
begin
   -- Use adaptive sizing based on file size
   if File_Size < SIZE_100MB then
      Config.Chunk_Size := SIZE_1MB;    -- Small chunks for small files
   elsif File_Size < SIZE_1GB then
      Config.Chunk_Size := SIZE_16MB;   -- Default chunks
   else
      Config.Chunk_Size := SIZE_64MB;   -- Large chunks for large files
   end if;

   -- Calculate expected chunk count
   Expected_Chunks : constant Natural :=
      Natural(File_Size / SI_Bytes_Type(Config.Chunk_Size.Value));

   Put_Line("File size: " & To_Natural(File_Size / SI_MB)'Image & " MB");
   Put_Line("Chunk size: " & To_Natural(SI_Bytes_Type(Config.Chunk_Size.Value) / SI_MB)'Image & " MB");
   Put_Line("Expected chunks: " & Expected_Chunks'Image);
end Configure_Chunk_Size;
```

## Summary

Strong typing in Pipelib provides:

1. **Compile-time Safety**: Prevents mixing different kinds of values
2. **Self-documenting Code**: Types indicate the purpose of values
3. **Safe Arithmetic**: Operations that preserve type safety
4. **Performance Monitoring**: Built-in calculations for common metrics
5. **Clear Intent**: Explicit conversions show deliberate type changes

By following these patterns, you create code that is both safer and easier to understand, while getting comprehensive performance insights into your pipeline processing.
