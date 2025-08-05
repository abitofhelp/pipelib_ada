pragma Ada_2022;

with System.Storage_Elements;

package Pipelib.Core.Domain.Constants is
   pragma Preelaborate;

   use System.Storage_Elements;

   --  =====================================================================
   --  Type Definitions for Type Safety
   --  =====================================================================

   --  Type for byte counts and file sizes (compatible with Storage_Count)
   type Byte_Count_Type is new Storage_Count;

   --  Type for worker count values (distinct from value object's Worker_Count_Type)
   type Worker_Count_Value_Type is range 1 .. 256;

   --  Type for queue depths
   type Queue_Depth_Type is new Positive;

   --  Type for display field widths
   type Field_Width_Type is new Positive range 1 .. 100;

   --  Type for numeric thresholds used in formatting
   type Numeric_Threshold_Type is new Natural;

   --  Type for worker multipliers
   type Worker_Multiplier_Type is new Positive range 1 .. 64;

   --  =====================================================================
   --  Pipeline-Specific Progress Types (prevent mixing different progress counts)
   --  =====================================================================

   --  Distinct types for different kinds of progress to prevent accidental mixing
   type Read_Count_Type is new Natural;        -- Chunks read from input
   type Processed_Count_Type is new Natural;   -- Chunks processed/transformed
   type Written_Count_Type is new Natural;     -- Chunks written to output
   type Error_Count_Type is new Natural;       -- Errors encountered during processing

   --  General types (duplicated here to avoid preelaboration dependency issues)
   type Processing_Time_Ms_Type is new Natural;
   type File_Position_Type is new Long_Long_Integer range 0 .. Long_Long_Integer'Last;
   type Sequence_Number_Type is new Natural;
   type Chunk_Count_Type is new Natural;
   type Chunk_Index_Type is new Positive;    -- 1-based chunk indexing for user display
   type Throughput_MBps_Type is new Float range 0.0 .. Float'Last;

   --  =====================================================================
   --  Base Units (using Byte_Count_Type for type safety)
   --  =====================================================================

   --  File Size Units (Note: Cannot use abohlib byte constants due to categorization)
   --  These values match abohlib SI units: KB=1000, MB=1000000, GB=1000000000
   KB : constant Byte_Count_Type := 1_000;
   MB : constant Byte_Count_Type := 1_000_000;
   GB : constant Byte_Count_Type := 1_000_000_000;

   --  =====================================================================
   --  File Size Thresholds
   --  =====================================================================

   Small_File_Threshold  : constant Byte_Count_Type := 10 * MB;     -- 10MB
   Medium_File_Threshold : constant Byte_Count_Type := 100 * MB;    -- 100MB
   Large_File_Threshold  : constant Byte_Count_Type := GB;          -- 1GB

   --  =====================================================================
   --  Chunk Size Constants
   --  =====================================================================

   Small_Chunk_Size  : constant Byte_Count_Type := 64 * KB;         -- 64KB
   Medium_Chunk_Size : constant Byte_Count_Type := MB;              -- 1MB
   Large_Chunk_Size  : constant Byte_Count_Type := 4 * MB;         -- 4MB
   Huge_Chunk_Size   : constant Byte_Count_Type := 16 * MB;        -- 16MB

   --  Default and limit values for chunk sizes
   Default_Chunk_Size : constant Byte_Count_Type := Small_Chunk_Size;  -- 64KB
   Min_Chunk_Size     : constant Byte_Count_Type := KB;               -- 1KB
   Max_Chunk_Size     : constant Byte_Count_Type := 512 * MB;        -- 512MB

   --  =====================================================================
   --  Memory Map Size Limits
   --  =====================================================================

   Min_Memory_Map_Size : constant Byte_Count_Type := 100 * MB;      -- 100MB
   Max_Memory_Map_Size : constant Byte_Count_Type := GB;            -- 1GB

   --  =====================================================================
   --  Worker Count Limits
   --  =====================================================================

   Max_Worker_Count       : constant Worker_Count_Value_Type := 256;
   Default_Worker_Count   : constant Worker_Count_Value_Type := 4;
   Max_Worker_Count_Range : constant Worker_Count_Value_Type := 64;  -- For DTOs

   --  Worker multiplier for CPU-based calculations
   Max_Worker_Multiplier  : constant Worker_Multiplier_Type := 32;

   --  =====================================================================
   --  Pipeline Configuration Defaults
   --  =====================================================================

   Default_Max_Queue_Depth : constant Queue_Depth_Type := 100;

   --  =====================================================================
   --  Progress Display Formatting
   --  =====================================================================

   Progress_Field_Width : constant Field_Width_Type := 4;

   --  Thresholds for number formatting
   Progress_Threshold_Ten      : constant Numeric_Threshold_Type := 10;
   Progress_Threshold_Hundred  : constant Numeric_Threshold_Type := 100;
   Progress_Threshold_Thousand : constant Numeric_Threshold_Type := 1_000;

   --  =====================================================================
   --  UUID Generation Constants
   --  =====================================================================

   UUID_Template     : constant String := "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx";
   UUID_Hex_Chars    : constant String := "0123456789abcdef";
   UUID_X_Char       : constant Character := 'x';
   UUID_Y_Char       : constant Character := 'y';
   UUID_Version_Char : constant Character := '4';

   --  =====================================================================
   --  ANSI Terminal Control
   --  =====================================================================

   ANSI_Escape_Char : constant Character := Character'Val (27);
   ANSI_Green       : constant String := "[32m";
   ANSI_Reset       : constant String := "[0m";
   ANSI_OK_Marker   : constant String := "[OK]";

   --  =====================================================================
   --  Conversion Functions (for interfacing with non-typed code)
   --  =====================================================================

   function To_Natural (Value : Byte_Count_Type) return Natural is
      (Natural (Value));

   function To_Natural (Value : Worker_Count_Value_Type) return Natural is
      (Natural (Value));

   function To_Natural (Value : Queue_Depth_Type) return Natural is
      (Natural (Value));

   function To_Natural (Value : Numeric_Threshold_Type) return Natural is
      (Natural (Value));

   --  Progress type conversions
   function To_Natural (Value : Read_Count_Type) return Natural is
      (Natural (Value));

   function To_Natural (Value : Processed_Count_Type) return Natural is
      (Natural (Value));

   function To_Natural (Value : Written_Count_Type) return Natural is
      (Natural (Value));

   function To_Natural (Value : Error_Count_Type) return Natural is
      (Natural (Value));

   function To_Storage_Count (Value : Byte_Count_Type) return Storage_Count is
      (Storage_Count (Value));

end Pipelib.Core.Domain.Constants;
