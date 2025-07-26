--   =============================================================================
--   Pipelib.Core.Domain.Value_Objects.Chunk_Size - Chunk Size Value Object
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Type-safe representation of chunk sizes for pipeline processing.
--   Ensures chunk sizes are within valid bounds and provides convenient
--   methods for working with chunk sizes.
--   =============================================================================

pragma Ada_2022;

with Ada.Finalization;
with Abohlib.Core.Domain.Constants.Bytes;

package Pipelib.Core.Domain.Value_Objects.Chunk_Size is

   use Abohlib.Core.Domain.Constants.Bytes;

   --  Chunk size constraints (using SI units)
   MIN_SIZE : constant := 1;                    -- 1 byte minimum
   MAX_SIZE : constant := 512 * SI_MB;          -- 512MB maximum
   DEFAULT_SIZE : constant := 32 * SI_MB;           -- 32MB default

   --  Type definition for chunk size
   type Chunk_Size_Type is new Ada.Finalization.Controlled with private;

   --  Constructors with contracts
   function Create (Bytes : Natural) return Chunk_Size_Type
   with
     Pre => Bytes >= MIN_SIZE and then Bytes <= MAX_SIZE,
     Post => Create'Result.Bytes = Bytes;

   function Create_From_KB (KB : Natural) return Chunk_Size_Type
   with
     Pre => KB > 0 and then KB * SI_KB <= MAX_SIZE,
     Post => Create_From_KB'Result.Bytes = KB * SI_KB;

   function Create_From_MB (MB : Natural) return Chunk_Size_Type
   with
     Pre => MB > 0 and then MB * SI_MB <= MAX_SIZE,
     Post => Create_From_MB'Result.Bytes = MB * SI_MB;

   function Create_Default return Chunk_Size_Type
   with Post => Create_Default'Result.Bytes = DEFAULT_SIZE;

   --  Accessors
   function Bytes (Size : Chunk_Size_Type) return Natural
   with Inline;

   function As_Bytes (Size : Chunk_Size_Type) return Natural
   with Inline;

   function Kilobytes (Size : Chunk_Size_Type) return Float
   with Inline;

   function Megabytes (Size : Chunk_Size_Type) return Float
   with Inline;

   --  Utility functions
   function Optimal_For_File_Size
     (File_Size : Long_Long_Integer) return Chunk_Size_Type;

   function Chunks_Needed_For_File
     (Size : Chunk_Size_Type; File_Size : Long_Long_Integer)
      return Long_Long_Integer;

   function Is_Optimal_For_File
     (Size : Chunk_Size_Type; File_Size : Long_Long_Integer) return Boolean;

   --  Comparison operators
   overriding
   function "=" (Left, Right : Chunk_Size_Type) return Boolean;
   function "<" (Left, Right : Chunk_Size_Type) return Boolean;
   function "<=" (Left, Right : Chunk_Size_Type) return Boolean;
   function ">" (Left, Right : Chunk_Size_Type) return Boolean;
   function ">=" (Left, Right : Chunk_Size_Type) return Boolean;

   --  String representation
   function Image (Size : Chunk_Size_Type) return String;

   --  Exceptions
   Invalid_Chunk_Size : exception;

private
   type Chunk_Size_Type is new Ada.Finalization.Controlled with record
      Bytes_Value : Natural;
   end record;

end Pipelib.Core.Domain.Value_Objects.Chunk_Size;
