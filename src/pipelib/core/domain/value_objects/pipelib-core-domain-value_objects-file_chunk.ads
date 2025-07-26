--   =============================================================================
--   Pipelib.Core.Domain.Value_Objects.File_Chunk - File Chunk Value Object
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Immutable chunk of data for processing within a pipeline system.
--   Follows Domain-Driven Design principles and ensures data integrity.
--   =============================================================================

pragma Ada_2022;

with Ada.Streams;           use Ada.Streams;
with Ada.Calendar;
with Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Pipelib.Core.Domain.Value_Objects.Chunk_Size;
with Abohlib.Core.Domain.Constants.Bytes;

package Pipelib.Core.Domain.Value_Objects.File_Chunk is

   --  Forward declaration
   type File_Chunk_Type is new Ada.Finalization.Controlled with private;

   --  Maximum chunk data size (512MB max)
   MAX_CHUNK_DATA_SIZE : constant :=
     512 * Abohlib.Core.Domain.Constants.Bytes.SI_MB;

   --  Constructors
   function Create
     (Sequence_Number : Natural;
      Offset          : Long_Long_Integer;
      Data            : Stream_Element_Array;
      Is_Final        : Boolean) return File_Chunk_Type
   with Pre => Data'Length > 0 and then Data'Length <= MAX_CHUNK_DATA_SIZE;
   --  Creates a new file chunk with ULID identifier

   function Create_With_Checksum
     (Sequence_Number : Natural;
      Offset          : Long_Long_Integer;
      Data            : Stream_Element_Array;
      Checksum        : String;
      Is_Final        : Boolean) return File_Chunk_Type
   with
     Pre =>
       Data'Length > 0
       and then Data'Length <= MAX_CHUNK_DATA_SIZE
       and then Checksum'Length = 64;  -- SHA-256 hex string
   --  Creates a new file chunk with checksum and ULID identifier

   --  Builder methods (create new instances) with contracts
   function With_Checksum
     (Chunk : File_Chunk_Type; Checksum : String) return File_Chunk_Type
   with
     Pre => Checksum'Length = 64,  -- SHA-256 hex string
     Post =>
       With_Checksum'Result.Has_Checksum
       and then With_Checksum'Result.Checksum = Checksum
       and then With_Checksum'Result.Sequence_Number = Chunk.Sequence_Number;
   --  Returns new chunk with checksum added

   function Calculate_And_Set_Checksum
     (Chunk : File_Chunk_Type) return File_Chunk_Type
   with
     Post =>
       Calculate_And_Set_Checksum'Result.Has_Checksum
       and then Calculate_And_Set_Checksum'Result.Checksum'Length = 64;
   --  Returns new chunk with calculated SHA-256 checksum

   --  Accessors
   function Id (Chunk : File_Chunk_Type) return String
   with Inline;

   function Sequence_Number (Chunk : File_Chunk_Type) return Natural
   with Inline;

   function Offset (Chunk : File_Chunk_Type) return Long_Long_Integer
   with Inline;

   function Size (Chunk : File_Chunk_Type) return Chunk_Size.Chunk_Size_Type
   with Inline;

   function Data (Chunk : File_Chunk_Type) return Stream_Element_Array
   with Inline;

   function Checksum (Chunk : File_Chunk_Type) return String;
   --  Returns empty string if no checksum

   function Has_Checksum (Chunk : File_Chunk_Type) return Boolean
   with Inline;

   function Is_Final (Chunk : File_Chunk_Type) return Boolean
   with Inline;

   function Created_At (Chunk : File_Chunk_Type) return Ada.Calendar.Time
   with Inline;

   function Data_Length (Chunk : File_Chunk_Type) return Natural
   with Inline;

   function Is_Empty (Chunk : File_Chunk_Type) return Boolean
   with Inline;

   --  Validation
   function Verify_Checksum (Chunk : File_Chunk_Type) return Boolean;
   --  Verifies checksum if present, returns True if no checksum

   function Calculate_Checksum (Data : Stream_Element_Array) return String;
   --  Calculates SHA-256 checksum of data

   --  Comparison
   overriding
   function "=" (Left, Right : File_Chunk_Type) return Boolean;

   --  String representation
   function Image (Chunk : File_Chunk_Type) return String;

   --  Exceptions
   Invalid_Chunk : exception;

private
   --  Access type for chunk data to allow heap allocation
   type Stream_Element_Array_Access is access Stream_Element_Array;

   type File_Chunk_Type is new Ada.Finalization.Controlled with record
      Id              : Unbounded_String;        -- ULID as string
      Sequence_Number : Natural;
      Offset          : Long_Long_Integer;
      Size            : Chunk_Size.Chunk_Size_Type;
      Data            : Stream_Element_Array_Access;
      Checksum        : Unbounded_String;        -- Empty if no checksum
      Is_Final        : Boolean;
      Created_At      : Ada.Calendar.Time;
   end record;

   --  Controlled type operations
   overriding
   procedure Initialize (Object : in out File_Chunk_Type);
   overriding
   procedure Adjust (Object : in out File_Chunk_Type);
   overriding
   procedure Finalize (Object : in out File_Chunk_Type);

end Pipelib.Core.Domain.Value_Objects.File_Chunk;
