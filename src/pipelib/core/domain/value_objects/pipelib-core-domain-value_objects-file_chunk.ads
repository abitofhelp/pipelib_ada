--   =============================================================================
--   Pipelib.Core.Domain.Value_Objects.File_Chunk - File Chunk Value Object
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Immutable chunk of data for processing within a pipeline system.
--   Follows Domain-Driven Design principles and ensures data integrity.
--
--   MEMORY MANAGEMENT WITH ADA 2022 OWNERSHIP:
--   =========================================
--   This type uses Ada 2022 ownership features for safe memory management:
--
--   1. OWNERSHIP MODEL:
--      - Each File_Chunk_Type owns its Data (Stream_Element_Array)
--      - Ownership is exclusive - no sharing of data between chunks
--      - Memory is automatically freed when chunk goes out of scope
--
--   2. COPY SEMANTICS:
--      - Assignment creates a deep copy (implemented in Adjust)
--      - Each copy owns its own data independently
--      - No risk of double-free or use-after-free
--
--   3. PERFORMANCE CONSIDERATIONS:
--      - Chunks can be large (up to 512MB)
--      - Deep copying is expensive for large chunks
--      - Use 'renames' or 'access' parameters to avoid copies
--      - Consider using 'Move' semantics when ownership transfer is intended
--
--   4. BEST PRACTICES:
--      - Prefer passing chunks as 'in' parameters (no copy)
--      - Use 'access' parameters when mutation is needed
--      - Avoid unnecessary assignments
--      - Let chunks go out of scope naturally for cleanup
--
--   5. TASK SAFETY:
--      - Read operations are task-safe
--      - Write operations require external synchronization
--      - Each task should own its chunks independently
--
--   6. USAGE EXAMPLES:
--      -- Efficient parameter passing (no copy):
--      procedure Process (Chunk : in File_Chunk_Type);
--
--      -- When mutation is needed (still no copy):
--      procedure Update (Chunk : access File_Chunk_Type);
--
--      -- Efficient ownership transfer:
--      declare
--         Source : File_Chunk_Type := Create (...);
--         Target : File_Chunk_Type;
--      begin
--         Move (Target, Source);  -- Source now has null data
--      end;
--
--      -- Avoid expensive copies:
--      Chunk_Ref : File_Chunk_Type renames Some_Chunk;  -- No copy
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
   --  Note: We keep this as a non-limited controlled type to support
   --  value semantics needed by pipeline stages. However, users should
   --  be aware that assignment causes deep copying of potentially large data.
   type File_Chunk_Type is new Ada.Finalization.Controlled with private;

   --  Access type for stream element arrays
   type Stream_Element_Array_Access is access Stream_Element_Array;

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

   function Create_From_Access
     (Sequence_Number : Natural;
      Offset          : Long_Long_Integer;
      Data            : not null Stream_Element_Array_Access;
      Is_Final        : Boolean) return File_Chunk_Type
   with Pre => Data.all'Length > 0 and then Data.all'Length <= MAX_CHUNK_DATA_SIZE;
   --  Creates a new file chunk taking ownership of the provided data
   --  The caller must NOT deallocate the Data after this call

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

   function Data_Access (Chunk : File_Chunk_Type) return not null Stream_Element_Array_Access
   with Inline;
   --  Returns direct access to the chunk data without copying

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

   --  Move operation for efficient ownership transfer (Ada 2022)
   --  Transfers ownership of data from Source to Target without copying
   --  After this operation, Source will have null data
   procedure Move (Target : out File_Chunk_Type; Source : in out File_Chunk_Type)
     with Post => Source.Is_Empty;

   --  Exceptions
   Invalid_Chunk : exception;

private
   --  Access type moved to public part for Create_From_Access function

   --  File_Chunk_Type uses Ada 2022 ownership for memory management
   --  IMPORTANT: Memory Management Guidelines:
   --  1. The Data field owns the allocated memory
   --  2. Assignment creates a new copy (deep copy in Adjust)
   --  3. Memory is automatically freed when chunk goes out of scope
   --  4. Use 'Move' aspect when transferring ownership is desired
   --  5. The type is task-safe for read operations only
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
