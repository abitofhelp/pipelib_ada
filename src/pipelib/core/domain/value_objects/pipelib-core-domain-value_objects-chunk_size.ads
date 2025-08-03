--  =============================================================================
--  Pipelib.Core.Domain.Value_Objects.Chunk_Size - Chunk Size Value Object
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Represents the size of data chunks for pipeline processing.
--  Provides common chunk sizes and validation.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Constants.Bytes;

package Pipelib.Core.Domain.Value_Objects.Chunk_Size is

   use Abohlib.Core.Domain.Constants.Bytes;

   --  Chunk size bounds (1KB to 1GB)
   MIN_CHUNK_SIZE     : constant Long_Long_Integer := SI_KB;
   MAX_CHUNK_SIZE     : constant Long_Long_Integer := SI_GB;
   DEFAULT_CHUNK_SIZE : constant Long_Long_Integer := 16 * SI_MB;

   --  Common chunk sizes
   SIZE_1KB : constant := 1 * SI_KB;
   SIZE_4KB : constant := 4 * SI_KB;
   SIZE_64KB : constant := 64 * SI_KB;
   SIZE_256KB : constant := 256 * SI_KB;
   SIZE_1MB : constant := 1 * SI_MB;
   SIZE_4MB : constant := 4 * SI_MB;
   SIZE_8MB : constant := 8 * SI_MB;
   SIZE_16MB : constant := 16 * SI_MB;
   SIZE_32MB : constant := 32 * SI_MB;
   SIZE_64MB : constant := 64 * SI_MB;
   SIZE_128MB : constant := 128 * SI_MB;
   SIZE_256MB : constant := 256 * SI_MB;
   SIZE_512MB : constant := 512 * SI_MB;

   --  Chunk size type
   type Chunk_Size_Type is private
   with Type_Invariant => Is_Valid (Chunk_Size_Type);

   --  Constructor with validation
   function Create (Bytes : Long_Long_Integer) return Chunk_Size_Type
   with
     Pre => Bytes >= MIN_CHUNK_SIZE and Bytes <= MAX_CHUNK_SIZE,
     Post => Value (Create'Result) = Bytes and Is_Valid (Create'Result);

   --  Get the value in bytes
   function Value (Size : Chunk_Size_Type) return Long_Long_Integer
   with
     Pre => Is_Valid (Size),
     Post => Value'Result >= MIN_CHUNK_SIZE and Value'Result <= MAX_CHUNK_SIZE,
     Inline;

   --  Validation
   function Is_Valid (Size : Chunk_Size_Type) return Boolean
   with Inline;

   --  Factory methods
   function Default return Chunk_Size_Type
   with
     Post =>
       Value (Default'Result) = DEFAULT_CHUNK_SIZE
       and Is_Valid (Default'Result);

   function Min return Chunk_Size_Type
   with Post => Value (Min'Result) = MIN_CHUNK_SIZE and Is_Valid (Min'Result);

   function Max return Chunk_Size_Type
   with Post => Value (Max'Result) = MAX_CHUNK_SIZE and Is_Valid (Max'Result);

   --  Convenience constructors
   function From_KB (KB : Natural) return Chunk_Size_Type
   with
     Pre =>
       Long_Long_Integer (KB) * SI_KB >= MIN_CHUNK_SIZE
       and Long_Long_Integer (KB) * SI_KB <= MAX_CHUNK_SIZE,
     Post =>
       Value (From_KB'Result) = Long_Long_Integer (KB) * SI_KB
       and Is_Valid (From_KB'Result);

   function From_MB (MB : Natural) return Chunk_Size_Type
   with
     Pre =>
       Long_Long_Integer (MB) * SI_MB >= MIN_CHUNK_SIZE
       and Long_Long_Integer (MB) * SI_MB <= MAX_CHUNK_SIZE,
     Post =>
       Value (From_MB'Result) = Long_Long_Integer (MB) * SI_MB
       and Is_Valid (From_MB'Result);

   --  Named size constructors
   function Small return Chunk_Size_Type   -- 1MB
   with Post => Value (Small'Result) = SIZE_1MB and Is_Valid (Small'Result);

   function Medium return Chunk_Size_Type  -- 16MB (default)
   with Post => Value (Medium'Result) = SIZE_16MB and Is_Valid (Medium'Result);

   function Large return Chunk_Size_Type   -- 64MB
   with Post => Value (Large'Result) = SIZE_64MB and Is_Valid (Large'Result);

   --  Adaptive chunk size based on total size
   function Adaptive_For_Size
     (Total_Size : Long_Long_Integer) return Chunk_Size_Type
   with
     Pre => Total_Size > 0,
     Post =>
       Is_Valid (Adaptive_For_Size'Result)
       and Value (Adaptive_For_Size'Result) >= MIN_CHUNK_SIZE
       and Value (Adaptive_For_Size'Result) <= MAX_CHUNK_SIZE;

private

   type Chunk_Size_Type is record
      Bytes : Long_Long_Integer range MIN_CHUNK_SIZE .. MAX_CHUNK_SIZE :=
        DEFAULT_CHUNK_SIZE;
   end record;

   function Value (Size : Chunk_Size_Type) return Long_Long_Integer
   is (Size.Bytes);

   pragma
     Warnings (Off, "condition can only be False if invalid values present");
   function Is_Valid (Size : Chunk_Size_Type) return Boolean
   is (Size.Bytes >= MIN_CHUNK_SIZE and Size.Bytes <= MAX_CHUNK_SIZE);
   pragma
     Warnings (On, "condition can only be False if invalid values present");

end Pipelib.Core.Domain.Value_Objects.Chunk_Size;
