--   =============================================================================
--   Pipelib.Infrastructure.IO.Memory_Mapped_File - Memory-Mapped File Support
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Provides memory-mapped file access for efficient I/O operations on large files.
--   Uses platform-specific system calls for optimal performance.
--   =============================================================================

pragma Ada_2022;

with Ada.Streams; use Ada.Streams;
with Ada.Finalization;
with Abohlib.Core.Domain.Value_Objects.File_Path;
with Abohlib.Core.Domain.Result;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System;

package Pipelib.Infrastructure.IO.Memory_Mapped_File is

   --  Type representing a memory-mapped file
   type Memory_Mapped_File_Type is new Ada.Finalization.Limited_Controlled with private;

   --  Result types
   package Map_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => Boolean,  -- Success indicator
      Err_Type => Unbounded_String);

   --  Mapping modes
   type Map_Mode is (Read_Only, Read_Write);

   --  Open and map a file into memory
   function Map_File
     (File : in out Memory_Mapped_File_Type;
      Path : Abohlib.Core.Domain.Value_Objects.File_Path.File_Path;
      Mode : Map_Mode := Read_Only) return Map_Result.Result
     with Pre => not Is_Mapped (File);

   --  Unmap the file
   procedure Unmap (File : in out Memory_Mapped_File_Type)
     with Post => not Is_Mapped (File);

   --  Check if file is mapped
   function Is_Mapped (File : Memory_Mapped_File_Type) return Boolean;

   --  Get file size
   function Size (File : Memory_Mapped_File_Type) return Long_Long_Integer
     with Pre => Is_Mapped (File);

   --  Read data from mapped file
   function Read
     (File : Memory_Mapped_File_Type;
      Offset : Long_Long_Integer;
      Length : Natural) return Stream_Element_Array
     with Pre => Is_Mapped (File) and then
                 Offset >= 0 and then
                 Offset + Long_Long_Integer (Length) <= Size (File);

   --  Note: Zero-copy access removed due to Ada access type restrictions
   --  Use Read function which copies data from mapped memory

   --  Write data to mapped file (only for Read_Write mode)
   procedure Write
     (File : in out Memory_Mapped_File_Type;
      Offset : Long_Long_Integer;
      Data : Stream_Element_Array)
     with Pre => Is_Mapped (File) and then
                 Offset >= 0 and then
                 Offset + Data'Length <= Size (File);

   --  Sync changes to disk (for Read_Write mode)
   procedure Sync (File : in out Memory_Mapped_File_Type)
     with Pre => Is_Mapped (File);

   --  Determine if memory mapping is recommended for file size
   function Should_Use_Memory_Map (File_Size : Long_Long_Integer) return Boolean;

   --  Constants
   --  Minimum file size for memory mapping (100MB)
   Min_Mmap_Size : constant Long_Long_Integer := 100 * 1024 * 1024;

private

   type Memory_Mapped_File_Type is new Ada.Finalization.Limited_Controlled with record
      Is_Mapped_Flag : Boolean := False;
      File_Size      : Long_Long_Integer := 0;
      Map_Address    : System.Address := System.Null_Address;
      File_Descriptor: Integer := -1;
      Mode           : Map_Mode := Read_Only;
   end record;

   overriding procedure Finalize (File : in out Memory_Mapped_File_Type);

end Pipelib.Infrastructure.IO.Memory_Mapped_File;
