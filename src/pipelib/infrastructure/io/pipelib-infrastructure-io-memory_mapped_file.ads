--  =============================================================================
--  Pipelib.Infrastructure.IO.Memory_Mapped_File - Memory-Mapped File Support
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Provides zero-copy access to files through memory mapping for optimal
--  performance with large files.
--  =============================================================================

pragma Ada_2022;

with Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System;
with System.Storage_Elements;
with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Value_Objects.File_Path;

package Pipelib.Infrastructure.IO.Memory_Mapped_File is

   use System.Storage_Elements;
   use Abohlib.Core.Domain.Value_Objects.File_Path;

   --  Memory mapped file type
   type Memory_Mapped_File is new Ada.Finalization.Limited_Controlled with private;
   type Memory_Mapped_File_Access is access all Memory_Mapped_File;

   --  Memory map view for zero-copy access
   type Memory_View is record
      Address : System.Address;
      Size    : Storage_Count;
   end record;

   --  Result type for memory mapping operations
   package Map_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type  => Memory_View,
      Err_Type => Unbounded_String);

   --  Map a file into memory
   function Map_File
     (File : in out Memory_Mapped_File;
      Path : File_Path;
      Read_Only : Boolean := True) return Map_Result.Result
     with Pre => not Is_Mapped (File);
   --  Maps the file at the given path into memory
   --  Returns a Memory_View providing zero-copy access to the file data

   --  Unmap the file from memory
   procedure Unmap (File : in out Memory_Mapped_File)
     with Post => not Is_Mapped (File);
   --  Unmaps the file and releases resources

   --  Check if file is currently mapped
   function Is_Mapped (File : Memory_Mapped_File) return Boolean;

   --  Get the current memory view
   function Get_View (File : Memory_Mapped_File) return Memory_View
     with Pre => Is_Mapped (File);
   --  Returns the memory view for direct access

   --  Get file size
   function Get_Size (File : Memory_Mapped_File) return Storage_Count
     with Pre => Is_Mapped (File);
   --  Returns the size of the mapped file in bytes

   --  Create a subview of the mapped memory
   function Create_Subview
     (File : Memory_Mapped_File;
      Offset : Storage_Count;
      Length : Storage_Count) return Memory_View
     with Pre => Is_Mapped (File) and then
                 Offset < Get_Size (File) and then
                 Offset + Length <= Get_Size (File);
   --  Creates a view into a portion of the mapped file

   --  Sync changes to disk (for writable mappings)
   procedure Sync (File : in out Memory_Mapped_File)
     with Pre => Is_Mapped (File);
   --  Forces any changes to be written to disk

   --  Advise the kernel about access patterns
   type Access_Pattern is (Sequential, Random, Will_Need, Dont_Need);

   procedure Advise
     (File : Memory_Mapped_File;
      Pattern : Access_Pattern;
      Offset : Storage_Count := 0;
      Length : Storage_Count := 0)
     with Pre => Is_Mapped (File);
   --  Provides hints to the OS about expected access patterns

   --  Check if memory mapping is available on this platform
   function Is_Memory_Mapping_Available return Boolean;

   --  Determine if a file should be memory mapped based on size
   function Should_Use_Memory_Map
     (File_Size : Long_Long_Integer;
      Threshold : Long_Long_Integer := 100 * 1024 * 1024) return Boolean;
   --  Returns true if file size exceeds threshold (default 100MB)

private
   type Platform_Handle is new Natural;
   Null_Handle : constant Platform_Handle := 0;

   type Memory_Mapped_File is new Ada.Finalization.Limited_Controlled with record
      Handle      : Platform_Handle := Null_Handle;
      Map_Address : System.Address := System.Null_Address;
      Map_Size    : Storage_Count := 0;
      Read_Only   : Boolean := True;
      File_Path   : Abohlib.Core.Domain.Value_Objects.File_Path.File_Path;
   end record;

   overriding
   procedure Finalize (File : in out Memory_Mapped_File);
   --  Ensures file is unmapped when object is destroyed

end Pipelib.Infrastructure.IO.Memory_Mapped_File;
