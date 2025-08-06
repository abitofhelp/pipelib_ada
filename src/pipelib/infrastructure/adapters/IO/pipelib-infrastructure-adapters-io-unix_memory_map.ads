--  =============================================================================
--  Pipelib.Infrastructure.Adapters.IO.Unix_Memory_Map - Unix Memory Mapping
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Unix-specific implementation of memory-mapped file operations using
--  mmap system calls. Implements the domain interface for memory mapping.
--  =============================================================================

pragma Ada_2022;

with Ada.Finalization;
with System;
with System.Storage_Elements;
with Abohlib.Core.Domain.Value_Objects.File_Path;
with Abohlib.Core.Domain.Types.Bytes;
with Pipelib.Core.Domain.Ports.Memory_Mapped_File_Interface;

package Pipelib.Infrastructure.Adapters.IO.Unix_Memory_Map is

   use System.Storage_Elements;
   use Abohlib.Core.Domain.Value_Objects.File_Path;
   use Pipelib.Core.Domain.Ports.Memory_Mapped_File_Interface;

   --  Unix-specific memory mapped file implementation
   type Unix_Memory_Map is
     new Ada.Finalization.Limited_Controlled
     and Memory_Mapped_File_Interface with private;
   type Unix_Memory_Map_Access is access all Unix_Memory_Map;

   --  Constructor
   function Create return Unix_Memory_Map_Access;

   --  Map a file into memory
   overriding
   function Map_File
     (Self      : in out Unix_Memory_Map;
      Path      : File_Path;
      Read_Only : Boolean := True) return Map_Result.Result;

   --  Unmap the file from memory
   overriding
   procedure Unmap (Self : in out Unix_Memory_Map);

   --  Check if file is currently mapped
   overriding
   function Is_Mapped (Self : Unix_Memory_Map) return Boolean;

   --  Get the current memory view
   overriding
   function Get_View (Self : Unix_Memory_Map) return Memory_View;

   --  Get file size
   overriding
   function Get_Size (Self : Unix_Memory_Map) return Storage_Count;

   --  Create a subview of the mapped memory
   overriding
   function Create_Subview
     (Self : Unix_Memory_Map; Offset : Storage_Count; Length : Storage_Count)
      return Memory_View;

   --  Sync changes to disk (for writable mappings)
   overriding
   procedure Sync (Self : in out Unix_Memory_Map);

   --  Advise about access patterns
   overriding
   procedure Advise
     (Self    : Unix_Memory_Map;
      Pattern : Access_Pattern;
      Offset  : Storage_Count := 0;
      Length  : Storage_Count := 0);

   --  Check if memory mapping is available on this platform
   function Is_Memory_Mapping_Available return Boolean;

   --  Determine if a file should be memory mapped based on size
   function Should_Use_Memory_Map
     (File_Size : Abohlib.Core.Domain.Types.Bytes.File_Size_Type;
      Threshold : Abohlib.Core.Domain.Types.Bytes.File_Size_Type := 100_000_000) return Boolean;

private
   type Platform_Handle is new Natural;
   Null_Handle : constant Platform_Handle := 0;

   type Unix_Memory_Map is
     new Ada.Finalization.Limited_Controlled
     and Memory_Mapped_File_Interface
   with record
      Handle      : Platform_Handle := Null_Handle;
      Map_Address : System.Address := System.Null_Address;
      Map_Size    : Storage_Count := 0;
      Read_Only   : Boolean := True;
      File_Path   : Abohlib.Core.Domain.Value_Objects.File_Path.File_Path;
   end record;

   overriding
   procedure Finalize (Self : in out Unix_Memory_Map);

end Pipelib.Infrastructure.Adapters.IO.Unix_Memory_Map;
