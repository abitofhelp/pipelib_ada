--  =============================================================================
--  Pipelib.Core.Domain.Ports.Memory_Mapped_File_Interface - Domain Interface
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Domain interface for memory-mapped file operations, enabling zero-copy
--  access to large files while maintaining architectural boundaries.
--  =============================================================================

pragma Ada_2022;

with System;
with System.Storage_Elements;
with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Value_Objects.File_Path;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Pipelib.Core.Domain.Ports.Memory_Mapped_File_Interface is

   use System.Storage_Elements;
   use Abohlib.Core.Domain.Value_Objects.File_Path;

   --  Memory view for zero-copy access
   type Memory_View is record
      Address : System.Address;
      Size    : Storage_Count;
   end record;

   --  Result type for memory mapping operations
   package Map_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type  => Memory_View,
      Err_Type => Unbounded_String);

   --  Interface for memory-mapped file operations
   type Memory_Mapped_File_Interface is limited interface;
   type Memory_Mapped_File_Interface_Access is access all Memory_Mapped_File_Interface'Class;

   --  Map a file into memory
   function Map_File
     (Self      : in out Memory_Mapped_File_Interface;
      Path      : File_Path;
      Read_Only : Boolean := True) return Map_Result.Result is abstract;

   --  Unmap the file from memory
   procedure Unmap (Self : in out Memory_Mapped_File_Interface) is abstract;

   --  Check if file is currently mapped
   function Is_Mapped (Self : Memory_Mapped_File_Interface) return Boolean is abstract;

   --  Get the current memory view
   function Get_View (Self : Memory_Mapped_File_Interface) return Memory_View is abstract
     with Pre'Class => Self.Is_Mapped;

   --  Get file size
   function Get_Size (Self : Memory_Mapped_File_Interface) return Storage_Count is abstract
     with Pre'Class => Self.Is_Mapped;

   --  Create a subview of the mapped memory
   function Create_Subview
     (Self   : Memory_Mapped_File_Interface;
      Offset : Storage_Count;
      Length : Storage_Count) return Memory_View is abstract
     with Pre'Class => Self.Is_Mapped and then
                       Offset < Self.Get_Size and then
                       Offset + Length <= Self.Get_Size;

   --  Sync changes to disk (for writable mappings)
   procedure Sync (Self : in out Memory_Mapped_File_Interface) is abstract
     with Pre'Class => Self.Is_Mapped;

   --  Access pattern hints
   type Access_Pattern is (Sequential, Random, Will_Need, Dont_Need);

   --  Advise about access patterns
   procedure Advise
     (Self    : Memory_Mapped_File_Interface;
      Pattern : Access_Pattern;
      Offset  : Storage_Count := 0;
      Length  : Storage_Count := 0) is abstract
     with Pre'Class => Self.Is_Mapped;

end Pipelib.Core.Domain.Ports.Memory_Mapped_File_Interface;
