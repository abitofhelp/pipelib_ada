--   =============================================================================
--   Pipelib.Infrastructure.IO.Memory_Mapped_File - Implementation
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;

with Interfaces.C;
with Interfaces.C.Strings;
with System.Storage_Elements;
with Ada.Directories;
with Ada.Unchecked_Conversion;
with Abohlib.Core.Domain.Constants.Bytes;

package body Pipelib.Infrastructure.IO.Memory_Mapped_File is

   use Interfaces.C;
   use Abohlib.Core.Domain.Value_Objects.File_Path;
   use System;

   --  Local constants
   SI_MB : constant := Abohlib.Core.Domain.Constants.Bytes.SI_MB;
   pragma Unreferenced (SI_MB);  -- Available for future use

   --  POSIX system calls

   --  File open flags
   O_RDONLY : constant := 0;
   O_RDWR   : constant := 2;

   --  Memory protection flags
   PROT_READ  : constant := 1;
   PROT_WRITE : constant := 2;

   --  Memory mapping flags
   MAP_SHARED  : constant := 1;
   MAP_PRIVATE : constant := 2;
   pragma Unreferenced (MAP_PRIVATE);  -- Available for future use

   --  Type for file offset
   subtype off_t is Interfaces.C.long;

   function c_open
     (pathname : Interfaces.C.Strings.chars_ptr;
      flags    : int) return int
     with Import, Convention => C, External_Name => "open";

   function c_close (fd : int) return int
     with Import, Convention => C, External_Name => "close";

   function mmap
     (addr   : System.Address;
      length : size_t;
      prot   : int;
      flags  : int;
      fd     : int;
      offset : off_t) return System.Address
     with Import, Convention => C, External_Name => "mmap";

   function munmap
     (addr   : System.Address;
      length : size_t) return int
     with Import, Convention => C, External_Name => "munmap";

   function msync
     (addr   : System.Address;
      length : size_t;
      flags  : int) return int
     with Import, Convention => C, External_Name => "msync";

   --  MAP_FAILED constant
   MAP_FAILED : constant System.Address := System.Storage_Elements.To_Address
     (System.Storage_Elements.Integer_Address'Last);

   -- --------------
   --  Map_File
   -- --------------

   function Map_File
     (File : in out Memory_Mapped_File_Type;
      Path : Abohlib.Core.Domain.Value_Objects.File_Path.File_Path;
      Mode : Map_Mode := Read_Only) return Map_Result.Result
   is
      use Map_Result;
      Path_String : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (To_String (Path));
      Open_Flags : constant int := (if Mode = Read_Only then O_RDONLY else O_RDWR);
      Prot_Flags : constant int := (if Mode = Read_Only then PROT_READ else PROT_READ + PROT_WRITE);
   begin
      --  Check if file exists
      if not Ada.Directories.Exists (To_String (Path)) then
         Interfaces.C.Strings.Free (Path_String);
         return Err (To_Unbounded_String ("File does not exist: " & To_String (Path)));
      end if;

      --  Get file size
      File.File_Size := Long_Long_Integer (Ada.Directories.Size (To_String (Path)));

      --  Open file
      File.File_Descriptor := Integer (c_open (Path_String, Open_Flags));
      Interfaces.C.Strings.Free (Path_String);

      if File.File_Descriptor < 0 then
         return Err (To_Unbounded_String ("Failed to open file"));
      end if;

      --  Map file into memory
      File.Map_Address := mmap
        (addr   => System.Null_Address,
         length => size_t (File.File_Size),
         prot   => Prot_Flags,
         flags  => MAP_SHARED,
         fd     => int (File.File_Descriptor),
         offset => 0);

      if File.Map_Address = MAP_FAILED then
         declare
            Dummy : int;
         begin
            Dummy := c_close (int (File.File_Descriptor));
         end;
         File.File_Descriptor := -1;
         return Err (To_Unbounded_String ("Failed to map file into memory"));
      end if;

      File.Is_Mapped_Flag := True;
      File.Mode := Mode;

      return Ok (True);
   end Map_File;

   -- ---------
   --  Unmap
   -- ---------

   procedure Unmap (File : in out Memory_Mapped_File_Type) is
      Dummy : int;
   begin
      if File.Is_Mapped_Flag then
         --  Unmap memory
         if File.Map_Address /= Null_Address then
            Dummy := munmap (File.Map_Address, size_t (File.File_Size));
         end if;

         --  Close file descriptor
         if File.File_Descriptor >= 0 then
            Dummy := c_close (int (File.File_Descriptor));
         end if;

         File.Is_Mapped_Flag := False;
         File.Map_Address := System.Null_Address;
         File.File_Descriptor := -1;
         File.File_Size := 0;
      end if;
   end Unmap;

   -- -------------
   --  Is_Mapped
   -- -------------

   function Is_Mapped (File : Memory_Mapped_File_Type) return Boolean is
   begin
      return File.Is_Mapped_Flag;
   end Is_Mapped;

   -- --------
   --  Size
   -- --------

   function Size (File : Memory_Mapped_File_Type) return Long_Long_Integer is
   begin
      return File.File_Size;
   end Size;

   -- --------
   --  Read
   -- --------

   function Read
     (File : Memory_Mapped_File_Type;
      Offset : Long_Long_Integer;
      Length : Natural) return Stream_Element_Array
   is
      subtype Result_Array is Stream_Element_Array (1 .. Stream_Element_Offset (Length));
      type Result_Array_Access is access all Result_Array;

      function To_Result_Array_Access is new Ada.Unchecked_Conversion
        (System.Address, Result_Array_Access);

      Start_Address : constant System.Address :=
        System.Storage_Elements."+"
          (File.Map_Address,
           System.Storage_Elements.Storage_Offset (Offset));

      Source : constant Result_Array_Access := To_Result_Array_Access (Start_Address);
   begin
      return Source.all;
   end Read;

   --  Get_Access function removed due to Ada access type restrictions
   --  Use Read function for accessing mapped data

   -- ---------
   --  Write
   -- ---------

   procedure Write
     (File : in out Memory_Mapped_File_Type;
      Offset : Long_Long_Integer;
      Data : Stream_Element_Array)
   is
      subtype Target_Array is Stream_Element_Array (1 .. Data'Length);
      type Target_Array_Access is access all Target_Array;

      function To_Target_Array_Access is new Ada.Unchecked_Conversion
        (System.Address, Target_Array_Access);

      Start_Address : constant System.Address :=
        System.Storage_Elements."+"
          (File.Map_Address,
           System.Storage_Elements.Storage_Offset (Offset));

      Target : constant Target_Array_Access := To_Target_Array_Access (Start_Address);
   begin
      Target.all := Data;
   end Write;

   -- --------
   --  Sync
   -- --------

   procedure Sync (File : in out Memory_Mapped_File_Type) is
      MS_SYNC : constant := 4;  -- Synchronous sync
      Dummy : int;
   begin
      if File.Is_Mapped_Flag and File.Mode = Read_Write then
         Dummy := msync (File.Map_Address, size_t (File.File_Size), MS_SYNC);
      end if;
   end Sync;

   -- --------------------------
   --  Should_Use_Memory_Map
   -- --------------------------

   function Should_Use_Memory_Map (File_Size : Long_Long_Integer) return Boolean is
   begin
      --  Use memory mapping for files larger than 100MB
      return File_Size >= Min_Mmap_Size;
   end Should_Use_Memory_Map;

   -- ------------
   --  Finalize
   -- ------------

   overriding procedure Finalize (File : in out Memory_Mapped_File_Type) is
   begin
      Unmap (File);
   end Finalize;

end Pipelib.Infrastructure.IO.Memory_Mapped_File;
