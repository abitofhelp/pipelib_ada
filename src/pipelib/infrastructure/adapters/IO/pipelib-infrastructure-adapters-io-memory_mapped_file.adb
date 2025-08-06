--  =============================================================================
--  Pipelib.Infrastructure.IO.Memory_Mapped_File - Memory-Mapped File Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Provides zero-copy access to files through memory mapping for optimal
--  performance with large files.
--  =============================================================================

pragma Ada_2022;

with Ada.Directories;
with Interfaces.C;

package body Pipelib.Infrastructure.Adapters.IO.Memory_Mapped_File is

   use type Interfaces.C.int;
   use type Interfaces.C.size_t;

   --  Platform-specific constants
   O_RDONLY : constant Interfaces.C.int := 0;
   O_RDWR   : constant Interfaces.C.int := 2;

   PROT_READ  : constant Interfaces.C.int := 1;
   PROT_WRITE : constant Interfaces.C.int := 2;

   MAP_SHARED  : constant Interfaces.C.int := 1;
   MAP_PRIVATE : constant Interfaces.C.int := 2;
   MAP_FAILED  : constant System.Address := System.Null_Address;

   MADV_SEQUENTIAL : constant Interfaces.C.int := 2;
   MADV_RANDOM     : constant Interfaces.C.int := 1;
   MADV_WILLNEED   : constant Interfaces.C.int := 3;
   MADV_DONTNEED   : constant Interfaces.C.int := 4;

   --  System call imports
   function C_Open
     (Path : Interfaces.C.char_array; Flags : Interfaces.C.int)
      return Interfaces.C.int
   with Import => True, Convention => C, External_Name => "open";

   function C_Close (FD : Interfaces.C.int) return Interfaces.C.int
   with Import => True, Convention => C, External_Name => "close";

   function C_Mmap
     (Addr   : System.Address;
      Length : Interfaces.C.size_t;
      Prot   : Interfaces.C.int;
      Flags  : Interfaces.C.int;
      FD     : Interfaces.C.int;
      Offset : Interfaces.C.size_t) return System.Address
   with Import => True, Convention => C, External_Name => "mmap";

   function C_Munmap
     (Addr : System.Address; Length : Interfaces.C.size_t)
      return Interfaces.C.int
   with Import => True, Convention => C, External_Name => "munmap";

   function C_Msync
     (Addr   : System.Address;
      Length : Interfaces.C.size_t;
      Flags  : Interfaces.C.int) return Interfaces.C.int
   with Import => True, Convention => C, External_Name => "msync";

   function C_Madvise
     (Addr   : System.Address;
      Length : Interfaces.C.size_t;
      Advice : Interfaces.C.int) return Interfaces.C.int
   with Import => True, Convention => C, External_Name => "madvise";

   type Stat_Buffer is record
      St_Size : Interfaces.C.long;
      --  Other fields omitted for brevity
   end record
   with Convention => C;

   function C_Fstat
     (FD : Interfaces.C.int; Buf : access Stat_Buffer) return Interfaces.C.int
   with Import => True, Convention => C, External_Name => "fstat";

   --  Map a file into memory
   function Map_File
     (File      : in out Memory_Mapped_File;
      Path      : File_Path;
      Read_Only : Boolean := True) return Map_Result.Result
   is

      Path_String : constant String := To_String (Path);
      C_Path      : constant Interfaces.C.char_array :=
        Interfaces.C.To_C (Path_String);
      FD          : Interfaces.C.int;
      Stat_Buf    : aliased Stat_Buffer;
      Map_Addr    : System.Address;
      File_Size   : Storage_Count;

   begin
      --  Check if file exists
      if not Ada.Directories.Exists (Path_String) then
         return
           Map_Result.Err
             (To_Unbounded_String ("File does not exist: " & Path_String));
      end if;

      --  Open file
      FD := C_Open (C_Path, (if Read_Only then O_RDONLY else O_RDWR));
      if FD = -1 then
         return
           Map_Result.Err
             (To_Unbounded_String ("Failed to open file: " & Path_String));
      end if;

      --  Get file size
      if C_Fstat (FD, Stat_Buf'Access) = -1 then
         declare
            Close_Result : constant Interfaces.C.int := C_Close (FD);
            pragma Unreferenced (Close_Result);
         begin
            return
              Map_Result.Err
                (To_Unbounded_String
                   ("Failed to get file size: " & Path_String));
         end;
      end if;

      File_Size := Storage_Count (Stat_Buf.St_Size);

      if File_Size = 0 then
         declare
            Close_Result : constant Interfaces.C.int := C_Close (FD);
            pragma Unreferenced (Close_Result);
         begin
            return
              Map_Result.Err
                (To_Unbounded_String
                   ("Cannot map empty file: " & Path_String));
         end;
      end if;

      --  Create memory mapping
      Map_Addr :=
        C_Mmap
          (Addr   => System.Null_Address,
           Length => Interfaces.C.size_t (File_Size),
           Prot   => (if Read_Only then PROT_READ else PROT_READ + PROT_WRITE),
           Flags  => (if Read_Only then MAP_PRIVATE else MAP_SHARED),
           FD     => FD,
           Offset => 0);

      --  Close file descriptor (no longer needed after mapping)
      declare
         Close_Result : constant Interfaces.C.int := C_Close (FD);
         pragma Unreferenced (Close_Result);
      begin
         null;
      end;

      if Map_Addr = MAP_FAILED then
         return
           Map_Result.Err
             (To_Unbounded_String ("Memory mapping failed: " & Path_String));
      end if;

      --  Store mapping information
      File.Handle := Null_Handle;  -- FD was closed after mmap
      File.Map_Address := Map_Addr;
      File.Map_Size := File_Size;
      File.Read_Only := Read_Only;
      File.File_Path := Path;

      --  Return memory view for zero-copy access
      declare
         View : constant Memory_View_Type :=
           (Address => Map_Addr, Size => File_Size);
      begin
         return Map_Result.Ok (View);
      end;
   end Map_File;

   --  Unmap the file from memory
   procedure Unmap (File : in out Memory_Mapped_File) is
      Result : Interfaces.C.int;
      pragma Unreferenced (Result);
   begin
      if File.Map_Address /= System.Null_Address then
         Result :=
           C_Munmap (File.Map_Address, Interfaces.C.size_t (File.Map_Size));
         File.Map_Address := System.Null_Address;
         File.Map_Size := 0;
         File.Handle := Null_Handle;
      end if;
   end Unmap;

   --  Check if file is currently mapped
   function Is_Mapped (File : Memory_Mapped_File) return Boolean is
   begin
      return File.Map_Address /= System.Null_Address and File.Map_Size > 0;
   end Is_Mapped;

   --  Get the current memory view
   function Get_View (File : Memory_Mapped_File) return Memory_View_Type is
   begin
      return (Address => File.Map_Address, Size => File.Map_Size);
   end Get_View;

   --  Get file size
   function Get_Size (File : Memory_Mapped_File) return Storage_Count is
   begin
      return File.Map_Size;
   end Get_Size;

   --  Create a subview of the mapped memory
   function Create_Subview
     (File   : Memory_Mapped_File;
      Offset : Storage_Count;
      Length : Storage_Count) return Memory_View_Type
   is

      Subview_Address : constant System.Address := File.Map_Address + Offset;
   begin
      return (Address => Subview_Address, Size => Length);
   end Create_Subview;

   --  Sync changes to disk (for writable mappings)
   procedure Sync (File : in out Memory_Mapped_File) is
      Result : Interfaces.C.int;
      pragma Unreferenced (Result);
   begin
      if File.Map_Address /= System.Null_Address then
         Result :=
           C_Msync (File.Map_Address, Interfaces.C.size_t (File.Map_Size), 0);
      end if;
   end Sync;

   --  Advise the kernel about access patterns
   procedure Advise
     (File    : Memory_Mapped_File;
      Pattern : Access_Pattern;
      Offset  : Storage_Count := 0;
      Length  : Storage_Count := 0)
   is

      Advice_Address : System.Address;
      Advice_Length  : Interfaces.C.size_t;
      Advice_Flag    : Interfaces.C.int;
      Result         : Interfaces.C.int;
      pragma Unreferenced (Result);
   begin
      if File.Map_Address = System.Null_Address then
         return;
      end if;

      --  Determine address and length for advice
      if Length = 0 then
         Advice_Address := File.Map_Address;
         Advice_Length := Interfaces.C.size_t (File.Map_Size);
      else
         Advice_Address := File.Map_Address + Offset;
         Advice_Length := Interfaces.C.size_t (Length);
      end if;

      --  Convert access pattern to system flag
      case Pattern is
         when Sequential =>
            Advice_Flag := MADV_SEQUENTIAL;

         when Random =>
            Advice_Flag := MADV_RANDOM;

         when Will_Need =>
            Advice_Flag := MADV_WILLNEED;

         when Dont_Need =>
            Advice_Flag := MADV_DONTNEED;
      end case;

      Result := C_Madvise (Advice_Address, Advice_Length, Advice_Flag);
   end Advise;

   --  Check if memory mapping is available on this platform
   function Is_Memory_Mapping_Available return Boolean is
   begin
      --  Memory mapping is available on Unix-like systems
      --  This could be enhanced to check at runtime
      return True;
   end Is_Memory_Mapping_Available;

   --  Determine if a file should be memory mapped based on size
   function Should_Use_Memory_Map
     (File_Size : Long_Long_Integer;
      Threshold : Long_Long_Integer := Long_Long_Integer(SI_MB * 100)) return Boolean is
   begin
      return File_Size >= Threshold and then Is_Memory_Mapping_Available;
   end Should_Use_Memory_Map;

   --  Finalization
   overriding
   procedure Finalize (File : in out Memory_Mapped_File) is
   begin
      if Is_Mapped (File) then
         Unmap (File);
      end if;
   end Finalize;

end Pipelib.Infrastructure.Adapters.IO.Memory_Mapped_File;
