--  =============================================================================
--  Pipelib.Infrastructure.Adapters.IO.Unix_Memory_Map - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Directories;
with Ada.Strings.Unbounded;
with Interfaces.C;

package body Pipelib.Infrastructure.Adapters.IO.Unix_Memory_Map is

   use Ada.Strings.Unbounded;

   use type Interfaces.C.int;
   use type Interfaces.C.size_t;
   use type System.Address;

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

   --  Note: We previously used fstat to get file sizes, but encountered issues
   --  with the platform-specific struct stat layout on macOS. The st_size field
   --  at offset 96 would sometimes read as 0 for files >= 100MB.
   --  We now use Ada.Directories.Size which is more portable and reliable.

   --  Constructor
   function Create return Unix_Memory_Map_Access is
   begin
      return new Unix_Memory_Map;
   end Create;

   --  Map a file into memory
   overriding
   function Map_File
     (Self      : in out Unix_Memory_Map;
      Path      : File_Path;
      Read_Only : Boolean := True) return Map_Result.Result
   is

      Path_String : constant String := To_String (Path);
      C_Path      : constant Interfaces.C.char_array :=
        Interfaces.C.To_C (Path_String);
      FD          : Interfaces.C.int;
      Map_Addr    : System.Address;
      File_Size   : Storage_Count;

   begin
      --  Check if already mapped
      if Self.Is_Mapped then
         return Map_Result.Err (To_Unbounded_String ("File already mapped"));
      end if;

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

      --  Get file size using Ada.Directories
      --  This avoids platform-specific struct stat issues
      File_Size := Storage_Count (Ada.Directories.Size (Path_String));

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
      Self.Handle := Null_Handle;  -- FD was closed after mmap
      Self.Map_Address := Map_Addr;
      Self.Map_Size := File_Size;
      Self.Read_Only := Read_Only;
      Self.File_Path := Path;

      --  Return memory view for zero-copy access
      declare
         View : constant Memory_View :=
           (Address => Map_Addr, Size => File_Size);
      begin
         return Map_Result.Ok (View);
      end;
   end Map_File;

   --  Unmap the file from memory
   overriding
   procedure Unmap (Self : in out Unix_Memory_Map) is
      Result : Interfaces.C.int;
      pragma Unreferenced (Result);
   begin
      if Self.Map_Address /= System.Null_Address then
         Result :=
           C_Munmap (Self.Map_Address, Interfaces.C.size_t (Self.Map_Size));
         Self.Map_Address := System.Null_Address;
         Self.Map_Size := 0;
         Self.Handle := Null_Handle;
      end if;
   end Unmap;

   --  Check if file is currently mapped
   overriding
   function Is_Mapped (Self : Unix_Memory_Map) return Boolean is
   begin
      return Self.Map_Address /= System.Null_Address and Self.Map_Size > 0;
   end Is_Mapped;

   --  Get the current memory view
   overriding
   function Get_View (Self : Unix_Memory_Map) return Memory_View is
   begin
      return (Address => Self.Map_Address, Size => Self.Map_Size);
   end Get_View;

   --  Get file size
   overriding
   function Get_Size (Self : Unix_Memory_Map) return Storage_Count is
   begin
      return Self.Map_Size;
   end Get_Size;

   --  Create a subview of the mapped memory
   overriding
   function Create_Subview
     (Self : Unix_Memory_Map; Offset : Storage_Count; Length : Storage_Count)
      return Memory_View
   is

      Subview_Address : constant System.Address := Self.Map_Address + Offset;
   begin
      return (Address => Subview_Address, Size => Length);
   end Create_Subview;

   --  Sync changes to disk (for writable mappings)
   overriding
   procedure Sync (Self : in out Unix_Memory_Map) is
      Result : Interfaces.C.int;
      pragma Unreferenced (Result);
   begin
      if Self.Map_Address /= System.Null_Address then
         Result :=
           C_Msync (Self.Map_Address, Interfaces.C.size_t (Self.Map_Size), 0);
      end if;
   end Sync;

   --  Advise the kernel about access patterns
   overriding
   procedure Advise
     (Self    : Unix_Memory_Map;
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
      if Self.Map_Address = System.Null_Address then
         return;
      end if;

      --  Determine address and length for advice
      if Length = 0 then
         Advice_Address := Self.Map_Address;
         Advice_Length := Interfaces.C.size_t (Self.Map_Size);
      else
         Advice_Address := Self.Map_Address + Offset;
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
      Threshold : Long_Long_Integer := 100 * 1024 * 1024) return Boolean is
   begin
      return File_Size >= Threshold and then Is_Memory_Mapping_Available;
   end Should_Use_Memory_Map;

   --  Finalization
   overriding
   procedure Finalize (Self : in out Unix_Memory_Map) is
   begin
      if Self.Is_Mapped then
         Self.Unmap;
      end if;
   end Finalize;

end Pipelib.Infrastructure.Adapters.IO.Unix_Memory_Map;
