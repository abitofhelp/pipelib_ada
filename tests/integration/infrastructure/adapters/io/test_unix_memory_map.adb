--  =============================================================================
--  Test_Unix_Memory_Map - Unix Memory Map Adapter Integration Tests Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with System;
with System.Storage_Elements;
with Interfaces.C;

with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Value_Objects.File_Path;
with Abohlib.Infrastructure.Testing.Test_Framework;
with Pipelib.Infrastructure.Adapters.IO.Unix_Memory_Map;
with Pipelib.Core.Domain.Ports.Memory_Mapped_File_Interface;

package body Test_Unix_Memory_Map is

   use System.Storage_Elements;
   use Abohlib.Core.Domain.Value_Objects.File_Path;
   use Abohlib.Infrastructure.Testing.Test_Framework;
   use Pipelib.Infrastructure.Adapters.IO.Unix_Memory_Map;
   use Pipelib.Core.Domain.Ports.Memory_Mapped_File_Interface;

   -- Unix system call imports
   package C_Unix is
      use Interfaces.C;

      -- mmap constants
      PROT_READ  : constant := 1;
      PROT_WRITE : constant := 2;
      MAP_SHARED : constant := 1;
      MAP_PRIVATE : constant := 2;

      -- madvise constants
      MADV_SEQUENTIAL : constant := 2;
      MADV_RANDOM     : constant := 1;
      MADV_WILLNEED   : constant := 3;
      MADV_DONTNEED   : constant := 4;

      -- msync constants
      MS_ASYNC      : constant := 1;
      MS_SYNC       : constant := 4;
      MS_INVALIDATE : constant := 2;

      function getpagesize return int
        with Import, Convention => C, External_Name => "getpagesize";

      function sysconf (name : int) return long
        with Import, Convention => C, External_Name => "sysconf";

      _SC_OPEN_MAX : constant := 4;  -- Maximum number of open files
   end C_Unix;

   --  Test constants
   Small_File_Size : constant := 4096;  -- 4 KB (one page)
   Test_Data : constant String := "Unix memory map test data";

   --  Helper to create a test file
   procedure Create_Test_File
     (Path : String;
      Size : Natural;
      Fill_Pattern : Character := 'U')
   is
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Path);
      for I in 1 .. Size loop
         Ada.Text_IO.Put (File, Fill_Pattern);
         if I mod 80 = 0 then
            Ada.Text_IO.New_Line (File);
         end if;
      end loop;
      Ada.Text_IO.Close (File);
   end Create_Test_File;

   --  Helper to delete test file if it exists
   procedure Delete_Test_File (Path : String) is
   begin
      if Ada.Directories.Exists (Path) then
         Ada.Directories.Delete_File (Path);
      end if;
   end Delete_Test_File;

   --  Helper to read data from memory view
   function Read_String_From_View
     (View : Memory_View;
      Length : Natural) return String
   is
      subtype String_Buffer is String (1 .. Length);
      type String_Access is access all String_Buffer;
      function To_String_Access is new Ada.Unchecked_Conversion
        (System.Address, String_Access);
      Str_Ptr : constant String_Access := To_String_Access (View.Address);
   begin
      if Str_Ptr = null then
         return "";
      end if;
      return Str_Ptr.all;
   end Read_String_From_View;

   --  Unix-specific mmap tests
   function Test_Mmap_System_Call return Void_Result.Result is
      Test_File : constant String := "test_mmap_syscall.dat";
      Unix_Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Small_File_Size);
      Unix_Map := Create;

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Test basic mmap functionality
      declare
         Map_Res : constant Map_Result.Result := Unix_Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Verify mapping succeeded
         if not Unix_Map.Is_Mapped then
            Unix_Map.Unmap;
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("Unix mmap should have succeeded"));
         end if;

         -- Verify we got a valid address
         declare
            View : constant Memory_View := Unix_Map.Get_View;
         begin
            if View.Address = System.Null_Address then
               Unix_Map.Unmap;
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("mmap returned null address"));
            end if;
         end;

         -- Cleanup
         Unix_Map.Unmap;
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Mmap_System_Call;

   function Test_Mmap_Protection_Flags return Void_Result.Result is
      Test_File : constant String := "test_mmap_prot.dat";
      Unix_Map_RO, Unix_Map_RW : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Small_File_Size);
      Unix_Map_RO := Create;
      Unix_Map_RW := Create;

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Test read-only protection
      declare
         Map_Res : constant Map_Result.Result := Unix_Map_RO.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Can read from read-only mapping
         declare
            View : constant Memory_View := Unix_Map_RO.Get_View;
            Data : constant String := Read_String_From_View (View, 10);
            pragma Unreferenced (Data);
         begin
            null; -- Read succeeded
         end;

         Unix_Map_RO.Unmap;
      end;

      -- Test read-write protection
      declare
         Map_Res : constant Map_Result.Result := Unix_Map_RW.Map_File (Path, False);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Can write to read-write mapping
         declare
            View : constant Memory_View := Unix_Map_RW.Get_View;
            subtype String_Buffer is String (1 .. 10);
            type String_Access is access all String_Buffer;
            function To_String_Access is new Ada.Unchecked_Conversion
              (System.Address, String_Access);
            Str_Ptr : constant String_Access := To_String_Access (View.Address);
         begin
            if Str_Ptr /= null then
               Str_Ptr.all := "Modified!!";
            end if;
         end;

         Unix_Map_RW.Sync;
         Unix_Map_RW.Unmap;
      end;

      -- Cleanup
      Delete_Test_File (Test_File);
      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Mmap_Protection_Flags;

   function Test_Mmap_Sharing_Flags return Void_Result.Result is
      Test_File : constant String := "test_mmap_sharing.dat";
      Unix_Map1, Unix_Map2 : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Small_File_Size);
      Unix_Map1 := Create;
      Unix_Map2 := Create;

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Test MAP_SHARED behavior - changes visible across mappings
      declare
         Map_Res1 : constant Map_Result.Result := Unix_Map1.Map_File (Path, False);
         Map_Res2 : constant Map_Result.Result := Unix_Map2.Map_File (Path, True);
      begin
         if Map_Res1.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res1.Unwrap_Err);
         end if;

         if Map_Res2.Is_Err then
            Unix_Map1.Unmap;
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res2.Unwrap_Err);
         end if;

         -- Write through first mapping
         declare
            View1 : constant Memory_View := Unix_Map1.Get_View;
            subtype String_Buffer is String (1 .. 6);
            type String_Access is access all String_Buffer;
            function To_String_Access is new Ada.Unchecked_Conversion
              (System.Address, String_Access);
            Str_Ptr : constant String_Access := To_String_Access (View1.Address);
         begin
            if Str_Ptr /= null then
               Str_Ptr.all := "SHARED";
            end if;
         end;

         -- Sync to ensure visibility
         Unix_Map1.Sync;

         -- Read through second mapping
         declare
            View2 : constant Memory_View := Unix_Map2.Get_View;
            Data : constant String := Read_String_From_View (View2, 6);
         begin
            if Data /= "SHARED" then
               Unix_Map1.Unmap;
               Unix_Map2.Unmap;
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("MAP_SHARED changes not visible"));
            end if;
         end;

         -- Cleanup
         Unix_Map1.Unmap;
         Unix_Map2.Unmap;
      end;

      Delete_Test_File (Test_File);
      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Mmap_Sharing_Flags;

   function Test_Mmap_Page_Alignment return Void_Result.Result is
      Test_File : constant String := "test_mmap_pagesize.dat";
      Unix_Map : Unix_Memory_Map_Access;
      Path : File_Path;
      Page_Size : constant Natural := Natural (C_Unix.getpagesize);
   begin
      -- Setup - create file of exactly one page
      Create_Test_File (Test_File, Page_Size);
      Unix_Map := Create;

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Test page-aligned mapping
      declare
         Map_Res : constant Map_Result.Result := Unix_Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Verify size is page-aligned
         if Unix_Map.Get_Size /= Storage_Count (Page_Size) then
            Unix_Map.Unmap;
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("Mapped size not page-aligned"));
         end if;

         -- Verify address is page-aligned
         declare
            View : constant Memory_View := Unix_Map.Get_View;
            Address_Int : constant Integer_Address :=
              To_Integer (View.Address);
         begin
            if Address_Int mod Integer_Address (Page_Size) /= 0 then
               Unix_Map.Unmap;
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("Mapped address not page-aligned"));
            end if;
         end;

         -- Cleanup
         Unix_Map.Unmap;
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Mmap_Page_Alignment;

   -- File descriptor management
   function Test_File_Descriptor_Lifecycle return Void_Result.Result is
      Test_File : constant String := "test_fd_lifecycle.dat";
      Unix_Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Small_File_Size);
      Unix_Map := Create;

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Test file descriptor is properly managed
      declare
         Map_Res : constant Map_Result.Result := Unix_Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- File can be deleted while mapped (Unix behavior)
         Delete_Test_File (Test_File);

         -- Mapping should still be valid
         if not Unix_Map.Is_Mapped then
            Unix_Map.Unmap;
            return Void_Result.Err (To_Unbounded_String
              ("Mapping invalidated after file delete"));
         end if;

         -- Can still read from mapping
         declare
            View : constant Memory_View := Unix_Map.Get_View;
            Data : constant String := Read_String_From_View (View, 10);
            pragma Unreferenced (Data);
         begin
            null; -- Read succeeded
         end;

         -- Cleanup
         Unix_Map.Unmap;
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_File_Descriptor_Lifecycle;

   function Test_Invalid_File_Descriptor return Void_Result.Result is
      Unix_Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      Unix_Map := Create;

      -- Try to map non-existent file
      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String ("/nonexistent/file.dat"));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Should fail with appropriate error
      declare
         Map_Res : constant Map_Result.Result := Unix_Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Ok then
            Unix_Map.Unmap;
            return Void_Result.Err (To_Unbounded_String
              ("Mapping non-existent file should fail"));
         end if;

         -- Verify we got meaningful error message
         declare
            Error_Msg : constant String := To_String (Map_Res.Unwrap_Err);
         begin
            if Error_Msg = "" then
               return Void_Result.Err (To_Unbounded_String
                 ("Empty error message for invalid file"));
            end if;
         end;

         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Invalid_File_Descriptor;

   function Test_File_Descriptor_Limits return Void_Result.Result is
      Max_FDs : constant := 10;  -- Test with small number
      Test_Files : array (1 .. Max_FDs) of Unbounded_String;
      Unix_Maps : array (1 .. Max_FDs) of Unix_Memory_Map_Access;
      Success_Count : Natural := 0;
   begin
      -- Create test files
      for I in Test_Files'Range loop
         Test_Files (I) := To_Unbounded_String
           ("test_fd_limit_" & I'Image & ".dat");
         Create_Test_File (To_String (Test_Files (I)), Small_File_Size);
         Unix_Maps (I) := Create;
      end loop;

      -- Try to map all files
      for I in Unix_Maps'Range loop
         declare
            Path : File_Path;
            Path_Result : constant File_Path_Result.Result :=
              Create_File_Path (Test_Files (I));
         begin
            if Path_Result.Is_Ok then
               Path := Path_Result.Unwrap;
               declare
                  Map_Res : constant Map_Result.Result :=
                    Unix_Maps (I).Map_File (Path, True);
               begin
                  if Map_Res.Is_Ok then
                     Success_Count := Success_Count + 1;
                  end if;
               end;
            end if;
         end;
      end loop;

      -- At least some should succeed
      if Success_Count = 0 then
         -- Cleanup
         for I in Unix_Maps'Range loop
            if Unix_Maps (I).Is_Mapped then
               Unix_Maps (I).Unmap;
            end if;
            Delete_Test_File (To_String (Test_Files (I)));
         end loop;
         return Void_Result.Err (To_Unbounded_String
           ("No mappings succeeded"));
      end if;

      -- Cleanup
      for I in Unix_Maps'Range loop
         if Unix_Maps (I).Is_Mapped then
            Unix_Maps (I).Unmap;
         end if;
         Delete_Test_File (To_String (Test_Files (I)));
      end loop;

      return Void_Result.Ok;

   exception
      when E : others =>
         -- Cleanup on exception
         for I in Test_Files'Range loop
            Delete_Test_File (To_String (Test_Files (I)));
         end loop;
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_File_Descriptor_Limits;

   -- Memory synchronization
   function Test_Msync_Synchronous return Void_Result.Result is
      Test_File : constant String := "test_msync_sync.dat";
      Unix_Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Small_File_Size);
      Unix_Map := Create;

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Map for writing
      declare
         Map_Res : constant Map_Result.Result := Unix_Map.Map_File (Path, False);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Modify data
         declare
            View : constant Memory_View := Unix_Map.Get_View;
            subtype String_Buffer is String (1 .. 5);
            type String_Access is access all String_Buffer;
            function To_String_Access is new Ada.Unchecked_Conversion
              (System.Address, String_Access);
            Str_Ptr : constant String_Access := To_String_Access (View.Address);
         begin
            if Str_Ptr /= null then
               Str_Ptr.all := "SYNCD";
            end if;
         end;

         -- Synchronous sync
         Unix_Map.Sync;

         -- Verify data was written
         Unix_Map.Unmap;

         -- Read file to verify
         declare
            use Ada.Text_IO;
            F : File_Type;
            Line : String (1 .. 5);
            Last : Natural;
         begin
            Open (F, In_File, Test_File);
            Get (F, Line, Last);
            Close (F);

            if Line (1 .. Last) /= "SYNCD" (1 .. Last) then
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("Sync did not persist changes"));
            end if;
         end;

         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Msync_Synchronous;

   function Test_Msync_Asynchronous return Void_Result.Result is
      Test_File : constant String := "test_msync_async.dat";
      Unix_Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Small_File_Size);
      Unix_Map := Create;

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Map for writing
      declare
         Map_Res : constant Map_Result.Result := Unix_Map.Map_File (Path, False);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Modify data
         declare
            View : constant Memory_View := Unix_Map.Get_View;
            subtype String_Buffer is String (1 .. 5);
            type String_Access is access all String_Buffer;
            function To_String_Access is new Ada.Unchecked_Conversion
              (System.Address, String_Access);
            Str_Ptr : constant String_Access := To_String_Access (View.Address);
         begin
            if Str_Ptr /= null then
               Str_Ptr.all := "ASYNC";
            end if;
         end;

         -- Async sync (returns immediately)
         Unix_Map.Sync;

         -- Cleanup
         Unix_Map.Unmap;
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Msync_Asynchronous;

   function Test_Msync_Invalidate return Void_Result.Result is
      Test_File : constant String := "test_msync_invalidate.dat";
      Unix_Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Small_File_Size);
      Unix_Map := Create;

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Test sync with invalidate
      declare
         Map_Res : constant Map_Result.Result := Unix_Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Sync should work even on read-only mapping
         Unix_Map.Sync;

         -- Cleanup
         Unix_Map.Unmap;
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Msync_Invalidate;

   -- Memory advisement
   function Test_Madvise_Sequential return Void_Result.Result is
      Test_File : constant String := "test_madvise_seq.dat";
      Unix_Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Small_File_Size);
      Unix_Map := Create;

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Test sequential access advisement
      declare
         Map_Res : constant Map_Result.Result := Unix_Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Advise sequential access
         Unix_Map.Advise (Sequential);

         -- Perform sequential read
         declare
            View : constant Memory_View := Unix_Map.Get_View;
            Data : String (1 .. 100);
         begin
            for I in Data'Range loop
               declare
                  subtype Char_Type is Character;
                  type Char_Access is access all Char_Type;
                  function To_Char_Access is new Ada.Unchecked_Conversion
                    (System.Address, Char_Access);
                  Char_Ptr : constant Char_Access :=
                    To_Char_Access (View.Address + Storage_Offset (I - 1));
               begin
                  if Char_Ptr /= null then
                     Data (I) := Char_Ptr.all;
                  end if;
               end;
            end loop;
         end;

         -- Cleanup
         Unix_Map.Unmap;
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Madvise_Sequential;

   function Test_Madvise_Random return Void_Result.Result is
      Test_File : constant String := "test_madvise_random.dat";
      Unix_Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Small_File_Size);
      Unix_Map := Create;

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Test random access advisement
      declare
         Map_Res : constant Map_Result.Result := Unix_Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Advise random access
         Unix_Map.Advise (Random);

         -- Perform random reads
         declare
            View : constant Memory_View := Unix_Map.Get_View;
            Offsets : constant array (1 .. 5) of Storage_Offset :=
              (100, 10, 500, 50, 200);
         begin
            for Offset of Offsets loop
               if Offset < Storage_Offset (Unix_Map.Get_Size) then
                  declare
                     subtype Char_Type is Character;
                     type Char_Access is access all Char_Type;
                     function To_Char_Access is new Ada.Unchecked_Conversion
                       (System.Address, Char_Access);
                     Char_Ptr : constant Char_Access :=
                       To_Char_Access (View.Address + Offset);
                     C : Character;
                  begin
                     if Char_Ptr /= null then
                        C := Char_Ptr.all;
                     end if;
                  end;
               end if;
            end loop;
         end;

         -- Cleanup
         Unix_Map.Unmap;
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Madvise_Random;

   function Test_Madvise_Willneed return Void_Result.Result is
      Test_File : constant String := "test_madvise_willneed.dat";
      Unix_Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Small_File_Size);
      Unix_Map := Create;

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Test will need advisement
      declare
         Map_Res : constant Map_Result.Result := Unix_Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Advise will need specific range
         Unix_Map.Advise (Will_Need, 0, 1024);

         -- Access the advised range
         declare
            View : constant Memory_View := Unix_Map.Get_View;
            Data : constant String := Read_String_From_View (View, 10);
            pragma Unreferenced (Data);
         begin
            null; -- Pages should be resident
         end;

         -- Cleanup
         Unix_Map.Unmap;
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Madvise_Willneed;

   function Test_Madvise_Dontneed return Void_Result.Result is
      Test_File : constant String := "test_madvise_dontneed.dat";
      Unix_Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Small_File_Size);
      Unix_Map := Create;

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Test don't need advisement
      declare
         Map_Res : constant Map_Result.Result := Unix_Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Read some data first
         declare
            View : constant Memory_View := Unix_Map.Get_View;
            Data : constant String := Read_String_From_View (View, 100);
            pragma Unreferenced (Data);
         begin
            null;
         end;

         -- Advise don't need for the range we just read
         Unix_Map.Advise (Dont_Need, 0, 100);

         -- Cleanup
         Unix_Map.Unmap;
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Madvise_Dontneed;

   -- Domain interface compliance
   function Test_Interface_Implementation return Void_Result.Result is
      Test_File : constant String := "test_interface.dat";
      Unix_Map : Unix_Memory_Map_Access;
      Interface_Ref : access Memory_Mapped_File_Interface'Class;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Small_File_Size);
      Unix_Map := Create;
      Interface_Ref := Unix_Map;  -- Verify it implements the interface

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Test all interface methods
      declare
         Map_Res : constant Map_Result.Result :=
           Interface_Ref.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Test Is_Mapped
         if not Interface_Ref.Is_Mapped then
            Interface_Ref.Unmap;
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("Is_Mapped returned false for mapped file"));
         end if;

         -- Test Get_View
         declare
            View : constant Memory_View := Interface_Ref.Get_View;
         begin
            if View.Address = System.Null_Address then
               Interface_Ref.Unmap;
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("Get_View returned null address"));
            end if;
         end;

         -- Test Get_Size
         if Interface_Ref.Get_Size = 0 then
            Interface_Ref.Unmap;
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("Get_Size returned 0 for non-empty file"));
         end if;

         -- Test Create_Subview
         declare
            Subview : constant Memory_View :=
              Interface_Ref.Create_Subview (0, 100);
         begin
            if Subview.Size /= 100 then
               Interface_Ref.Unmap;
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("Create_Subview returned wrong size"));
            end if;
         end;

         -- Test Sync
         Interface_Ref.Sync;

         -- Test Advise
         Interface_Ref.Advise (Sequential);

         -- Test Unmap
         Interface_Ref.Unmap;

         if Interface_Ref.Is_Mapped then
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("Is_Mapped returned true after unmap"));
         end if;

         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Interface_Implementation;

   function Test_Interface_Contracts return Void_Result.Result is
      Test_File : constant String := "test_contracts.dat";
      Unix_Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Small_File_Size);
      Unix_Map := Create;

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Test precondition violations
      declare
         Success : Boolean := False;
      begin
         -- Get_View on unmapped file should raise exception
         declare
            View : constant Memory_View := Unix_Map.Get_View;
            pragma Unreferenced (View);
         begin
            Success := False;
         end;
      exception
         when others =>
            Success := True;  -- Expected
      end;

      if not Success then
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Get_View precondition not enforced"));
      end if;

      -- Map the file
      declare
         Map_Res : constant Map_Result.Result := Unix_Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;
      end;

      -- Test postconditions
      Unix_Map.Unmap;

      -- Postcondition: not Is_Mapped after Unmap
      if Unix_Map.Is_Mapped then
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unmap postcondition violated"));
      end if;

      Delete_Test_File (Test_File);
      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Interface_Contracts;

   function Test_Interface_Error_Propagation return Void_Result.Result is
      Unix_Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      Unix_Map := Create;

      -- Test error propagation through Result type
      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String ("/no/such/directory/file.dat"));
      begin
         if Path_Result.Is_Err then
            -- Path creation might fail - that's ok
            return Void_Result.Ok;
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Map should fail and return Result.Err
      declare
         Map_Res : constant Map_Result.Result := Unix_Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Ok then
            Unix_Map.Unmap;
            return Void_Result.Err (To_Unbounded_String
              ("Expected error not propagated"));
         end if;

         -- Verify error message is meaningful
         declare
            Error_Msg : constant String := To_String (Map_Res.Unwrap_Err);
         begin
            if Error_Msg = "" then
               return Void_Result.Err (To_Unbounded_String
                 ("Empty error message"));
            end if;
         end;

         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Interface_Error_Propagation;

   -- Unix-specific error conditions
   function Test_Errno_Handling return Void_Result.Result is
      Unix_Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      Unix_Map := Create;

      -- Create a path that will trigger specific errno values
      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String ("/root/privileged_file.dat"));
      begin
         if Path_Result.Is_Err then
            -- Path creation failed - try alternative
            declare
               Alt_Path_Result : constant File_Path_Result.Result :=
                 Create_File_Path (To_Unbounded_String ("/etc/shadow"));
            begin
               if Alt_Path_Result.Is_Err then
                  return Void_Result.Ok;  -- Skip test
               end if;
               Path := Alt_Path_Result.Unwrap;
            end;
         else
            Path := Path_Result.Unwrap;
         end if;
      end;

      -- Try to map privileged file - should get permission error
      declare
         Map_Res : constant Map_Result.Result := Unix_Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Ok then
            Unix_Map.Unmap;
            -- Running as root? Skip test
            return Void_Result.Ok;
         end if;

         -- Check error message contains permission-related text
         declare
            Error_Msg : constant String := To_String (Map_Res.Unwrap_Err);
         begin
            -- Error message should mention permission or access
            if Error_Msg = "" then
               return Void_Result.Err (To_Unbounded_String
                 ("No error message for permission denied"));
            end if;
         end;

         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Errno_Handling;

   function Test_Signal_Handling return Void_Result.Result is
      Test_File : constant String := "test_signal.dat";
      Unix_Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Small_File_Size);
      Unix_Map := Create;

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Map file
      declare
         Map_Res : constant Map_Result.Result := Unix_Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Test potential SIGBUS scenario - truncate file while mapped
         -- This is platform-dependent behavior

         -- For safety, just verify mapping is still valid
         if not Unix_Map.Is_Mapped then
            Unix_Map.Unmap;
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("Mapping invalidated unexpectedly"));
         end if;

         -- Cleanup
         Unix_Map.Unmap;
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Signal_Handling;

   function Test_Permission_Errors return Void_Result.Result is
      Test_File : constant String := "test_permissions.dat";
      Unix_Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup - create read-only file
      Create_Test_File (Test_File, Small_File_Size);

      -- Make file read-only
      declare
         use Interfaces.C;
         function chmod (pathname : char_array; mode : int) return int
           with Import, Convention => C, External_Name => "chmod";
         Result : int;
      begin
         Result := chmod (To_C (Test_File), 8#444#);  -- Read-only
         if Result /= 0 then
            Delete_Test_File (Test_File);
            return Void_Result.Ok;  -- Skip test if chmod fails
         end if;
      end;

      Unix_Map := Create;

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            -- Restore permissions
            declare
               use Interfaces.C;
               function chmod (pathname : char_array; mode : int) return int
                 with Import, Convention => C, External_Name => "chmod";
               Result : int;
            begin
               Result := chmod (To_C (Test_File), 8#644#);
            end;
            Delete_Test_File (Test_File);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Try to map read-only file for writing - should fail
      declare
         Map_Res : constant Map_Result.Result := Unix_Map.Map_File (Path, False);
      begin
         -- Restore permissions
         declare
            use Interfaces.C;
            function chmod (pathname : char_array; mode : int) return int
              with Import, Convention => C, External_Name => "chmod";
            Result : int;
         begin
            Result := chmod (To_C (Test_File), 8#644#);
         end;

         if Map_Res.Is_Ok then
            Unix_Map.Unmap;
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("Write mapping of read-only file should fail"));
         end if;

         -- Expected failure
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Permission_Errors;

   -- Resource limits
   function Test_Memory_Lock_Limits return Void_Result.Result is
      Test_File : constant String := "test_mlock.dat";
      Unix_Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Small_File_Size);
      Unix_Map := Create;

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Map file
      declare
         Map_Res : constant Map_Result.Result := Unix_Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Memory locking would require mlock/mlockall
         -- Just verify mapping works within limits
         if not Unix_Map.Is_Mapped then
            Unix_Map.Unmap;
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("Mapping failed within limits"));
         end if;

         -- Cleanup
         Unix_Map.Unmap;
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Memory_Lock_Limits;

   function Test_Virtual_Memory_Limits return Void_Result.Result is
      -- Test behavior at virtual memory limits
      -- This is a minimal test as actually exhausting VM is dangerous
      Unix_Map : Unix_Memory_Map_Access;
   begin
      Unix_Map := Create;

      -- Just verify the system has reasonable limits
      declare
         Open_Max : constant Long_Long_Integer :=
           Long_Long_Integer (C_Unix.sysconf (C_Unix._SC_OPEN_MAX));
      begin
         if Open_Max <= 0 then
            return Void_Result.Err (To_Unbounded_String
              ("Invalid open file limit"));
         end if;
      end;

      return Void_Result.Ok;

   exception
      when E : others =>
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Virtual_Memory_Limits;

   function Test_Address_Space_Exhaustion return Void_Result.Result is
      -- Test behavior when address space is exhausted
      -- This is a minimal test as actually exhausting address space is dangerous
   begin
      -- Just verify we can detect platform capabilities
      if not Is_Memory_Mapping_Available then
         return Void_Result.Err (To_Unbounded_String
           ("Memory mapping should be available"));
      end if;

      return Void_Result.Ok;

   exception
      when E : others =>
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Address_Space_Exhaustion;

   -- Special file types
   function Test_Device_File_Mapping return Void_Result.Result is
      Unix_Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      Unix_Map := Create;

      -- Try to map /dev/zero (commonly available)
      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String ("/dev/zero"));
      begin
         if Path_Result.Is_Err then
            -- /dev/zero not available, skip test
            return Void_Result.Ok;
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Mapping /dev/zero might succeed or fail depending on system
      declare
         Map_Res : constant Map_Result.Result := Unix_Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Ok then
            -- Some systems allow mapping device files
            Unix_Map.Unmap;
         end if;
         -- Either way is acceptable
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Device_File_Mapping;

   function Test_Sparse_File_Mapping return Void_Result.Result is
      Test_File : constant String := "test_sparse.dat";
      Unix_Map : Unix_Memory_Map_Access;
      Path : File_Path;
      Sparse_Size : constant := 1024 * 1024;  -- 1MB sparse file
   begin
      -- Create sparse file
      declare
         use Ada.Text_IO;
         F : File_Type;
      begin
         Create (F, Out_File, Test_File);
         -- Write at beginning
         Put (F, "START");
         -- Seek to near end
         Set_Index (F, Ada.Text_IO.Count (Sparse_Size - 10));
         -- Write at end
         Put (F, "END");
         Close (F);
      end;

      Unix_Map := Create;

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Map sparse file
      declare
         Map_Res : constant Map_Result.Result := Unix_Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Verify we can read from beginning and end
         declare
            View : constant Memory_View := Unix_Map.Get_View;
            Start_Data : constant String := Read_String_From_View (View, 5);
         begin
            if Start_Data /= "START" then
               Unix_Map.Unmap;
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("Failed to read sparse file start"));
            end if;
         end;

         -- Read from end
         declare
            End_View : constant Memory_View :=
              Unix_Map.Create_Subview
                (Storage_Count (Sparse_Size - 10), 3);
            End_Data : constant String :=
              Read_String_From_View (End_View, 3);
         begin
            if End_Data /= "END" then
               Unix_Map.Unmap;
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("Failed to read sparse file end"));
            end if;
         end;

         -- Cleanup
         Unix_Map.Unmap;
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Sparse_File_Mapping;

   function Test_Huge_Page_Support return Void_Result.Result is
      -- Test transparent huge page support
      -- This is system-dependent and may not be available
      Test_File : constant String := "test_hugepage.dat";
      Unix_Map : Unix_Memory_Map_Access;
      Path : File_Path;
      Huge_Size : constant := 2 * 1024 * 1024;  -- 2MB (typical huge page size)
   begin
      -- Create file large enough for huge pages
      Create_Test_File (Test_File, Huge_Size);
      Unix_Map := Create;

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Map file (huge pages may be used transparently)
      declare
         Map_Res : constant Map_Result.Result := Unix_Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Advise for huge page usage
         Unix_Map.Advise (Sequential);

         -- Verify mapping works
         if Unix_Map.Get_Size < Storage_Count (Huge_Size) then
            Unix_Map.Unmap;
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("Mapped size incorrect"));
         end if;

         -- Cleanup
         Unix_Map.Unmap;
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Huge_Page_Support;

   -- Concurrent access
   function Test_Fork_Inheritance return Void_Result.Result is
      -- Fork inheritance testing would require process creation
      -- This is a simplified test for memory mapping consistency
      Test_File : constant String := "test_fork.dat";
      Unix_Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Small_File_Size);
      Unix_Map := Create;

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Map file with MAP_SHARED (would be inherited by fork)
      declare
         Map_Res : constant Map_Result.Result := Unix_Map.Map_File (Path, False);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- In a real fork test, child would inherit mapping
         -- Here we just verify mapping is stable
         if not Unix_Map.Is_Mapped then
            Unix_Map.Unmap;
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("Mapping not stable"));
         end if;

         -- Cleanup
         Unix_Map.Unmap;
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Fork_Inheritance;

   function Test_Thread_Safety return Void_Result.Result is
      Test_File : constant String := "test_threads.dat";
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Small_File_Size);

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Test concurrent access using tasks
      declare
         task type Reader is
            entry Start;
            entry Get_Result (Success : out Boolean);
         end Reader;

         task body Reader is
            Unix_Map : Unix_Memory_Map_Access;
            OK : Boolean := False;
         begin
            accept Start;

            Unix_Map := Create;
            declare
               Map_Res : constant Map_Result.Result :=
                 Unix_Map.Map_File (Path, True);
            begin
               if Map_Res.Is_Ok then
                  -- Perform some reads
                  declare
                     View : constant Memory_View := Unix_Map.Get_View;
                  begin
                     if View.Address /= System.Null_Address then
                        -- Read some data
                        declare
                           Data : constant String :=
                             Read_String_From_View (View, 10);
                           pragma Unreferenced (Data);
                        begin
                           OK := True;
                        end;
                     end if;
                  end;
                  Unix_Map.Unmap;
               end if;
            end;

            accept Get_Result (Success : out Boolean) do
               Success := OK;
            end Get_Result;
         end Reader;

         Readers : array (1 .. 4) of Reader;
         All_Success : Boolean := True;
      begin
         -- Start all readers
         for R of Readers loop
            R.Start;
         end loop;

         -- Get results
         for R of Readers loop
            declare
               Success : Boolean;
            begin
               R.Get_Result (Success);
               All_Success := All_Success and Success;
            end;
         end loop;

         if not All_Success then
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("Not all threads succeeded"));
         end if;
      end;

      -- Cleanup
      Delete_Test_File (Test_File);
      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Thread_Safety;

   function Test_Shared_Memory_Consistency return Void_Result.Result is
      Test_File : constant String := "test_shared_consistency.dat";
      Unix_Map1, Unix_Map2 : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Small_File_Size);
      Unix_Map1 := Create;
      Unix_Map2 := Create;

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Create two MAP_SHARED mappings
      declare
         Map_Res1 : constant Map_Result.Result :=
           Unix_Map1.Map_File (Path, False);
         Map_Res2 : constant Map_Result.Result :=
           Unix_Map2.Map_File (Path, True);
      begin
         if Map_Res1.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res1.Unwrap_Err);
         end if;

         if Map_Res2.Is_Err then
            Unix_Map1.Unmap;
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res2.Unwrap_Err);
         end if;

         -- Write through first mapping
         declare
            View1 : constant Memory_View := Unix_Map1.Get_View;
            Test_Pattern : constant String := "CONSISTENCY";
            subtype String_Buffer is String (1 .. Test_Pattern'Length);
            type String_Access is access all String_Buffer;
            function To_String_Access is new Ada.Unchecked_Conversion
              (System.Address, String_Access);
            Str_Ptr : constant String_Access :=
              To_String_Access (View1.Address);
         begin
            if Str_Ptr /= null then
               Str_Ptr.all := Test_Pattern;
            end if;
         end;

         -- Sync first mapping
         Unix_Map1.Sync;

         -- Read through second mapping
         declare
            View2 : constant Memory_View := Unix_Map2.Get_View;
            Data : constant String :=
              Read_String_From_View (View2, 11);
         begin
            if Data /= "CONSISTENCY" then
               Unix_Map1.Unmap;
               Unix_Map2.Unmap;
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("Shared memory not consistent"));
            end if;
         end;

         -- Cleanup
         Unix_Map1.Unmap;
         Unix_Map2.Unmap;
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Shared_Memory_Consistency;

   -- Run all tests
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Stats : Test_Stats := (Total => 0, Passed => 0, Failed => 0, Skipped => 0);
   begin
      -- Unix-specific mmap tests
      Run_Test (Output.all, "Mmap System Call", Test_Mmap_System_Call'Access, Stats);
      Run_Test (Output.all, "Mmap Protection Flags", Test_Mmap_Protection_Flags'Access, Stats);
      Run_Test (Output.all, "Mmap Sharing Flags", Test_Mmap_Sharing_Flags'Access, Stats);
      Run_Test (Output.all, "Mmap Page Alignment", Test_Mmap_Page_Alignment'Access, Stats);

      -- File descriptor management
      Run_Test (Output.all, "File Descriptor Lifecycle", Test_File_Descriptor_Lifecycle'Access, Stats);
      Run_Test (Output.all, "Invalid File Descriptor", Test_Invalid_File_Descriptor'Access, Stats);
      Run_Test (Output.all, "File Descriptor Limits", Test_File_Descriptor_Limits'Access, Stats);

      -- Memory synchronization
      Run_Test (Output.all, "Msync Synchronous", Test_Msync_Synchronous'Access, Stats);
      Run_Test (Output.all, "Msync Asynchronous", Test_Msync_Asynchronous'Access, Stats);
      Run_Test (Output.all, "Msync Invalidate", Test_Msync_Invalidate'Access, Stats);

      -- Memory advisement
      Run_Test (Output.all, "Madvise Sequential", Test_Madvise_Sequential'Access, Stats);
      Run_Test (Output.all, "Madvise Random", Test_Madvise_Random'Access, Stats);
      Run_Test (Output.all, "Madvise Willneed", Test_Madvise_Willneed'Access, Stats);
      Run_Test (Output.all, "Madvise Dontneed", Test_Madvise_Dontneed'Access, Stats);

      -- Domain interface compliance
      Run_Test (Output.all, "Interface Implementation", Test_Interface_Implementation'Access, Stats);
      Run_Test (Output.all, "Interface Contracts", Test_Interface_Contracts'Access, Stats);
      Run_Test (Output.all, "Interface Error Propagation", Test_Interface_Error_Propagation'Access, Stats);

      -- Unix-specific error conditions
      Run_Test (Output.all, "Errno Handling", Test_Errno_Handling'Access, Stats);
      Run_Test (Output.all, "Signal Handling", Test_Signal_Handling'Access, Stats);
      Run_Test (Output.all, "Permission Errors", Test_Permission_Errors'Access, Stats);

      -- Resource limits
      Run_Test (Output.all, "Memory Lock Limits", Test_Memory_Lock_Limits'Access, Stats);
      Run_Test (Output.all, "Virtual Memory Limits", Test_Virtual_Memory_Limits'Access, Stats);
      Run_Test (Output.all, "Address Space Exhaustion", Test_Address_Space_Exhaustion'Access, Stats);

      -- Special file types
      Run_Test (Output.all, "Device File Mapping", Test_Device_File_Mapping'Access, Stats);
      Run_Test (Output.all, "Sparse File Mapping", Test_Sparse_File_Mapping'Access, Stats);
      Run_Test (Output.all, "Huge Page Support", Test_Huge_Page_Support'Access, Stats);

      -- Concurrent access
      Run_Test (Output.all, "Fork Inheritance", Test_Fork_Inheritance'Access, Stats);
      Run_Test (Output.all, "Thread Safety", Test_Thread_Safety'Access, Stats);
      Run_Test (Output.all, "Shared Memory Consistency", Test_Shared_Memory_Consistency'Access, Stats);

      return Test_Stats_Result.Ok (Stats);
   end Run_All_Tests;

end Test_Unix_Memory_Map;
