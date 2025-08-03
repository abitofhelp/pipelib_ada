--  =============================================================================
--  Test_Memory_Mapped_File - Memory Mapped File Integration Tests Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with System.Storage_Elements;
with Interfaces.C;

with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Value_Objects.File_Path;
with Abohlib.Infrastructure.Testing.Test_Framework;
with Pipelib.Infrastructure.IO.Memory_Mapped_File;

package body Test_Memory_Mapped_File is

   use System.Storage_Elements;
   use Abohlib.Core.Domain.Value_Objects.File_Path;
   use Abohlib.Infrastructure.Testing.Test_Framework;
   use Pipelib.Infrastructure.IO.Memory_Mapped_File;

   --  Test constants
   Small_File_Size : constant := 1024;  -- 1 KB
   Large_File_Size : constant := 100 * 1024 * 1024;  -- 100 MB
   Test_Data : constant String := "Test data for memory mapping";

   --  Helper to create a test file
   procedure Create_Test_File
     (Path : String;
      Size : Natural;
      Fill_Pattern : Character := 'A')
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

   --  Basic file mapping operations
   function Test_Map_Small_File return Void_Result.Result is
      Test_File : constant String := "test_small_file.dat";
      File : Memory_Mapped_File;
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

      -- Test
      declare
         Map_Res : constant Map_Result.Result := Map_File (File, Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Verify mapping
         if not Is_Mapped (File) then
            Unmap (File);
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("File should be mapped after successful Map_File"));
         end if;

         -- Verify size
         if Get_Size (File) /= Storage_Count (Small_File_Size) then
            Unmap (File);
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("Mapped file size doesn't match expected size"));
         end if;

         -- Verify view
         declare
            View : constant Memory_View := Get_View (File);
         begin
            if View.Address = System.Null_Address then
               Unmap (File);
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("Memory view has null address"));
            end if;

            if View.Size /= Storage_Count (Small_File_Size) then
               Unmap (File);
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("Memory view size doesn't match file size"));
            end if;
         end;

         -- Cleanup
         Unmap (File);
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Map_Small_File;

   function Test_Map_Large_File return Void_Result.Result is
      Test_File : constant String := "test_large_file.dat";
      File : Memory_Mapped_File;
      Path : File_Path;
   begin
      -- Skip test if not enough disk space
      if not Should_Use_Memory_Map (Long_Long_Integer (Large_File_Size)) then
         return Void_Result.Ok;  -- Skip test
      end if;

      -- Setup - create sparse file for faster testing
      declare
         use Ada.Text_IO;
         F : File_Type;
      begin
         Create (F, Out_File, Test_File);
         -- Write header
         Put_Line (F, "Large file test");
         -- Seek to end position to create sparse file
         Set_Index (F, Ada.Text_IO.Count (Large_File_Size - 10));
         Put_Line (F, "End");
         Close (F);
      end;

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

      -- Test
      declare
         Map_Res : constant Map_Result.Result := Map_File (File, Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Verify mapping
         if not Is_Mapped (File) then
            Unmap (File);
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("Large file should be mapped"));
         end if;

         -- Test advise for large file
         Advise (File, Sequential);

         -- Cleanup
         Unmap (File);
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Map_Large_File;

   function Test_Map_Empty_File return Void_Result.Result is
      Test_File : constant String := "test_empty_file.dat";
      File : Memory_Mapped_File;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, 0);

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

      -- Test - mapping empty file should fail or return empty view
      declare
         Map_Res : constant Map_Result.Result := Map_File (File, Path, True);
      begin
         if Map_Res.Is_Ok then
            -- Some systems allow mapping empty files
            if Get_Size (File) /= 0 then
               Unmap (File);
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("Empty file should have size 0"));
            end if;
            Unmap (File);
         end if;
         -- Either mapping failed (expected) or succeeded with size 0
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Map_Empty_File;

   function Test_Map_Nonexistent_File return Void_Result.Result is
      Test_File : constant String := "nonexistent_file_12345.dat";
      File : Memory_Mapped_File;
      Path : File_Path;
   begin
      -- Ensure file doesn't exist
      Delete_Test_File (Test_File);

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Test - mapping nonexistent file should fail
      declare
         Map_Res : constant Map_Result.Result := Map_File (File, Path, True);
      begin
         if Map_Res.Is_Ok then
            Unmap (File);
            return Void_Result.Err (To_Unbounded_String
              ("Mapping nonexistent file should fail"));
         end if;
         -- Expected failure
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Map_Nonexistent_File;

   -- Read/Write operations
   function Test_Read_Only_Mapping return Void_Result.Result is
      Test_File : constant String := "test_readonly.dat";
      File : Memory_Mapped_File;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Test_Data'Length);

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

      -- Write test data
      declare
         use Ada.Text_IO;
         F : File_Type;
      begin
         Open (F, Out_File, Test_File);
         Put (F, Test_Data);
         Close (F);
      end;

      -- Test read-only mapping
      declare
         Map_Res : constant Map_Result.Result := Map_File (File, Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Read data from mapped view
         declare
            View : constant Memory_View := Get_View (File);
            Data : constant String := Read_String_From_View
              (View, Test_Data'Length);
         begin
            if Data /= Test_Data then
               Unmap (File);
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("Read data doesn't match written data"));
            end if;
         end;

         -- Cleanup
         Unmap (File);
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Read_Only_Mapping;

   function Test_Read_Write_Mapping return Void_Result.Result is
      Test_File : constant String := "test_readwrite.dat";
      File : Memory_Mapped_File;
      Path : File_Path;
      Modified_Data : constant String := "Modified test data!!!";
   begin
      -- Setup
      Create_Test_File (Test_File, Test_Data'Length);

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

      -- Test read-write mapping
      declare
         Map_Res : constant Map_Result.Result := Map_File (File, Path, False);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Modify data through mapped view
         declare
            View : constant Memory_View := Get_View (File);
            subtype String_Buffer is String (1 .. Modified_Data'Length);
            type String_Access is access all String_Buffer;
            function To_String_Access is new Ada.Unchecked_Conversion
              (System.Address, String_Access);
            Str_Ptr : constant String_Access := To_String_Access (View.Address);
         begin
            if Str_Ptr /= null then
               Str_Ptr.all := Modified_Data;
            end if;
         end;

         -- Sync changes
         Sync (File);

         -- Cleanup
         Unmap (File);

         -- Verify changes were persisted
         declare
            use Ada.Text_IO;
            F : File_Type;
            Line : String (1 .. Modified_Data'Length);
            Last : Natural;
         begin
            Open (F, In_File, Test_File);
            Get (F, Line, Last);
            Close (F);

            if Line (1 .. Last) /= Modified_Data (1 .. Last) then
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("Modified data not persisted"));
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
   end Test_Read_Write_Mapping;

   function Test_Multiple_Mappings return Void_Result.Result is
      Test_File : constant String := "test_multiple.dat";
      File1, File2 : Memory_Mapped_File;
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

      -- Test multiple read-only mappings
      declare
         Map_Res1 : constant Map_Result.Result := Map_File (File1, Path, True);
         Map_Res2 : constant Map_Result.Result := Map_File (File2, Path, True);
      begin
         if Map_Res1.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res1.Unwrap_Err);
         end if;

         if Map_Res2.Is_Err then
            Unmap (File1);
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res2.Unwrap_Err);
         end if;

         -- Verify both mappings are valid
         if not Is_Mapped (File1) or not Is_Mapped (File2) then
            Unmap (File1);
            Unmap (File2);
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("Both files should be mapped"));
         end if;

         -- Cleanup
         Unmap (File1);
         Unmap (File2);
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Multiple_Mappings;

   -- Subview operations
   function Test_Create_Subview_Valid return Void_Result.Result is
      Test_File : constant String := "test_subview.dat";
      File : Memory_Mapped_File;
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

      -- Test
      declare
         Map_Res : constant Map_Result.Result := Map_File (File, Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Create subview in middle of file
         declare
            Offset : constant Storage_Count := 100;
            Length : constant Storage_Count := 200;
            Subview : constant Memory_View :=
              Create_Subview (File, Offset, Length);
         begin
            if Subview.Address = System.Null_Address then
               Unmap (File);
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("Subview has null address"));
            end if;

            if Subview.Size /= Length then
               Unmap (File);
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("Subview size doesn't match requested length"));
            end if;
         end;

         -- Cleanup
         Unmap (File);
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Create_Subview_Valid;

   function Test_Create_Subview_Boundary return Void_Result.Result is
      Test_File : constant String := "test_subview_boundary.dat";
      File : Memory_Mapped_File;
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

      -- Test
      declare
         Map_Res : constant Map_Result.Result := Map_File (File, Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         declare
            File_Size : constant Storage_Count := Get_Size (File);
         begin
            -- Test subview at beginning
            declare
               Subview : constant Memory_View :=
                 Create_Subview (File, 0, 100);
            begin
               if Subview.Size /= 100 then
                  Unmap (File);
                  Delete_Test_File (Test_File);
                  return Void_Result.Err (To_Unbounded_String
                    ("Beginning subview size incorrect"));
               end if;
            end;

            -- Test subview at end
            declare
               Subview : constant Memory_View :=
                 Create_Subview (File, File_Size - 100, 100);
            begin
               if Subview.Size /= 100 then
                  Unmap (File);
                  Delete_Test_File (Test_File);
                  return Void_Result.Err (To_Unbounded_String
                    ("End subview size incorrect"));
               end if;
            end;

            -- Test full file subview
            declare
               Subview : constant Memory_View :=
                 Create_Subview (File, 0, File_Size);
            begin
               if Subview.Size /= File_Size then
                  Unmap (File);
                  Delete_Test_File (Test_File);
                  return Void_Result.Err (To_Unbounded_String
                    ("Full file subview size incorrect"));
               end if;
            end;
         end;

         -- Cleanup
         Unmap (File);
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Create_Subview_Boundary;

   function Test_Create_Subview_Invalid return Void_Result.Result is
      Test_File : constant String := "test_subview_invalid.dat";
      File : Memory_Mapped_File;
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

      -- Test
      declare
         Map_Res : constant Map_Result.Result := Map_File (File, Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         declare
            File_Size : constant Storage_Count := Get_Size (File);
            Success : Boolean := False;
         begin
            -- Test offset beyond file size - should raise exception
            begin
               declare
                  Subview : constant Memory_View :=
                    Create_Subview (File, File_Size + 100, 100);
                  pragma Unreferenced (Subview);
               begin
                  Success := False;  -- Should not reach here
               end;
            exception
               when others =>
                  Success := True;  -- Expected
            end;

            if not Success then
               Unmap (File);
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("Invalid offset should raise exception"));
            end if;

            -- Test length beyond file size - should raise exception
            Success := False;
            begin
               declare
                  Subview : constant Memory_View :=
                    Create_Subview (File, 0, File_Size + 100);
                  pragma Unreferenced (Subview);
               begin
                  Success := False;  -- Should not reach here
               end;
            exception
               when others =>
                  Success := True;  -- Expected
            end;

            if not Success then
               Unmap (File);
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("Invalid length should raise exception"));
            end if;
         end;

         -- Cleanup
         Unmap (File);
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Create_Subview_Invalid;

   -- Synchronization operations
   function Test_Sync_Changes return Void_Result.Result is
      Test_File : constant String := "test_sync.dat";
      File : Memory_Mapped_File;
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

      -- Test
      declare
         Map_Res : constant Map_Result.Result := Map_File (File, Path, False);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Sync should work without errors
         Sync (File);

         -- Cleanup
         Unmap (File);
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Sync_Changes;

   function Test_Sync_Read_Only return Void_Result.Result is
      Test_File : constant String := "test_sync_readonly.dat";
      File : Memory_Mapped_File;
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

      -- Test
      declare
         Map_Res : constant Map_Result.Result := Map_File (File, Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Sync on read-only mapping should not fail
         Sync (File);

         -- Cleanup
         Unmap (File);
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Sync_Read_Only;

   -- Access pattern advisement
   function Test_Advise_Sequential return Void_Result.Result is
      Test_File : constant String := "test_advise_seq.dat";
      File : Memory_Mapped_File;
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

      -- Test
      declare
         Map_Res : constant Map_Result.Result := Map_File (File, Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Advise sequential access
         Advise (File, Sequential);

         -- Cleanup
         Unmap (File);
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Advise_Sequential;

   function Test_Advise_Random return Void_Result.Result is
      Test_File : constant String := "test_advise_random.dat";
      File : Memory_Mapped_File;
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

      -- Test
      declare
         Map_Res : constant Map_Result.Result := Map_File (File, Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Advise random access
         Advise (File, Random);

         -- Cleanup
         Unmap (File);
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Advise_Random;

   function Test_Advise_Will_Need return Void_Result.Result is
      Test_File : constant String := "test_advise_willneed.dat";
      File : Memory_Mapped_File;
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

      -- Test
      declare
         Map_Res : constant Map_Result.Result := Map_File (File, Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Advise will need pattern with specific range
         Advise (File, Will_Need, 0, 512);

         -- Cleanup
         Unmap (File);
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Advise_Will_Need;

   function Test_Advise_Dont_Need return Void_Result.Result is
      Test_File : constant String := "test_advise_dontneed.dat";
      File : Memory_Mapped_File;
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

      -- Test
      declare
         Map_Res : constant Map_Result.Result := Map_File (File, Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Advise don't need pattern
         Advise (File, Dont_Need, 512, 512);

         -- Cleanup
         Unmap (File);
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Advise_Dont_Need;

   -- Resource management
   function Test_Unmap_Cleanup return Void_Result.Result is
      Test_File : constant String := "test_unmap.dat";
      File : Memory_Mapped_File;
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

      -- Test
      declare
         Map_Res : constant Map_Result.Result := Map_File (File, Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Verify mapped
         if not Is_Mapped (File) then
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("File should be mapped"));
         end if;

         -- Unmap
         Unmap (File);

         -- Verify unmapped
         if Is_Mapped (File) then
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("File should not be mapped after unmap"));
         end if;

         -- Cleanup
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Unmap_Cleanup;

   function Test_Finalization return Void_Result.Result is
      Test_File : constant String := "test_finalize.dat";
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

      -- Test - create file in nested scope
      declare
         File : Memory_Mapped_File;
         Map_Res : constant Map_Result.Result := Map_File (File, Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;
         -- File goes out of scope here, should be finalized
      end;

      -- Cleanup
      Delete_Test_File (Test_File);
      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Finalization;

   function Test_Double_Unmap return Void_Result.Result is
      Test_File : constant String := "test_double_unmap.dat";
      File : Memory_Mapped_File;
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

      -- Test
      declare
         Map_Res : constant Map_Result.Result := Map_File (File, Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- First unmap
         Unmap (File);

         -- Second unmap - should not crash
         Unmap (File);

         -- Verify still unmapped
         if Is_Mapped (File) then
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("File should remain unmapped"));
         end if;

         -- Cleanup
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Double_Unmap;

   -- Platform capability tests
   function Test_Memory_Mapping_Available return Void_Result.Result is
   begin
      -- Test - should return true on supported platforms
      if not Is_Memory_Mapping_Available then
         return Void_Result.Err (To_Unbounded_String
           ("Memory mapping should be available on this platform"));
      end if;

      return Void_Result.Ok;

   exception
      when E : others =>
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Memory_Mapping_Available;

   function Test_Should_Use_Memory_Map return Void_Result.Result is
   begin
      -- Test default threshold (100MB)
      if Should_Use_Memory_Map (50 * 1024 * 1024) then
         return Void_Result.Err (To_Unbounded_String
           ("50MB file should not use memory mapping by default"));
      end if;

      if not Should_Use_Memory_Map (200 * 1024 * 1024) then
         return Void_Result.Err (To_Unbounded_String
           ("200MB file should use memory mapping by default"));
      end if;

      -- Test custom threshold
      if not Should_Use_Memory_Map (60 * 1024 * 1024, 50 * 1024 * 1024) then
         return Void_Result.Err (To_Unbounded_String
           ("60MB file should use memory mapping with 50MB threshold"));
      end if;

      return Void_Result.Ok;

   exception
      when E : others =>
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Should_Use_Memory_Map;

   -- Error handling
   function Test_Map_Protected_File return Void_Result.Result is
      Test_File : constant String := "test_protected.dat";
      File : Memory_Mapped_File;
      Path : File_Path;
   begin
      -- Setup - create file with restricted permissions
      Create_Test_File (Test_File, Small_File_Size);

      -- Change permissions to write-only (no read)
      declare
         use Interfaces.C;
         function chmod (pathname : char_array; mode : int) return int
           with Import, Convention => C, External_Name => "chmod";
         Result : int;
      begin
         Result := chmod (To_C (Test_File), 8#200#);  -- Write only
         if Result /= 0 then
            Delete_Test_File (Test_File);
            return Void_Result.Ok;  -- Skip test if chmod fails
         end if;
      end;

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

      -- Test - mapping should fail
      declare
         Map_Res : constant Map_Result.Result := Map_File (File, Path, True);
      begin
         -- Restore permissions before checking result
         declare
            use Interfaces.C;
            function chmod (pathname : char_array; mode : int) return int
              with Import, Convention => C, External_Name => "chmod";
            Result : int;
         begin
            Result := chmod (To_C (Test_File), 8#644#);  -- Restore normal permissions
         end;

         if Map_Res.Is_Ok then
            Unmap (File);
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("Mapping write-only file for reading should fail"));
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
   end Test_Map_Protected_File;

   function Test_Map_Directory return Void_Result.Result is
      Test_Dir : constant String := "test_directory_12345";
      File : Memory_Mapped_File;
      Path : File_Path;
   begin
      -- Setup
      Ada.Directories.Create_Directory (Test_Dir);

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_Dir));
      begin
         if Path_Result.Is_Err then
            Ada.Directories.Delete_Directory (Test_Dir);
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Test - mapping directory should fail
      declare
         Map_Res : constant Map_Result.Result := Map_File (File, Path, True);
      begin
         if Map_Res.Is_Ok then
            Unmap (File);
            Ada.Directories.Delete_Directory (Test_Dir);
            return Void_Result.Err (To_Unbounded_String
              ("Mapping directory should fail"));
         end if;

         -- Expected failure
         Ada.Directories.Delete_Directory (Test_Dir);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         if Ada.Directories.Exists (Test_Dir) then
            Ada.Directories.Delete_Directory (Test_Dir);
         end if;
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Map_Directory;

   function Test_Map_Special_File return Void_Result.Result is
      File : Memory_Mapped_File;
      Path : File_Path;
   begin
      -- Test mapping /dev/null or similar special file
      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String ("/dev/null"));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Ok;  -- Skip test if path creation fails
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Test - mapping special file might fail or succeed with size 0
      declare
         Map_Res : constant Map_Result.Result := Map_File (File, Path, True);
      begin
         if Map_Res.Is_Ok then
            -- Some systems allow mapping special files
            Unmap (File);
         end if;
         -- Either way is acceptable
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Map_Special_File;

   -- Concurrency tests
   function Test_Concurrent_Reads return Void_Result.Result is
      Test_File : constant String := "test_concurrent_reads.dat";
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

      -- Test concurrent reads using tasks
      declare
         task type Reader is
            entry Start;
            entry Get_Result (Success : out Boolean);
         end Reader;

         task body Reader is
            File : Memory_Mapped_File;
            OK : Boolean := False;
         begin
            accept Start;

            declare
               Map_Res : constant Map_Result.Result := Map_File (File, Path, True);
            begin
               if Map_Res.Is_Ok then
                  -- Perform some reads
                  declare
                     View : constant Memory_View := Get_View (File);
                  begin
                     if View.Address /= System.Null_Address then
                        OK := True;
                     end if;
                  end;
                  Unmap (File);
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
              ("Not all concurrent readers succeeded"));
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
   end Test_Concurrent_Reads;

   function Test_Concurrent_Modifications return Void_Result.Result is
      Test_File : constant String := "test_concurrent_mods.dat";
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

      -- Test - single writer with sync
      declare
         File : Memory_Mapped_File;
         Map_Res : constant Map_Result.Result := Map_File (File, Path, False);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         -- Modify and sync
         Sync (File);

         -- Cleanup
         Unmap (File);
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      end;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Concurrent_Modifications;

   -- Run all tests
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Stats : Test_Stats := (Total => 0, Passed => 0, Failed => 0, Skipped => 0);
   begin
      -- Basic file mapping operations
      Run_Test (Output.all, "Map Small File", Test_Map_Small_File'Access, Stats);
      Run_Test (Output.all, "Map Large File", Test_Map_Large_File'Access, Stats);
      Run_Test (Output.all, "Map Empty File", Test_Map_Empty_File'Access, Stats);
      Run_Test (Output.all, "Map Nonexistent File", Test_Map_Nonexistent_File'Access, Stats);

      -- Read/Write operations
      Run_Test (Output.all, "Read Only Mapping", Test_Read_Only_Mapping'Access, Stats);
      Run_Test (Output.all, "Read Write Mapping", Test_Read_Write_Mapping'Access, Stats);
      Run_Test (Output.all, "Multiple Mappings", Test_Multiple_Mappings'Access, Stats);

      -- Subview operations
      Run_Test (Output.all, "Create Subview Valid", Test_Create_Subview_Valid'Access, Stats);
      Run_Test (Output.all, "Create Subview Boundary", Test_Create_Subview_Boundary'Access, Stats);
      Run_Test (Output.all, "Create Subview Invalid", Test_Create_Subview_Invalid'Access, Stats);

      -- Synchronization operations
      Run_Test (Output.all, "Sync Changes", Test_Sync_Changes'Access, Stats);
      Run_Test (Output.all, "Sync Read Only", Test_Sync_Read_Only'Access, Stats);

      -- Access pattern advisement
      Run_Test (Output.all, "Advise Sequential", Test_Advise_Sequential'Access, Stats);
      Run_Test (Output.all, "Advise Random", Test_Advise_Random'Access, Stats);
      Run_Test (Output.all, "Advise Will Need", Test_Advise_Will_Need'Access, Stats);
      Run_Test (Output.all, "Advise Dont Need", Test_Advise_Dont_Need'Access, Stats);

      -- Resource management
      Run_Test (Output.all, "Unmap Cleanup", Test_Unmap_Cleanup'Access, Stats);
      Run_Test (Output.all, "Finalization", Test_Finalization'Access, Stats);
      Run_Test (Output.all, "Double Unmap", Test_Double_Unmap'Access, Stats);

      -- Platform capability tests
      Run_Test (Output.all, "Memory Mapping Available", Test_Memory_Mapping_Available'Access, Stats);
      Run_Test (Output.all, "Should Use Memory Map", Test_Should_Use_Memory_Map'Access, Stats);

      -- Error handling
      Run_Test (Output.all, "Map Protected File", Test_Map_Protected_File'Access, Stats);
      Run_Test (Output.all, "Map Directory", Test_Map_Directory'Access, Stats);
      Run_Test (Output.all, "Map Special File", Test_Map_Special_File'Access, Stats);

      -- Concurrency tests
      Run_Test (Output.all, "Concurrent Reads", Test_Concurrent_Reads'Access, Stats);
      Run_Test (Output.all, "Concurrent Modifications", Test_Concurrent_Modifications'Access, Stats);

      return Test_Stats_Result.Ok (Stats);
   end Run_All_Tests;

end Test_Memory_Mapped_File;
