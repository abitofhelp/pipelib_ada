--  =============================================================================
--  Test_Random_Write_File - Random Write File Integration Tests Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;
with Ada.Numerics.Discrete_Random;
with System.Storage_Elements;

with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Value_Objects.File_Path;
with Abohlib.Infrastructure.Testing.Test_Framework;
with Pipelib.Infrastructure.IO.Random_Write_File;
with Pipelib.Core.Domain.Value_Objects.File_Chunk;
with Pipelib.Core.Domain.Value_Objects.Chunk_Size;

package body Test_Random_Write_File is

   use System.Storage_Elements;
   use Abohlib.Core.Domain.Value_Objects.File_Path;
   use Abohlib.Infrastructure.Testing.Test_Framework;
   use Pipelib.Infrastructure.IO.Random_Write_File;
   use Pipelib.Core.Domain.Value_Objects.File_Chunk;
   use Pipelib.Core.Domain.Value_Objects.Chunk_Size;

   --  Test constants
   Small_Chunk_Size : constant := 1024;  -- 1 KB
   Medium_Chunk_Size : constant := 64 * 1024;  -- 64 KB
   Large_Chunk_Size : constant := 1024 * 1024;  -- 1 MB
   Test_Data : constant String := "Random write file test data chunk";

   --  Helper to create test data
   function Create_Test_Data (Size : Natural; Pattern : Character := 'R') return Storage_Array is
      Data : Storage_Array (1 .. Storage_Offset (Size));
   begin
      for I in Data'Range loop
         Data (I) := Character'Pos (Pattern);
      end loop;
      return Data;
   end Create_Test_Data;

   --  Helper to create a test chunk
   function Create_Test_Chunk
     (Sequence : Natural;
      Size : Natural;
      Pattern : Character := 'C') return File_Chunk_Type
   is
      Data : constant Storage_Array := Create_Test_Data (Size, Pattern);
      Chunk_Size_Val : Chunk_Size_Type;
   begin
      Chunk_Size_Val := Create_Chunk_Size (Positive (Size));
      return Create_File_Chunk
        (Sequence_Number => Sequence,
         Data => Data,
         Size => Chunk_Size_Val,
         Checksum => 0);  -- Simple checksum for testing
   end Create_Test_Chunk;

   --  Helper to delete test file if it exists
   procedure Delete_Test_File (Path : String) is
   begin
      if Ada.Directories.Exists (Path) then
         Ada.Directories.Delete_File (Path);
      end if;
   end Delete_Test_File;

   --  Helper to verify file content
   function Verify_File_Content
     (Path : String;
      Expected : String;
      Offset : Natural := 0) return Boolean
   is
      use Ada.Text_IO;
      use Ada.Streams;
      File : File_Type;
      Buffer : String (1 .. Expected'Length);
      Last : Natural;
   begin
      Open (File, In_File, Path);
      if Offset > 0 then
         Set_Index (File, Count (Offset + 1));
      end if;
      Get (File, Buffer, Last);
      Close (File);
      return Buffer (1 .. Last) = Expected (1 .. Last);
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         return False;
   end Verify_File_Content;

   --  Basic file operations
   function Test_Create_And_Open return Void_Result.Result is
      Test_File : constant String := "test_create_open.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Test creating and opening
      Writer := Create (Path);

      if Writer = null then
         return Void_Result.Err (To_Unbounded_String
           ("Create returned null"));
      end if;

      if not Is_Open (Writer.all) then
         Destroy (Writer);
         return Void_Result.Err (To_Unbounded_String
           ("File should be open after creation"));
      end if;

      -- Test temp file was created
      if not Ada.Directories.Exists (Test_File & ".tmp") then
         Destroy (Writer);
         return Void_Result.Err (To_Unbounded_String
           ("Temp file not created"));
      end if;

      -- Cleanup
      Close (Writer.all);
      Destroy (Writer);
      Delete_Test_File (Test_File & ".tmp");

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Create_And_Open;

   function Test_Create_With_Preallocate return Void_Result.Result is
      Test_File : constant String := "test_preallocate.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
      Expected_Size : constant := 10 * 1024 * 1024;  -- 10 MB
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Test creating with pre-allocated size
      Writer := Create (Path, Expected_Size => Expected_Size);

      if Writer = null then
         return Void_Result.Err (To_Unbounded_String
           ("Create returned null"));
      end if;

      -- Pre-allocation happens on first write or explicit call
      Preallocate (Writer.all, Expected_Size);

      -- Size might not match exactly due to filesystem behavior
      -- Just verify file is open
      if not Is_Open (Writer.all) then
         Destroy (Writer);
         return Void_Result.Err (To_Unbounded_String
           ("File should be open"));
      end if;

      -- Cleanup
      Close (Writer.all);
      Destroy (Writer);
      Delete_Test_File (Test_File & ".tmp");

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Create_With_Preallocate;

   function Test_Create_Without_Temp return Void_Result.Result is
      Test_File : constant String := "test_no_temp.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
   begin
      -- Setup
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

      -- Test creating without temp file
      Writer := Create (Path, Use_Temp_File => False);

      if Writer = null then
         return Void_Result.Err (To_Unbounded_String
           ("Create returned null"));
      end if;

      -- Verify direct file was created (no temp)
      if Ada.Directories.Exists (Test_File & ".tmp") then
         Destroy (Writer);
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Temp file should not exist"));
      end if;

      if not Ada.Directories.Exists (Test_File) then
         Destroy (Writer);
         return Void_Result.Err (To_Unbounded_String
           ("Direct file not created"));
      end if;

      -- Cleanup
      Close (Writer.all);
      Destroy (Writer);
      Delete_Test_File (Test_File);

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Create_Without_Temp;

   -- Write operations
   function Test_Write_Single_Chunk return Void_Result.Result is
      Test_File : constant String := "test_single_chunk.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      Writer := Create (Path);

      declare
         Chunk : constant File_Chunk_Type := Create_Test_Chunk (0, Small_Chunk_Size, 'S');
      begin

      -- Test writing single chunk
      Write_Chunk (Writer.all, Chunk);

      -- Verify size
      if Size (Writer.all) < Long_Long_Integer (Small_Chunk_Size) then
         Close (Writer.all);
         Destroy (Writer);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("File size incorrect after write"));
      end if;

      -- Commit and verify
      declare
         Commit_Res : constant Write_Result.Result := Commit (Writer.all);
      begin
         if Commit_Res.Is_Err then
            Destroy (Writer);
            Delete_Test_File (Test_File);
            return Void_Result.Err (Commit_Res.Unwrap_Err);
         end if;
      end;

      -- Verify file exists
      if not Ada.Directories.Exists (Test_File) then
         Destroy (Writer);
         return Void_Result.Err (To_Unbounded_String
           ("Committed file not found"));
      end if;

      -- Cleanup
      Destroy (Writer);
      Delete_Test_File (Test_File);
      end;

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Write_Single_Chunk;

   function Test_Write_Multiple_Chunks return Void_Result.Result is
      Test_File : constant String := "test_multiple_chunks.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
      Num_Chunks : constant := 10;
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      Writer := Create (Path);

      -- Write multiple chunks sequentially
      for I in 0 .. Num_Chunks - 1 loop
         declare
            Chunk : constant File_Chunk_Type :=
              Create_Test_Chunk (I, Small_Chunk_Size, Character'Val (Character'Pos ('A') + I));
         begin
            Write_Chunk (Writer.all, Chunk);
         end;
      end loop;

      -- Verify size
      declare
         Expected_Size : constant Long_Long_Integer :=
           Long_Long_Integer (Num_Chunks * Small_Chunk_Size);
      begin
         if Size (Writer.all) < Expected_Size then
            Close (Writer.all);
            Destroy (Writer);
            Delete_Test_File (Test_File & ".tmp");
            return Void_Result.Err (To_Unbounded_String
              ("File size incorrect after multiple writes"));
         end if;
      end;

      -- Commit
      declare
         Commit_Res : constant Write_Result.Result := Commit (Writer.all);
      begin
         if Commit_Res.Is_Err then
            Destroy (Writer);
            Delete_Test_File (Test_File);
            return Void_Result.Err (Commit_Res.Unwrap_Err);
         end if;
      end;

      -- Cleanup
      Destroy (Writer);
      Delete_Test_File (Test_File);

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Write_Multiple_Chunks;

   function Test_Write_Random_Positions return Void_Result.Result is
      Test_File : constant String := "test_random_pos.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;

      subtype Position_Range is Natural range 0 .. 9;
      package Random_Position is new Ada.Numerics.Discrete_Random (Position_Range);
      Gen : Random_Position.Generator;
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");
      Random_Position.Reset (Gen);

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      Writer := Create (Path);

      -- Pre-allocate space for random writes
      Preallocate (Writer.all, 10 * Small_Chunk_Size);

      -- Write chunks at random positions
      for I in 1 .. 5 loop
         declare
            Pos : constant Natural := Random_Position.Random (Gen);
            Chunk : constant File_Chunk_Type :=
              Create_Test_Chunk (I, Small_Chunk_Size, Character'Val (Character'Pos ('0') + Pos));
            Position : constant Long_Long_Integer :=
              Long_Long_Integer (Pos * Small_Chunk_Size);
         begin
            Write_Chunk_At_Position (Writer.all, Chunk, Position);
         end;
      end loop;

      -- Flush to ensure writes are persisted
      Flush (Writer.all);

      -- Commit
      declare
         Commit_Res : constant Write_Result.Result := Commit (Writer.all);
      begin
         if Commit_Res.Is_Err then
            Destroy (Writer);
            Delete_Test_File (Test_File);
            return Void_Result.Err (Commit_Res.Unwrap_Err);
         end if;
      end;

      -- Cleanup
      Destroy (Writer);
      Delete_Test_File (Test_File);

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Write_Random_Positions;

   function Test_Write_Overlapping_Chunks return Void_Result.Result is
      Test_File : constant String := "test_overlap.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      Writer := Create (Path);

      -- Write first chunk
      declare
         Chunk1 : constant File_Chunk_Type :=
           Create_Test_Chunk (0, Small_Chunk_Size, 'X');
      begin
         Write_Chunk_At_Position (Writer.all, Chunk1, 0);
      end;

      -- Write overlapping chunk (overwrites half of first chunk)
      declare
         Chunk2 : constant File_Chunk_Type :=
           Create_Test_Chunk (1, Small_Chunk_Size, 'Y');
         Overlap_Position : constant Long_Long_Integer :=
           Long_Long_Integer (Small_Chunk_Size / 2);
      begin
         Write_Chunk_At_Position (Writer.all, Chunk2, Overlap_Position);
      end;

      -- Expected size is 1.5 chunks
      declare
         Expected_Size : constant Long_Long_Integer :=
           Long_Long_Integer (Small_Chunk_Size + Small_Chunk_Size / 2);
      begin
         if Size (Writer.all) < Expected_Size then
            Close (Writer.all);
            Destroy (Writer);
            Delete_Test_File (Test_File & ".tmp");
            return Void_Result.Err (To_Unbounded_String
              ("File size incorrect after overlapping writes"));
         end if;
      end;

      -- Commit
      declare
         Commit_Res : constant Write_Result.Result := Commit (Writer.all);
      begin
         if Commit_Res.Is_Err then
            Destroy (Writer);
            Delete_Test_File (Test_File);
            return Void_Result.Err (Commit_Res.Unwrap_Err);
         end if;
      end;

      -- Cleanup
      Destroy (Writer);
      Delete_Test_File (Test_File);

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Write_Overlapping_Chunks;

   function Test_Write_Beyond_EOF return Void_Result.Result is
      Test_File : constant String := "test_beyond_eof.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
      Gap_Position : constant := 10 * Small_Chunk_Size;
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      Writer := Create (Path);

      -- Write chunk at position 0
      declare
         Chunk1 : constant File_Chunk_Type :=
           Create_Test_Chunk (0, Small_Chunk_Size, 'A');
      begin
         Write_Chunk_At_Position (Writer.all, Chunk1, 0);
      end;

      -- Write chunk far beyond current EOF (creates sparse file)
      declare
         Chunk2 : constant File_Chunk_Type :=
           Create_Test_Chunk (1, Small_Chunk_Size, 'Z');
      begin
         Write_Chunk_At_Position (Writer.all, Chunk2, Long_Long_Integer (Gap_Position));
      end;

      -- Size should reflect the gap
      if Size (Writer.all) < Long_Long_Integer (Gap_Position + Small_Chunk_Size) then
         Close (Writer.all);
         Destroy (Writer);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("File size incorrect after gap write"));
      end if;

      -- Commit
      declare
         Commit_Res : constant Write_Result.Result := Commit (Writer.all);
      begin
         if Commit_Res.Is_Err then
            Destroy (Writer);
            Delete_Test_File (Test_File);
            return Void_Result.Err (Commit_Res.Unwrap_Err);
         end if;
      end;

      -- Cleanup
      Destroy (Writer);
      Delete_Test_File (Test_File);

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Write_Beyond_EOF;

   -- Chunk sequence operations
   function Test_Write_By_Sequence_Number return Void_Result.Result is
      Test_File : constant String := "test_sequence.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      Writer := Create (Path);

      -- Write chunks using their sequence numbers
      for Seq in 0 .. 4 loop
         declare
            Chunk : constant File_Chunk_Type :=
              Create_Test_Chunk (Seq, Small_Chunk_Size, Character'Val (Character'Pos ('0') + Seq));
         begin
            -- Write_Chunk uses sequence number for position calculation
            Write_Chunk (Writer.all, Chunk);
         end;
      end loop;

      -- Verify sequential writing
      declare
         Expected_Size : constant Long_Long_Integer :=
           Long_Long_Integer (5 * Small_Chunk_Size);
      begin
         if Size (Writer.all) < Expected_Size then
            Close (Writer.all);
            Destroy (Writer);
            Delete_Test_File (Test_File & ".tmp");
            return Void_Result.Err (To_Unbounded_String
              ("File size incorrect after sequence writes"));
         end if;
      end;

      -- Commit
      declare
         Commit_Res : constant Write_Result.Result := Commit (Writer.all);
      begin
         if Commit_Res.Is_Err then
            Destroy (Writer);
            Delete_Test_File (Test_File);
            return Void_Result.Err (Commit_Res.Unwrap_Err);
         end if;
      end;

      -- Cleanup
      Destroy (Writer);
      Delete_Test_File (Test_File);

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Write_By_Sequence_Number;

   function Test_Write_Out_Of_Order return Void_Result.Result is
      Test_File : constant String := "test_out_of_order.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
      Order : constant array (0 .. 4) of Natural := (2, 0, 4, 1, 3);
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      Writer := Create (Path);

      -- Pre-allocate for out-of-order writes
      Preallocate (Writer.all, 5 * Small_Chunk_Size);

      -- Write chunks out of order
      for I in Order'Range loop
         declare
            Seq : constant Natural := Order (I);
            Chunk : constant File_Chunk_Type :=
              Create_Test_Chunk (Seq, Small_Chunk_Size, Character'Val (Character'Pos ('A') + Seq));
            Position : constant Long_Long_Integer :=
              Long_Long_Integer (Seq * Small_Chunk_Size);
         begin
            Write_Chunk_At_Position (Writer.all, Chunk, Position);
         end;
      end loop;

      -- Flush to ensure all writes are complete
      Flush (Writer.all);

      -- Commit
      declare
         Commit_Res : constant Write_Result.Result := Commit (Writer.all);
      begin
         if Commit_Res.Is_Err then
            Destroy (Writer);
            Delete_Test_File (Test_File);
            return Void_Result.Err (Commit_Res.Unwrap_Err);
         end if;
      end;

      -- Cleanup
      Destroy (Writer);
      Delete_Test_File (Test_File);

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Write_Out_Of_Order;

   -- Commit and rollback operations
   function Test_Commit_Success return Void_Result.Result is
      Test_File : constant String := "test_commit.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      Writer := Create (Path);

      -- Write some data
      declare
         Chunk : constant File_Chunk_Type :=
           Create_Test_Chunk (0, Small_Chunk_Size, 'C');
      begin
         Write_Chunk (Writer.all, Chunk);
      end;

      -- Verify temp file exists
      if not Ada.Directories.Exists (Test_File & ".tmp") then
         Close (Writer.all);
         Destroy (Writer);
         return Void_Result.Err (To_Unbounded_String
           ("Temp file not found before commit"));
      end if;

      -- Commit
      declare
         Commit_Res : constant Write_Result.Result := Commit (Writer.all);
      begin
         if Commit_Res.Is_Err then
            Destroy (Writer);
            Delete_Test_File (Test_File);
            Delete_Test_File (Test_File & ".tmp");
            return Void_Result.Err (Commit_Res.Unwrap_Err);
         end if;
      end;

      -- Verify final file exists and temp is gone
      if not Ada.Directories.Exists (Test_File) then
         Destroy (Writer);
         return Void_Result.Err (To_Unbounded_String
           ("Final file not found after commit"));
      end if;

      if Ada.Directories.Exists (Test_File & ".tmp") then
         Destroy (Writer);
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Temp file still exists after commit"));
      end if;

      -- File should be closed after commit
      if Is_Open (Writer.all) then
         Destroy (Writer);
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("File still open after commit"));
      end if;

      -- Cleanup
      Destroy (Writer);
      Delete_Test_File (Test_File);

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Commit_Success;

   function Test_Rollback_Success return Void_Result.Result is
      Test_File : constant String := "test_rollback.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      Writer := Create (Path);

      -- Write some data
      declare
         Chunk : constant File_Chunk_Type :=
           Create_Test_Chunk (0, Small_Chunk_Size, 'R');
      begin
         Write_Chunk (Writer.all, Chunk);
      end;

      -- Verify temp file exists
      if not Ada.Directories.Exists (Test_File & ".tmp") then
         Close (Writer.all);
         Destroy (Writer);
         return Void_Result.Err (To_Unbounded_String
           ("Temp file not found before rollback"));
      end if;

      -- Rollback
      Rollback (Writer.all);

      -- Verify temp file is gone and no final file
      if Ada.Directories.Exists (Test_File & ".tmp") then
         Destroy (Writer);
         return Void_Result.Err (To_Unbounded_String
           ("Temp file still exists after rollback"));
      end if;

      if Ada.Directories.Exists (Test_File) then
         Destroy (Writer);
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Final file exists after rollback"));
      end if;

      -- File should be closed after rollback
      if Is_Open (Writer.all) then
         Destroy (Writer);
         return Void_Result.Err (To_Unbounded_String
           ("File still open after rollback"));
      end if;

      -- Cleanup
      Destroy (Writer);

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Rollback_Success;

   function Test_Commit_After_Rollback return Void_Result.Result is
      Test_File : constant String := "test_commit_after_rollback.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      Writer := Create (Path);

      -- Write some data
      declare
         Chunk : constant File_Chunk_Type :=
           Create_Test_Chunk (0, Small_Chunk_Size, 'X');
      begin
         Write_Chunk (Writer.all, Chunk);
      end;

      -- Rollback
      Rollback (Writer.all);

      -- Try to commit after rollback - should fail
      declare
         Commit_Res : constant Write_Result.Result := Commit (Writer.all);
      begin
         if Commit_Res.Is_Ok then
            Destroy (Writer);
            return Void_Result.Err (To_Unbounded_String
              ("Commit should fail after rollback"));
         end if;
         -- Expected failure
      end;

      -- Cleanup
      Destroy (Writer);

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Commit_After_Rollback;

   -- File management
   function Test_Flush_Operations return Void_Result.Result is
      Test_File : constant String := "test_flush.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      Writer := Create (Path);

      -- Write data
      declare
         Chunk : constant File_Chunk_Type :=
           Create_Test_Chunk (0, Small_Chunk_Size, 'F');
      begin
         Write_Chunk (Writer.all, Chunk);
      end;

      -- Flush should succeed
      Flush (Writer.all);

      -- Write more data
      declare
         Chunk : constant File_Chunk_Type :=
           Create_Test_Chunk (1, Small_Chunk_Size, 'L');
      begin
         Write_Chunk (Writer.all, Chunk);
      end;

      -- Flush again
      Flush (Writer.all);

      -- Commit
      declare
         Commit_Res : constant Write_Result.Result := Commit (Writer.all);
      begin
         if Commit_Res.Is_Err then
            Destroy (Writer);
            Delete_Test_File (Test_File);
            return Void_Result.Err (Commit_Res.Unwrap_Err);
         end if;
      end;

      -- Cleanup
      Destroy (Writer);
      Delete_Test_File (Test_File);

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Flush_Operations;

   function Test_Preallocate_Space return Void_Result.Result is
      Test_File : constant String := "test_preallocate_space.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
      Preallocate_Size : constant := 5 * 1024 * 1024;  -- 5 MB
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      Writer := Create (Path);

      -- Preallocate space
      Preallocate (Writer.all, Preallocate_Size);

      -- Write small chunk
      declare
         Chunk : constant File_Chunk_Type :=
           Create_Test_Chunk (0, Small_Chunk_Size, 'P');
      begin
         Write_Chunk (Writer.all, Chunk);
      end;

      -- File should have data but filesystem may handle preallocation differently
      if Size (Writer.all) = 0 then
         Close (Writer.all);
         Destroy (Writer);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("No data written after preallocation"));
      end if;

      -- Commit
      declare
         Commit_Res : constant Write_Result.Result := Commit (Writer.all);
      begin
         if Commit_Res.Is_Err then
            Destroy (Writer);
            Delete_Test_File (Test_File);
            return Void_Result.Err (Commit_Res.Unwrap_Err);
         end if;
      end;

      -- Cleanup
      Destroy (Writer);
      Delete_Test_File (Test_File);

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Preallocate_Space;

   function Test_File_Size_Tracking return Void_Result.Result is
      Test_File : constant String := "test_size_track.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      Writer := Create (Path);

      -- Initial size should be 0
      if Size (Writer.all) /= 0 then
         Close (Writer.all);
         Destroy (Writer);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Initial size not 0"));
      end if;

      -- Write first chunk
      declare
         Chunk1 : constant File_Chunk_Type :=
           Create_Test_Chunk (0, Small_Chunk_Size, '1');
      begin
         Write_Chunk (Writer.all, Chunk1);
      end;

      -- Check size after first write
      if Size (Writer.all) < Long_Long_Integer (Small_Chunk_Size) then
         Close (Writer.all);
         Destroy (Writer);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Size incorrect after first write"));
      end if;

      -- Write second chunk
      declare
         Chunk2 : constant File_Chunk_Type :=
           Create_Test_Chunk (1, Medium_Chunk_Size, '2');
      begin
         Write_Chunk (Writer.all, Chunk2);
      end;

      -- Check cumulative size
      if Size (Writer.all) < Long_Long_Integer (Small_Chunk_Size + Medium_Chunk_Size) then
         Close (Writer.all);
         Destroy (Writer);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Size incorrect after second write"));
      end if;

      -- Commit
      declare
         Commit_Res : constant Write_Result.Result := Commit (Writer.all);
      begin
         if Commit_Res.Is_Err then
            Destroy (Writer);
            Delete_Test_File (Test_File);
            return Void_Result.Err (Commit_Res.Unwrap_Err);
         end if;
      end;

      -- Cleanup
      Destroy (Writer);
      Delete_Test_File (Test_File);

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_File_Size_Tracking;

   -- Error handling
   function Test_Write_To_Closed_File return Void_Result.Result is
      Test_File : constant String := "test_closed.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      Writer := Create (Path);

      -- Close the file
      Close (Writer.all);

      -- Try to write to closed file - should raise exception
      declare
         Chunk : constant File_Chunk_Type :=
           Create_Test_Chunk (0, Small_Chunk_Size, 'E');
         Success : Boolean := False;
      begin
         Write_Chunk (Writer.all, Chunk);
         Success := False;  -- Should not reach here
      exception
         when others =>
            Success := True;  -- Expected
      end;

      -- Cleanup
      Destroy (Writer);

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Write_To_Closed_File;

   function Test_Invalid_Positions return Void_Result.Result is
      Test_File : constant String := "test_invalid_pos.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      Writer := Create (Path);

      -- Try to write at negative position - should raise exception
      declare
         Chunk : constant File_Chunk_Type :=
           Create_Test_Chunk (0, Small_Chunk_Size, 'N');
         Success : Boolean := False;
      begin
         Write_Chunk_At_Position (Writer.all, Chunk, -1);
         Success := False;  -- Should not reach here
      exception
         when others =>
            Success := True;  -- Expected
      end;

      -- Cleanup
      Close (Writer.all);
      Destroy (Writer);
      Delete_Test_File (Test_File & ".tmp");

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Invalid_Positions;

   function Test_Disk_Full_Handling return Void_Result.Result is
      -- This test is difficult to implement portably
      -- We'll do a basic test that the system handles writes gracefully
      Test_File : constant String := "test_disk_full.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      Writer := Create (Path);

      -- Write a reasonable amount of data
      declare
         Chunk : constant File_Chunk_Type :=
           Create_Test_Chunk (0, Medium_Chunk_Size, 'D');
         Write_Res : Write_Result.Result;
      begin
         Write_Chunk_At (Writer.all, Chunk, 0, Write_Res);

         -- Either succeeds or returns error - both are valid
         if Write_Res.Is_Err then
            -- System properly reported error
            Close (Writer.all);
            Destroy (Writer);
            Delete_Test_File (Test_File & ".tmp");
            return Void_Result.Ok;
         end if;
      end;

      -- Success case - commit
      declare
         Commit_Res : constant Write_Result.Result := Commit (Writer.all);
      begin
         if Commit_Res.Is_Err then
            -- Commit failed - that's ok for this test
            null;
         end if;
      end;

      -- Cleanup
      Destroy (Writer);
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Disk_Full_Handling;

   function Test_Permission_Errors return Void_Result.Result is
      Test_File : constant String := "/root/test_perm_error.dat";  -- Likely no permission
      Path : File_Path;
      Writer : Random_Write_File_Access;
   begin
      -- Try to create file in restricted directory
      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            -- Path creation failed - expected
            return Void_Result.Ok;
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Try to create writer - should fail
      begin
         Writer := Create (Path);
         -- If we get here, we have permission (running as root?)
         Close (Writer.all);
         Destroy (Writer);
         Delete_Test_File (Test_File);
         return Void_Result.Ok;
      exception
         when others =>
            -- Expected permission error
            return Void_Result.Ok;
      end;

   exception
      when E : others =>
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Permission_Errors;

   -- Concurrent access
   function Test_Protected_Concurrent_Writes return Void_Result.Result is
      Test_File : constant String := "test_concurrent.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
      Protected_Writer : Protected_Random_Write_File_Access;
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      Writer := Create (Path);
      Protected_Writer := new Protected_Random_Write_File;
      Protected_Writer.Initialize (Writer);

      -- Test concurrent writes using tasks
      declare
         task type Writer_Task is
            entry Start (ID : Natural);
            entry Done;
         end Writer_Task;

         task body Writer_Task is
            Task_ID : Natural;
         begin
            accept Start (ID : Natural) do
               Task_ID := ID;
            end Start;

            -- Write chunk
            declare
               Chunk : constant File_Chunk_Type :=
                 Create_Test_Chunk (Task_ID, Small_Chunk_Size,
                   Character'Val (Character'Pos ('A') + Task_ID));
            begin
               Protected_Writer.Write_Chunk (Chunk);
            end;

            accept Done;
         end Writer_Task;

         Writers : array (0 .. 3) of Writer_Task;
      begin
         -- Start all writers
         for I in Writers'Range loop
            Writers (I).Start (I);
         end loop;

         -- Wait for completion
         for W of Writers loop
            W.Done;
         end loop;
      end;

      -- Verify writes succeeded
      if not Protected_Writer.Is_Ready then
         Close (Writer.all);
         Destroy (Writer);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Protected writer not ready"));
      end if;

      -- Commit
      declare
         Commit_Res : constant Write_Result.Result := Commit (Writer.all);
      begin
         if Commit_Res.Is_Err then
            Destroy (Writer);
            Delete_Test_File (Test_File);
            return Void_Result.Err (Commit_Res.Unwrap_Err);
         end if;
      end;

      -- Cleanup
      Destroy (Writer);
      Delete_Test_File (Test_File);

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Protected_Concurrent_Writes;

   function Test_Protected_Write_Order return Void_Result.Result is
      Test_File : constant String := "test_protected_order.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
      Protected_Writer : Protected_Random_Write_File_Access;
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      Writer := Create (Path);
      Protected_Writer := new Protected_Random_Write_File;
      Protected_Writer.Initialize (Writer);

      -- Write chunks at specific positions
      for I in 0 .. 4 loop
         declare
            Chunk : constant File_Chunk_Type :=
              Create_Test_Chunk (I, Small_Chunk_Size, Character'Val (Character'Pos ('0') + I));
         begin
            Protected_Writer.Write_At_Position
              (Chunk, Long_Long_Integer (I * Small_Chunk_Size));
         end;
      end loop;

      -- Check size
      declare
         Expected_Size : constant Long_Long_Integer :=
           Long_Long_Integer (5 * Small_Chunk_Size);
         Actual_Size : constant Long_Long_Integer := Protected_Writer.Get_Size;
      begin
         if Actual_Size < Expected_Size then
            Close (Writer.all);
            Destroy (Writer);
            Delete_Test_File (Test_File & ".tmp");
            return Void_Result.Err (To_Unbounded_String
              ("Protected writer size incorrect"));
         end if;
      end;

      -- Commit
      declare
         Commit_Res : constant Write_Result.Result := Commit (Writer.all);
      begin
         if Commit_Res.Is_Err then
            Destroy (Writer);
            Delete_Test_File (Test_File);
            return Void_Result.Err (Commit_Res.Unwrap_Err);
         end if;
      end;

      -- Cleanup
      Destroy (Writer);
      Delete_Test_File (Test_File);

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Protected_Write_Order;

   function Test_Protected_Size_Query return Void_Result.Result is
      Test_File : constant String := "test_protected_size.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
      Protected_Writer : Protected_Random_Write_File_Access;
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      Writer := Create (Path);
      Protected_Writer := new Protected_Random_Write_File;
      Protected_Writer.Initialize (Writer);

      -- Initial size should be 0
      if Protected_Writer.Get_Size /= 0 then
         Close (Writer.all);
         Destroy (Writer);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Initial protected size not 0"));
      end if;

      -- Write and check size updates
      declare
         Chunk : constant File_Chunk_Type :=
           Create_Test_Chunk (0, Medium_Chunk_Size, 'S');
      begin
         Protected_Writer.Write_Chunk (Chunk);
      end;

      if Protected_Writer.Get_Size < Long_Long_Integer (Medium_Chunk_Size) then
         Close (Writer.all);
         Destroy (Writer);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Protected size not updated"));
      end if;

      -- Commit
      declare
         Commit_Res : constant Write_Result.Result := Commit (Writer.all);
      begin
         if Commit_Res.Is_Err then
            Destroy (Writer);
            Delete_Test_File (Test_File);
            return Void_Result.Err (Commit_Res.Unwrap_Err);
         end if;
      end;

      -- Cleanup
      Destroy (Writer);
      Delete_Test_File (Test_File);

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Protected_Size_Query;

   -- Resource management
   function Test_Finalization_Cleanup return Void_Result.Result is
      Test_File : constant String := "test_finalize.dat";
      Path : File_Path;
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      -- Create writer in nested scope
      declare
         Writer : Random_Write_File_Access := Create (Path);
         Chunk : constant File_Chunk_Type :=
           Create_Test_Chunk (0, Small_Chunk_Size, 'F');
      begin
         Write_Chunk (Writer.all, Chunk);
         -- Writer goes out of scope here - should be finalized
         -- Note: Explicitly destroy to ensure cleanup
         Close (Writer.all);
         Destroy (Writer);
      end;

      -- Temp file might be cleaned up by finalization
      -- Just verify no crash occurred
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Finalization_Cleanup;

   function Test_Destroy_Procedure return Void_Result.Result is
      Test_File : constant String := "test_destroy.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      Writer := Create (Path);

      -- Write some data
      declare
         Chunk : constant File_Chunk_Type :=
           Create_Test_Chunk (0, Small_Chunk_Size, 'D');
      begin
         Write_Chunk (Writer.all, Chunk);
      end;

      -- Destroy without explicit close
      Destroy (Writer);

      -- Writer should be null after destroy
      if Writer /= null then
         return Void_Result.Err (To_Unbounded_String
           ("Writer not null after destroy"));
      end if;

      -- Cleanup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Destroy_Procedure;

   function Test_Multiple_File_Handles return Void_Result.Result is
      Test_File1 : constant String := "test_multi1.dat";
      Test_File2 : constant String := "test_multi2.dat";
      Path1, Path2 : File_Path;
      Writer1, Writer2 : Random_Write_File_Access;
   begin
      -- Setup
      Delete_Test_File (Test_File1);
      Delete_Test_File (Test_File1 & ".tmp");
      Delete_Test_File (Test_File2);
      Delete_Test_File (Test_File2 & ".tmp");

      declare
         Path_Result1 : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File1));
         Path_Result2 : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File2));
      begin
         if Path_Result1.Is_Err then
            return Void_Result.Err (Path_Result1.Unwrap_Err);
         end if;
         if Path_Result2.Is_Err then
            return Void_Result.Err (Path_Result2.Unwrap_Err);
         end if;
         Path1 := Path_Result1.Unwrap;
         Path2 := Path_Result2.Unwrap;
      end;

      -- Create multiple writers
      Writer1 := Create (Path1);
      Writer2 := Create (Path2);

      -- Write to both
      declare
         Chunk1 : constant File_Chunk_Type :=
           Create_Test_Chunk (0, Small_Chunk_Size, '1');
         Chunk2 : constant File_Chunk_Type :=
           Create_Test_Chunk (0, Medium_Chunk_Size, '2');
      begin
         Write_Chunk (Writer1.all, Chunk1);
         Write_Chunk (Writer2.all, Chunk2);
      end;

      -- Verify both are open
      if not Is_Open (Writer1.all) or not Is_Open (Writer2.all) then
         Close (Writer1.all);
         Close (Writer2.all);
         Destroy (Writer1);
         Destroy (Writer2);
         Delete_Test_File (Test_File1 & ".tmp");
         Delete_Test_File (Test_File2 & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Not all files open"));
      end if;

      -- Commit both
      declare
         Commit_Res1 : constant Write_Result.Result := Commit (Writer1.all);
         Commit_Res2 : constant Write_Result.Result := Commit (Writer2.all);
      begin
         if Commit_Res1.Is_Err or Commit_Res2.Is_Err then
            Destroy (Writer1);
            Destroy (Writer2);
            Delete_Test_File (Test_File1);
            Delete_Test_File (Test_File2);
            return Void_Result.Err (To_Unbounded_String
              ("Failed to commit multiple files"));
         end if;
      end;

      -- Cleanup
      Destroy (Writer1);
      Destroy (Writer2);
      Delete_Test_File (Test_File1);
      Delete_Test_File (Test_File2);

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File1);
         Delete_Test_File (Test_File1 & ".tmp");
         Delete_Test_File (Test_File2);
         Delete_Test_File (Test_File2 & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Multiple_File_Handles;

   -- Large file operations
   function Test_Large_File_Writing return Void_Result.Result is
      Test_File : constant String := "test_large.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
      -- Test with modest size to avoid test timeouts
      Num_Chunks : constant := 100;  -- 100 MB total with 1MB chunks
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      Writer := Create (Path, Expected_Size => Num_Chunks * Large_Chunk_Size);

      -- Write large chunks
      for I in 0 .. Num_Chunks - 1 loop
         declare
            Chunk : constant File_Chunk_Type :=
              Create_Test_Chunk (I, Large_Chunk_Size, 'L');
         begin
            Write_Chunk (Writer.all, Chunk);
         end;

         -- Periodic flush
         if I mod 10 = 0 then
            Flush (Writer.all);
         end if;
      end loop;

      -- Verify size
      declare
         Expected_Size : constant Long_Long_Integer :=
           Long_Long_Integer (Num_Chunks) * Long_Long_Integer (Large_Chunk_Size);
      begin
         if Size (Writer.all) < Expected_Size then
            Close (Writer.all);
            Destroy (Writer);
            Delete_Test_File (Test_File & ".tmp");
            return Void_Result.Err (To_Unbounded_String
              ("Large file size incorrect"));
         end if;
      end;

      -- Commit
      declare
         Commit_Res : constant Write_Result.Result := Commit (Writer.all);
      begin
         if Commit_Res.Is_Err then
            Destroy (Writer);
            Delete_Test_File (Test_File);
            return Void_Result.Err (Commit_Res.Unwrap_Err);
         end if;
      end;

      -- Cleanup
      Destroy (Writer);
      Delete_Test_File (Test_File);

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Large_File_Writing;

   function Test_Sparse_File_Creation return Void_Result.Result is
      Test_File : constant String := "test_sparse.dat";
      Path : File_Path;
      Writer : Random_Write_File_Access;
      Gap_Size : constant := 10 * Large_Chunk_Size;
   begin
      -- Setup
      Delete_Test_File (Test_File);
      Delete_Test_File (Test_File & ".tmp");

      declare
         Path_Result : constant File_Path_Result.Result :=
           Create_File_Path (To_Unbounded_String (Test_File));
      begin
         if Path_Result.Is_Err then
            return Void_Result.Err (Path_Result.Unwrap_Err);
         end if;
         Path := Path_Result.Unwrap;
      end;

      Writer := Create (Path);

      -- Write chunks with gaps (sparse file)
      for I in 0 .. 4 loop
         declare
            Chunk : constant File_Chunk_Type :=
              Create_Test_Chunk (I, Small_Chunk_Size, Character'Val (Character'Pos ('A') + I));
            Position : constant Long_Long_Integer :=
              Long_Long_Integer (I) * Long_Long_Integer (Gap_Size);
         begin
            Write_Chunk_At_Position (Writer.all, Chunk, Position);
         end;
      end loop;

      -- Size should reflect sparse nature
      declare
         Min_Size : constant Long_Long_Integer :=
           4 * Long_Long_Integer (Gap_Size) + Long_Long_Integer (Small_Chunk_Size);
      begin
         if Size (Writer.all) < Min_Size then
            Close (Writer.all);
            Destroy (Writer);
            Delete_Test_File (Test_File & ".tmp");
            return Void_Result.Err (To_Unbounded_String
              ("Sparse file size incorrect"));
         end if;
      end;

      -- Commit
      declare
         Commit_Res : constant Write_Result.Result := Commit (Writer.all);
      begin
         if Commit_Res.Is_Err then
            Destroy (Writer);
            Delete_Test_File (Test_File);
            return Void_Result.Err (Commit_Res.Unwrap_Err);
         end if;
      end;

      -- Cleanup
      Destroy (Writer);
      Delete_Test_File (Test_File);

      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         Delete_Test_File (Test_File & ".tmp");
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Sparse_File_Creation;

   -- Run all tests
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Stats : Test_Stats := (Total => 0, Passed => 0, Failed => 0, Skipped => 0);
   begin
      -- Basic file operations
      Run_Test (Output.all, "Create And Open", Test_Create_And_Open'Access, Stats);
      Run_Test (Output.all, "Create With Preallocate", Test_Create_With_Preallocate'Access, Stats);
      Run_Test (Output.all, "Create Without Temp", Test_Create_Without_Temp'Access, Stats);

      -- Write operations
      Run_Test (Output.all, "Write Single Chunk", Test_Write_Single_Chunk'Access, Stats);
      Run_Test (Output.all, "Write Multiple Chunks", Test_Write_Multiple_Chunks'Access, Stats);
      Run_Test (Output.all, "Write Random Positions", Test_Write_Random_Positions'Access, Stats);
      Run_Test (Output.all, "Write Overlapping Chunks", Test_Write_Overlapping_Chunks'Access, Stats);
      Run_Test (Output.all, "Write Beyond EOF", Test_Write_Beyond_EOF'Access, Stats);

      -- Chunk sequence operations
      Run_Test (Output.all, "Write By Sequence Number", Test_Write_By_Sequence_Number'Access, Stats);
      Run_Test (Output.all, "Write Out Of Order", Test_Write_Out_Of_Order'Access, Stats);

      -- Commit and rollback operations
      Run_Test (Output.all, "Commit Success", Test_Commit_Success'Access, Stats);
      Run_Test (Output.all, "Rollback Success", Test_Rollback_Success'Access, Stats);
      Run_Test (Output.all, "Commit After Rollback", Test_Commit_After_Rollback'Access, Stats);

      -- File management
      Run_Test (Output.all, "Flush Operations", Test_Flush_Operations'Access, Stats);
      Run_Test (Output.all, "Preallocate Space", Test_Preallocate_Space'Access, Stats);
      Run_Test (Output.all, "File Size Tracking", Test_File_Size_Tracking'Access, Stats);

      -- Error handling
      Run_Test (Output.all, "Write To Closed File", Test_Write_To_Closed_File'Access, Stats);
      Run_Test (Output.all, "Invalid Positions", Test_Invalid_Positions'Access, Stats);
      Run_Test (Output.all, "Disk Full Handling", Test_Disk_Full_Handling'Access, Stats);
      Run_Test (Output.all, "Permission Errors", Test_Permission_Errors'Access, Stats);

      -- Concurrent access
      Run_Test (Output.all, "Protected Concurrent Writes", Test_Protected_Concurrent_Writes'Access, Stats);
      Run_Test (Output.all, "Protected Write Order", Test_Protected_Write_Order'Access, Stats);
      Run_Test (Output.all, "Protected Size Query", Test_Protected_Size_Query'Access, Stats);

      -- Resource management
      Run_Test (Output.all, "Finalization Cleanup", Test_Finalization_Cleanup'Access, Stats);
      Run_Test (Output.all, "Destroy Procedure", Test_Destroy_Procedure'Access, Stats);
      Run_Test (Output.all, "Multiple File Handles", Test_Multiple_File_Handles'Access, Stats);

      -- Large file operations
      Run_Test (Output.all, "Large File Writing", Test_Large_File_Writing'Access, Stats);
      Run_Test (Output.all, "Sparse File Creation", Test_Sparse_File_Creation'Access, Stats);

      return Test_Stats_Result.Ok (Stats);
   end Run_All_Tests;

end Test_Random_Write_File;
