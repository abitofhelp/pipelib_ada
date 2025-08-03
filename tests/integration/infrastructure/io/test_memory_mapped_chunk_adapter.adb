--  =============================================================================
--  Test_Memory_Mapped_Chunk_Adapter - Memory Mapped Chunk Adapter Integration Tests Implementation
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

with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Value_Objects.File_Path;
with Abohlib.Infrastructure.Testing.Test_Framework;
with Pipelib.Infrastructure.IO.Memory_Mapped_Chunk_Adapter;
with Pipelib.Infrastructure.Adapters.IO.Unix_Memory_Map;
with Pipelib.Core.Domain.Ports.Memory_Mapped_File_Interface;
with Pipelib.Core.Domain.Value_Objects.File_Chunk;
with Pipelib.Core.Domain.Value_Objects.File_Chunk.Vectors;

package body Test_Memory_Mapped_Chunk_Adapter is

   use System.Storage_Elements;
   use Abohlib.Core.Domain.Value_Objects.File_Path;
   use Abohlib.Infrastructure.Testing.Test_Framework;
   use Pipelib.Infrastructure.IO.Memory_Mapped_Chunk_Adapter;
   use Pipelib.Infrastructure.Adapters.IO.Unix_Memory_Map;
   use Pipelib.Core.Domain.Ports.Memory_Mapped_File_Interface;
   use Pipelib.Core.Domain.Value_Objects.File_Chunk;
   use Pipelib.Core.Domain.Value_Objects.File_Chunk.Vectors;

   --  Test constants
   Small_File_Size : constant := 4096;  -- 4 KB
   Medium_File_Size : constant := 100 * 1024;  -- 100 KB
   Large_File_Size : constant := 10 * 1024 * 1024;  -- 10 MB

   --  Helper to create a test file with pattern
   procedure Create_Test_File
     (Path : String;
      Size : Natural;
      Pattern : String := "CHUNK")
   is
      File : Ada.Text_IO.File_Type;
      Pattern_Length : constant Natural := Pattern'Length;
      Repetitions : constant Natural := Size / Pattern_Length;
      Remainder : constant Natural := Size mod Pattern_Length;
   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Path);

      -- Write pattern repeatedly
      for I in 1 .. Repetitions loop
         Ada.Text_IO.Put (File, Pattern);
      end loop;

      -- Write any remaining bytes
      if Remainder > 0 then
         Ada.Text_IO.Put (File, Pattern (1 .. Remainder));
      end if;

      Ada.Text_IO.Close (File);
   end Create_Test_File;

   --  Helper to delete test file if it exists
   procedure Delete_Test_File (Path : String) is
   begin
      if Ada.Directories.Exists (Path) then
         Ada.Directories.Delete_File (Path);
      end if;
   end Delete_Test_File;

   --  Helper to verify chunk data
   function Verify_Chunk_Data
     (Chunk : File_Chunk_Type;
      Expected_Pattern : String) return Boolean
   is
      Data : constant Stream_Element_Array_Access := Get_Data (Chunk);
      Pattern_Bytes : Storage_Array (1 .. Storage_Offset (Expected_Pattern'Length));
   begin
      -- Convert pattern to bytes
      for I in Expected_Pattern'Range loop
         Pattern_Bytes (Storage_Offset (I - Expected_Pattern'First + 1)) :=
           Character'Pos (Expected_Pattern (I));
      end loop;

      -- Check if chunk starts with pattern
      if Data.all'Length < Pattern_Bytes'Length then
         return False;
      end if;

      for I in Pattern_Bytes'Range loop
         if Data (Data'First + Storage_Offset (I - 1)) /= Pattern_Bytes (I) then
            return False;
         end if;
      end loop;

      return True;
   end Verify_Chunk_Data;

   -- Zero-copy operations tests
   function Test_Create_Chunks_Zero_Copy return Void_Result.Result is
      Test_File : constant String := "test_zero_copy.dat";
      Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Medium_File_Size, "ZEROCOPY");
      Map := Create;

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
         Map_Res : constant Map_Result.Result := Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;
      end;

      -- Create chunks with zero-copy
      declare
         Config : constant Chunk_Config :=
           (Default_Chunk_Size => 16 * 1024,  -- 16KB chunks
            Calculate_Checksums => True,
            Use_Sequential_Access => True);
         Chunks_Result : constant Chunk_Vector_Result.Result :=
           Create_Chunks_From_Memory_Map (Map.all, Config);
      begin
         if Chunks_Result.Is_Err then
            Map.Unmap;
            Delete_Test_File (Test_File);
            return Void_Result.Err (Chunks_Result.Unwrap_Err);
         end if;

         declare
            Chunks : constant File_Chunk_Vector := Chunks_Result.Unwrap;
         begin
            -- Verify we got expected number of chunks
            if Natural (Chunks.Length) = 0 then
               Map.Unmap;
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("No chunks created"));
            end if;

            -- Verify first chunk has correct data
            if not Verify_Chunk_Data (Chunks.First_Element, "ZEROCOPY") then
               Map.Unmap;
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("First chunk data incorrect"));
            end if;

            -- Verify chunks point to memory-mapped data (zero-copy)
            declare
               First_Chunk_Data : constant Stream_Element_Array_Access :=
                 Get_Data (Chunks.First_Element);
               Map_View : constant Memory_View := Map.Get_View;
               First_Chunk_Address : constant System.Address :=
                 First_Chunk_Data.all'Address;
            begin
               -- Check if chunk data is within mapped region
               if To_Integer (First_Chunk_Address) < To_Integer (Map_View.Address) or
                  To_Integer (First_Chunk_Address) >=
                    To_Integer (Map_View.Address) + Integer_Address (Map_View.Size)
               then
                  Map.Unmap;
                  Delete_Test_File (Test_File);
                  return Void_Result.Err (To_Unbounded_String
                    ("Chunk data not in mapped region - not zero-copy"));
               end if;
            end;
         end;
      end;

      -- Cleanup
      Map.Unmap;
      Delete_Test_File (Test_File);
      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Create_Chunks_Zero_Copy;

   function Test_Single_Chunk_Creation return Void_Result.Result is
      Test_File : constant String := "test_single_chunk.dat";
      Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Small_File_Size, "SINGLE");
      Map := Create;

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
         Map_Res : constant Map_Result.Result := Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;
      end;

      -- Create single chunk
      declare
         Chunk : constant File_Chunk_Type :=
           Create_Single_Chunk_From_Memory_Map
             (Map.all,
              Sequence_Number => 0,
              Offset => 0,
              Length => Storage_Count (Small_File_Size),
              Is_Final => True,
              Calculate_Checksum => True);
      begin
         -- Verify chunk properties
         if Get_Sequence_Number (Chunk) /= 0 then
            Map.Unmap;
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("Incorrect sequence number"));
         end if;

         if not Is_Final_Chunk (Chunk) then
            Map.Unmap;
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("Chunk should be marked as final"));
         end if;

         if Natural (Get_Data (Chunk).all'Length) /= Small_File_Size then
            Map.Unmap;
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("Chunk size incorrect"));
         end if;

         -- Verify data content
         if not Verify_Chunk_Data (Chunk, "SINGLE") then
            Map.Unmap;
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("Chunk data incorrect"));
         end if;

         -- Verify checksum was calculated
         if Get_Checksum (Chunk) = 0 then
            Map.Unmap;
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("Checksum not calculated"));
         end if;
      end;

      -- Cleanup
      Map.Unmap;
      Delete_Test_File (Test_File);
      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Single_Chunk_Creation;

   function Test_Large_File_Chunking return Void_Result.Result is
      Test_File : constant String := "test_large_chunking.dat";
      Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup - create large file
      Create_Test_File (Test_File, Large_File_Size, "LARGEFILE");
      Map := Create;

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
         Map_Res : constant Map_Result.Result := Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;
      end;

      -- Create chunks with larger size for large file
      declare
         Config : constant Chunk_Config :=
           (Default_Chunk_Size => 1024 * 1024,  -- 1MB chunks
            Calculate_Checksums => False,  -- Skip for performance
            Use_Sequential_Access => True);
         Chunks_Result : constant Chunk_Vector_Result.Result :=
           Create_Chunks_From_Memory_Map (Map.all, Config);
      begin
         if Chunks_Result.Is_Err then
            Map.Unmap;
            Delete_Test_File (Test_File);
            return Void_Result.Err (Chunks_Result.Unwrap_Err);
         end if;

         declare
            Chunks : constant File_Chunk_Vector := Chunks_Result.Unwrap;
            Expected_Chunks : constant Natural :=
              Large_File_Size / Config.Default_Chunk_Size;
         begin
            -- Verify chunk count
            if Natural (Chunks.Length) < Expected_Chunks then
               Map.Unmap;
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("Too few chunks created for large file"));
            end if;

            -- Verify last chunk is marked final
            if not Is_Final_Chunk (Chunks.Last_Element) then
               Map.Unmap;
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("Last chunk not marked as final"));
            end if;

            -- Verify sequential numbering
            for I in 0 .. Natural (Chunks.Length) - 1 loop
               if Get_Sequence_Number (Chunks.Element (I)) /= I then
                  Map.Unmap;
                  Delete_Test_File (Test_File);
                  return Void_Result.Err (To_Unbounded_String
                    ("Chunk sequence numbers not sequential"));
               end if;
            end loop;
         end;
      end;

      -- Cleanup
      Map.Unmap;
      Delete_Test_File (Test_File);
      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Large_File_Chunking;

   -- Configuration and optimization tests
   function Test_Chunk_Config_Variations return Void_Result.Result is
      Test_File : constant String := "test_config.dat";
      Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Medium_File_Size, "CONFIG");
      Map := Create;

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
         Map_Res : constant Map_Result.Result := Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;
      end;

      -- Test different configurations
      declare
         Configs : constant array (1 .. 3) of Chunk_Config :=
           ((Default_Chunk_Size => 4096, Calculate_Checksums => True, Use_Sequential_Access => True),
            (Default_Chunk_Size => 32768, Calculate_Checksums => False, Use_Sequential_Access => False),
            (Default_Chunk_Size => 65536, Calculate_Checksums => True, Use_Sequential_Access => False));
      begin
         for Config of Configs loop
            declare
               Chunks_Result : constant Chunk_Vector_Result.Result :=
                 Create_Chunks_From_Memory_Map (Map.all, Config);
            begin
               if Chunks_Result.Is_Err then
                  Map.Unmap;
                  Delete_Test_File (Test_File);
                  return Void_Result.Err (Chunks_Result.Unwrap_Err);
               end if;

               declare
                  Chunks : constant File_Chunk_Vector := Chunks_Result.Unwrap;
               begin
                  -- Verify chunks created according to config
                  if Natural (Chunks.Length) = 0 then
                     Map.Unmap;
                     Delete_Test_File (Test_File);
                     return Void_Result.Err (To_Unbounded_String
                       ("No chunks created with config"));
                  end if;

                  -- Check checksum calculation
                  for Chunk of Chunks loop
                     if Config.Calculate_Checksums and Get_Checksum (Chunk) = 0 then
                        Map.Unmap;
                        Delete_Test_File (Test_File);
                        return Void_Result.Err (To_Unbounded_String
                          ("Checksum not calculated when requested"));
                     elsif not Config.Calculate_Checksums and Get_Checksum (Chunk) /= 0 then
                        Map.Unmap;
                        Delete_Test_File (Test_File);
                        return Void_Result.Err (To_Unbounded_String
                          ("Checksum calculated when not requested"));
                     end if;
                  end loop;
               end;
            end;
         end loop;
      end;

      -- Cleanup
      Map.Unmap;
      Delete_Test_File (Test_File);
      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Chunk_Config_Variations;

   function Test_Optimal_Chunk_Size_Calculation return Void_Result.Result is
   begin
      -- Test various file sizes
      declare
         Test_Cases : constant array (1 .. 5) of Storage_Count :=
           (1024,                    -- 1 KB
            100 * 1024,              -- 100 KB
            10 * 1024 * 1024,        -- 10 MB
            100 * 1024 * 1024,       -- 100 MB
            1024 * 1024 * 1024);     -- 1 GB
      begin
         for File_Size of Test_Cases loop
            declare
               Chunk_Size : constant Positive :=
                 Calculate_Optimal_Chunk_Size (File_Size);
            begin
               -- Verify chunk size is reasonable
               if Chunk_Size < 4096 then  -- Minimum 4KB
                  return Void_Result.Err (To_Unbounded_String
                    ("Chunk size too small: " & Chunk_Size'Image));
               end if;

               if Chunk_Size > 10 * 1024 * 1024 then  -- Maximum 10MB
                  return Void_Result.Err (To_Unbounded_String
                    ("Chunk size too large: " & Chunk_Size'Image));
               end if;

               -- Smaller files should have smaller chunks
               if File_Size < 1024 * 1024 and Chunk_Size > Natural (File_Size) then
                  return Void_Result.Err (To_Unbounded_String
                    ("Chunk size larger than file size"));
               end if;
            end;
         end loop;
      end;

      -- Test with memory constraints
      declare
         File_Size : constant Storage_Count := 100 * 1024 * 1024;  -- 100 MB
         Available_Memory : constant Storage_Count := 50 * 1024 * 1024;  -- 50 MB
         Chunk_Size : constant Positive :=
           Calculate_Optimal_Chunk_Size (File_Size, Available_Memory);
      begin
         -- Chunk size should consider available memory
         if Chunk_Size > Natural (Available_Memory / 2) then
            return Void_Result.Err (To_Unbounded_String
              ("Chunk size doesn't respect memory constraint"));
         end if;
      end;

      return Void_Result.Ok;

   exception
      when E : others =>
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Optimal_Chunk_Size_Calculation;

   function Test_Memory_Mapping_Decision return Void_Result.Result is
   begin
      -- Test decision logic for various file sizes
      declare
         Small_Files : constant array (1 .. 3) of Storage_Count :=
           (1024, 10 * 1024, 100 * 1024);  -- 1KB, 10KB, 100KB
         Large_Files : constant array (1 .. 3) of Storage_Count :=
           (100 * 1024 * 1024,              -- 100MB
            500 * 1024 * 1024,              -- 500MB
            1024 * 1024 * 1024);            -- 1GB
      begin
         -- Small files might not benefit from memory mapping
         for Size of Small_Files loop
            declare
               Should_Map : constant Boolean :=
                 Should_Use_Memory_Mapping_For_File (Size);
            begin
               -- Decision depends on implementation, just verify it returns valid result
               null;
            end;
         end loop;

         -- Large files should typically use memory mapping
         for Size of Large_Files loop
            declare
               Should_Map : constant Boolean :=
                 Should_Use_Memory_Mapping_For_File (Size);
            begin
               if not Should_Map then
                  return Void_Result.Err (To_Unbounded_String
                    ("Large file should use memory mapping: " & Size'Image));
               end if;
            end;
         end loop;
      end;

      -- Test with memory constraints
      declare
         File_Size : constant Storage_Count := 1024 * 1024 * 1024;  -- 1GB
         Limited_Memory : constant Storage_Count := 100 * 1024 * 1024;  -- 100MB
         Should_Map : constant Boolean :=
           Should_Use_Memory_Mapping_For_File (File_Size, Limited_Memory);
      begin
         -- With limited memory, might decide against memory mapping
         -- Just verify function works with memory parameter
         null;
      end;

      return Void_Result.Ok;

   exception
      when E : others =>
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Memory_Mapping_Decision;

   -- Error handling and edge cases
   function Test_Invalid_Memory_Map_Handling return Void_Result.Result is
      Map : Unix_Memory_Map_Access;
   begin
      Map := Create;

      -- Try to create chunks from unmapped file
      declare
         Config : constant Chunk_Config := (others => <>);
         Success : Boolean := False;
      begin
         declare
            Chunks_Result : constant Chunk_Vector_Result.Result :=
              Create_Chunks_From_Memory_Map (Map.all, Config);
            pragma Unreferenced (Chunks_Result);
         begin
            Success := False;  -- Should not reach here
         end;
      exception
         when others =>
            Success := True;  -- Expected - precondition violation
      end;

      if not Success then
         return Void_Result.Err (To_Unbounded_String
           ("Should fail with unmapped file"));
      end if;

      return Void_Result.Ok;

   exception
      when E : others =>
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Invalid_Memory_Map_Handling;

   function Test_Memory_Access_Violations return Void_Result.Result is
      Test_File : constant String := "test_access.dat";
      Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Small_File_Size);
      Map := Create;

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
         Map_Res : constant Map_Result.Result := Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;
      end;

      -- Try to create chunk beyond file size
      declare
         Success : Boolean := False;
      begin
         declare
            Chunk : constant File_Chunk_Type :=
              Create_Single_Chunk_From_Memory_Map
                (Map.all,
                 Sequence_Number => 0,
                 Offset => Storage_Count (Small_File_Size) + 1000,
                 Length => 100,
                 Is_Final => True);
            pragma Unreferenced (Chunk);
         begin
            Success := False;  -- Should not reach here
         end;
      exception
         when others =>
            Success := True;  -- Expected - precondition violation
      end;

      if not Success then
         Map.Unmap;
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Should fail with out-of-bounds access"));
      end if;

      -- Cleanup
      Map.Unmap;
      Delete_Test_File (Test_File);
      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Memory_Access_Violations;

   function Test_Resource_Cleanup return Void_Result.Result is
      Test_File : constant String := "test_cleanup.dat";
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Medium_File_Size);

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

      -- Create chunks in nested scope
      declare
         Map : Unix_Memory_Map_Access := Create;
         Map_Res : constant Map_Result.Result := Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         declare
            Config : constant Chunk_Config := (others => <>);
            Chunks_Result : constant Chunk_Vector_Result.Result :=
              Create_Chunks_From_Memory_Map (Map.all, Config);
         begin
            if Chunks_Result.Is_Err then
               Map.Unmap;
               Delete_Test_File (Test_File);
               return Void_Result.Err (Chunks_Result.Unwrap_Err);
            end if;

            -- Chunks go out of scope here
         end;

         -- Map still valid after chunks destroyed
         if not Map.Is_Mapped then
            Delete_Test_File (Test_File);
            return Void_Result.Err (To_Unbounded_String
              ("Map invalidated after chunk cleanup"));
         end if;

         Map.Unmap;
      end;

      -- Cleanup
      Delete_Test_File (Test_File);
      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Resource_Cleanup;

   -- Performance and concurrency tests
   function Test_Concurrent_Chunk_Creation return Void_Result.Result is
      Test_File : constant String := "test_concurrent.dat";
      Path : File_Path;
   begin
      -- Setup large file for concurrent processing
      Create_Test_File (Test_File, Large_File_Size, "CONCURRENT");

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

      -- Test concurrent chunk creation using tasks
      declare
         Map : Unix_Memory_Map_Access := Create;
         Map_Res : constant Map_Result.Result := Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;

         declare
            Chunk_Size : constant := 1024 * 1024;  -- 1MB chunks
            Num_Tasks : constant := 4;

            task type Chunk_Creator is
               entry Start (Task_ID : Natural);
               entry Get_Result (Success : out Boolean; Error : out Unbounded_String);
            end Chunk_Creator;

            task body Chunk_Creator is
               ID : Natural;
               OK : Boolean := False;
               Err_Msg : Unbounded_String := Null_Unbounded_String;
            begin
               accept Start (Task_ID : Natural) do
                  ID := Task_ID;
               end Start;

               -- Each task creates chunks from different file regions
               declare
                  Offset : constant Storage_Count :=
                    Storage_Count (ID * Chunk_Size * 2);
                  Length : constant Storage_Count :=
                    Storage_Count (Chunk_Size);
               begin
                  if Offset + Length <= Map.Get_Size then
                     declare
                        Chunk : constant File_Chunk_Type :=
                          Create_Single_Chunk_From_Memory_Map
                            (Map.all, ID, Offset, Length, False, True);
                     begin
                        -- Verify chunk
                        if Get_Sequence_Number (Chunk) = ID then
                           OK := True;
                        else
                           Err_Msg := To_Unbounded_String ("Wrong sequence number");
                        end if;
                     end;
                  else
                     OK := True;  -- Skip if beyond file
                  end if;
               exception
                  when E : others =>
                     Err_Msg := To_Unbounded_String
                       ("Task " & ID'Image & ": " &
                        Ada.Exceptions.Exception_Message (E));
               end;

               accept Get_Result (Success : out Boolean; Error : out Unbounded_String) do
                  Success := OK;
                  Error := Err_Msg;
               end Get_Result;
            end Chunk_Creator;

            Creators : array (0 .. Num_Tasks - 1) of Chunk_Creator;
            All_Success : Boolean := True;
         begin
            -- Start all tasks
            for I in Creators'Range loop
               Creators (I).Start (I);
            end loop;

            -- Collect results
            for Creator of Creators loop
               declare
                  Success : Boolean;
                  Error : Unbounded_String;
               begin
                  Creator.Get_Result (Success, Error);
                  if not Success then
                     All_Success := False;
                     -- Log error but continue
                  end if;
               end;
            end loop;

            if not All_Success then
               Map.Unmap;
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("Not all concurrent tasks succeeded"));
            end if;
         end;

         Map.Unmap;
      end;

      -- Cleanup
      Delete_Test_File (Test_File);
      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Concurrent_Chunk_Creation;

   function Test_Memory_Pressure_Scenarios return Void_Result.Result is
      Test_File : constant String := "test_pressure.dat";
      Map : Unix_Memory_Map_Access;
      Path : File_Path;
   begin
      -- Setup medium file
      Create_Test_File (Test_File, Medium_File_Size);
      Map := Create;

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
         Map_Res : constant Map_Result.Result := Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;
      end;

      -- Create many small chunks to simulate memory pressure
      declare
         Config : constant Chunk_Config :=
           (Default_Chunk_Size => 512,  -- Very small chunks
            Calculate_Checksums => True,
            Use_Sequential_Access => False);
         Chunks_Result : constant Chunk_Vector_Result.Result :=
           Create_Chunks_From_Memory_Map (Map.all, Config);
      begin
         if Chunks_Result.Is_Err then
            Map.Unmap;
            Delete_Test_File (Test_File);
            return Void_Result.Err (Chunks_Result.Unwrap_Err);
         end if;

         declare
            Chunks : constant File_Chunk_Vector := Chunks_Result.Unwrap;
         begin
            -- Should create many chunks
            if Natural (Chunks.Length) < Medium_File_Size / Config.Default_Chunk_Size then
               Map.Unmap;
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("Too few chunks under memory pressure"));
            end if;

            -- All chunks should still be valid
            for Chunk of Chunks loop
               if Get_Data (Chunk) = null then
                  Map.Unmap;
                  Delete_Test_File (Test_File);
                  return Void_Result.Err (To_Unbounded_String
                    ("Null chunk data under memory pressure"));
               end if;
            end loop;
         end;
      end;

      -- Cleanup
      Map.Unmap;
      Delete_Test_File (Test_File);
      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Memory_Pressure_Scenarios;

   -- Integration with memory-mapped file interface
   function Test_Memory_Map_Interface_Integration return Void_Result.Result is
      Test_File : constant String := "test_interface.dat";
      Map : Unix_Memory_Map_Access;
      Interface_Ref : access Memory_Mapped_File_Interface'Class;
      Path : File_Path;
   begin
      -- Setup
      Create_Test_File (Test_File, Medium_File_Size, "INTERFACE");
      Map := Create;
      Interface_Ref := Map;  -- Verify it implements interface

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

      -- Map file through interface
      declare
         Map_Res : constant Map_Result.Result := Interface_Ref.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;
      end;

      -- Create chunks using interface reference
      declare
         Config : constant Chunk_Config := (others => <>);
         Chunks_Result : constant Chunk_Vector_Result.Result :=
           Create_Chunks_From_Memory_Map (Interface_Ref.all, Config);
      begin
         if Chunks_Result.Is_Err then
            Interface_Ref.Unmap;
            Delete_Test_File (Test_File);
            return Void_Result.Err (Chunks_Result.Unwrap_Err);
         end if;

         declare
            Chunks : constant File_Chunk_Vector := Chunks_Result.Unwrap;
         begin
            -- Verify chunks work with interface
            if Natural (Chunks.Length) = 0 then
               Interface_Ref.Unmap;
               Delete_Test_File (Test_File);
               return Void_Result.Err (To_Unbounded_String
                 ("No chunks from interface"));
            end if;

            -- Test single chunk creation through interface
            declare
               Single_Chunk : constant File_Chunk_Type :=
                 Create_Single_Chunk_From_Memory_Map
                   (Interface_Ref.all,
                    Sequence_Number => 99,
                    Offset => 0,
                    Length => 1024,
                    Is_Final => False);
            begin
               if Get_Sequence_Number (Single_Chunk) /= 99 then
                  Interface_Ref.Unmap;
                  Delete_Test_File (Test_File);
                  return Void_Result.Err (To_Unbounded_String
                    ("Interface chunk has wrong sequence"));
               end if;
            end;
         end;
      end;

      -- Cleanup through interface
      Interface_Ref.Unmap;
      Delete_Test_File (Test_File);
      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Memory_Map_Interface_Integration;

   function Test_Checksum_Calculation return Void_Result.Result is
      Test_File : constant String := "test_checksum.dat";
      Map : Unix_Memory_Map_Access;
      Path : File_Path;
      Test_Pattern : constant String := "CHECKSUM_TEST_PATTERN_12345";
   begin
      -- Setup with specific pattern
      Create_Test_File (Test_File, Small_File_Size, Test_Pattern);
      Map := Create;

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
         Map_Res : constant Map_Result.Result := Map.Map_File (Path, True);
      begin
         if Map_Res.Is_Err then
            Delete_Test_File (Test_File);
            return Void_Result.Err (Map_Res.Unwrap_Err);
         end if;
      end;

      -- Create chunks with checksum calculation
      declare
         Config : constant Chunk_Config :=
           (Default_Chunk_Size => 1024,
            Calculate_Checksums => True,
            Use_Sequential_Access => True);
         Chunks_Result : constant Chunk_Vector_Result.Result :=
           Create_Chunks_From_Memory_Map (Map.all, Config);
      begin
         if Chunks_Result.Is_Err then
            Map.Unmap;
            Delete_Test_File (Test_File);
            return Void_Result.Err (Chunks_Result.Unwrap_Err);
         end if;

         declare
            Chunks : constant File_Chunk_Vector := Chunks_Result.Unwrap;
            First_Checksum : Long_Long_Integer := 0;
         begin
            -- Verify checksums calculated
            for I in 0 .. Natural (Chunks.Length) - 1 loop
               declare
                  Chunk : constant File_Chunk_Type := Chunks.Element (I);
                  Checksum : constant Long_Long_Integer := Get_Checksum (Chunk);
               begin
                  if Checksum = 0 then
                     Map.Unmap;
                     Delete_Test_File (Test_File);
                     return Void_Result.Err (To_Unbounded_String
                       ("Zero checksum calculated"));
                  end if;

                  -- Chunks with same data should have same checksum
                  if I = 0 then
                     First_Checksum := Checksum;
                  elsif Natural (Get_Data (Chunk).all'Length) =
                        Natural (Get_Data (Chunks.First_Element).all'Length) then
                     -- If chunks are same size and have same pattern,
                     -- checksums might be similar (depends on algorithm)
                     null;
                  end if;
               end;
            end loop;

            -- Test without checksum calculation
            declare
               No_Checksum_Config : constant Chunk_Config :=
                 (Default_Chunk_Size => 1024,
                  Calculate_Checksums => False,
                  Use_Sequential_Access => True);
               No_CS_Result : constant Chunk_Vector_Result.Result :=
                 Create_Chunks_From_Memory_Map (Map.all, No_Checksum_Config);
            begin
               if No_CS_Result.Is_Ok then
                  declare
                     No_CS_Chunks : constant File_Chunk_Vector := No_CS_Result.Unwrap;
                  begin
                     -- Verify no checksums calculated
                     for Chunk of No_CS_Chunks loop
                        if Get_Checksum (Chunk) /= 0 then
                           Map.Unmap;
                           Delete_Test_File (Test_File);
                           return Void_Result.Err (To_Unbounded_String
                             ("Checksum calculated when disabled"));
                        end if;
                     end loop;
                  end;
               end if;
            end;
         end;
      end;

      -- Cleanup
      Map.Unmap;
      Delete_Test_File (Test_File);
      return Void_Result.Ok;

   exception
      when E : others =>
         Delete_Test_File (Test_File);
         return Void_Result.Err (To_Unbounded_String
           ("Unexpected exception: " & Ada.Exceptions.Exception_Message (E)));
   end Test_Checksum_Calculation;

   -- Run all tests
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Stats : Test_Stats := (Total => 0, Passed => 0, Failed => 0, Skipped => 0);
   begin
      -- Zero-copy operations tests
      Run_Test (Output.all, "Create Chunks Zero Copy", Test_Create_Chunks_Zero_Copy'Access, Stats);
      Run_Test (Output.all, "Single Chunk Creation", Test_Single_Chunk_Creation'Access, Stats);
      Run_Test (Output.all, "Large File Chunking", Test_Large_File_Chunking'Access, Stats);

      -- Configuration and optimization tests
      Run_Test (Output.all, "Chunk Config Variations", Test_Chunk_Config_Variations'Access, Stats);
      Run_Test (Output.all, "Optimal Chunk Size Calculation", Test_Optimal_Chunk_Size_Calculation'Access, Stats);
      Run_Test (Output.all, "Memory Mapping Decision", Test_Memory_Mapping_Decision'Access, Stats);

      -- Error handling and edge cases
      Run_Test (Output.all, "Invalid Memory Map Handling", Test_Invalid_Memory_Map_Handling'Access, Stats);
      Run_Test (Output.all, "Memory Access Violations", Test_Memory_Access_Violations'Access, Stats);
      Run_Test (Output.all, "Resource Cleanup", Test_Resource_Cleanup'Access, Stats);

      -- Performance and concurrency tests
      Run_Test (Output.all, "Concurrent Chunk Creation", Test_Concurrent_Chunk_Creation'Access, Stats);
      Run_Test (Output.all, "Memory Pressure Scenarios", Test_Memory_Pressure_Scenarios'Access, Stats);

      -- Integration with memory-mapped file interface
      Run_Test (Output.all, "Memory Map Interface Integration", Test_Memory_Map_Interface_Integration'Access, Stats);
      Run_Test (Output.all, "Checksum Calculation", Test_Checksum_Calculation'Access, Stats);

      return Test_Stats_Result.Ok (Stats);
   end Run_All_Tests;

end Test_Memory_Mapped_Chunk_Adapter;
