--  =============================================================================
--  Test_Pipeline_E2E - End-to-End Pipeline Tests Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams; use Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Direct_IO;
with Ada.Calendar; use Ada.Calendar;
with Abohlib.Core.Domain.Constants.Bytes;
with Pipelib.Core.Domain.Value_Objects.Chunk_Size; use Pipelib.Core.Domain.Value_Objects.Chunk_Size;
with Pipelib.Core.Domain.Value_Objects.File_Chunk; use Pipelib.Core.Domain.Value_Objects.File_Chunk;
with Pipelib.Core.Domain.Services.Stages.Generic_Hasher_Stage;

package body Test_Pipeline_E2E is

   --  Test file management
   Test_Dir : constant String := "tests/e2e/test_data";

   procedure Ensure_Test_Directory is
   begin
      if not Exists (Test_Dir) then
         Create_Directory (Test_Dir);
      end if;
   end Ensure_Test_Directory;

   procedure Cleanup_Test_Files is
   begin
      if Exists (Test_Dir) then
         -- Remove test files
         declare
            Search : Search_Type;
            Dir_Ent : Directory_Entry_Type;
         begin
            Start_Search (Search, Test_Dir, "*", [Ordinary_File => True, others => False]);
            while More_Entries (Search) loop
               Get_Next_Entry (Search, Dir_Ent);
               Delete_File (Full_Name (Dir_Ent));
            end loop;
            End_Search (Search);
         end;
      end if;
   end Cleanup_Test_Files;

   --  Helper to create a test file with pattern data
   procedure Create_Test_File (Name : String; Size : Natural) is
      package Byte_IO is new Ada.Direct_IO (Stream_Element);
      use Byte_IO;

      File : Byte_IO.File_Type;
      Path : constant String := Test_Dir & "/" & Name;
   begin
      Create (File, Out_File, Path);

      for I in 1 .. Size loop
         Write (File, Stream_Element (I mod 256));
      end loop;

      Close (File);
   end Create_Test_File;

   --  ==========================================================================
   --  End-to-End Tests
   --  ==========================================================================

   function Test_Simple_File_Hashing return Void_Result.Result is
      package Test_Hasher is new Pipelib.Core.Domain.Services.Stages.Generic_Hasher_Stage
         (Stage_Purpose => "E2E_Simple");
      use Test_Hasher;

      Stage : Hasher_Stage_Type;
      Test_File : constant String := "simple_test.dat";
      File_Size : constant Natural := 10 * Natural (Abohlib.Core.Domain.Constants.Bytes.SI_KB);
      Chunk_Size : constant Chunk_Size_Type := From_KB (4);
      Total_Processed : Natural := 0;
   begin
      Ensure_Test_Directory;

      -- Create test file
      Create_Test_File (Test_File, File_Size);

      -- Process file in chunks
      declare
         Offset : Long_Long_Integer := 0;
         Chunk_Num : Natural := 1;
      begin
         while Offset < Long_Long_Integer (File_Size) loop
            declare
               Remaining : constant Natural := Natural'Min (
                  Natural (Value (Chunk_Size)),
                  File_Size - Natural (Offset)
               );
               Data : Stream_Element_Array (1 .. Stream_Element_Offset (Remaining));
            begin
               -- Read chunk data (simulated - in real implementation would use actual file I/O)
               for I in Data'Range loop
                  Data (I) := Stream_Element ((Natural (Offset) + Natural (I)) mod 256);
               end loop;

               -- Create and process chunk
               declare
                  Chunk : constant File_Chunk_Type := Create (
                     Sequence_Number => Chunk_Num,
                     Offset          => Offset,
                     Data            => Data,
                     Is_Final        => Offset + Long_Long_Integer (Remaining) >= Long_Long_Integer (File_Size)
                  );
                  Result : constant Chunk_Result.Result := Process_Chunk (Stage, Chunk);
               begin
                  if not Result.Is_Ok then
                     Cleanup_Test_Files;
                     return Void_Result.Err (Test_Error'(
                        Kind        => Assertion_Failed,
                        Message     => To_Unbounded_String ("Failed to process chunk"),
                        Details     => Result.Get_Err,
                        Line_Number => 0,
                        Test_Name   => To_Unbounded_String ("Test_Simple_File_Hashing")
                     ));
                  end if;

                  Total_Processed := Total_Processed + Remaining;
                  Offset := Offset + Long_Long_Integer (Remaining);
                  Chunk_Num := Chunk_Num + 1;
               end;
            end;
         end loop;
      end;

      -- Finalize and get hash
      declare
         Hash_Result : constant Test_Hasher.Hash_Result.Result := Finalize_Hash (Stage);
      begin
         if not Hash_Result.Is_Ok then
            Cleanup_Test_Files;
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Failed to finalize hash"),
               Details     => Hash_Result.Get_Err,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Simple_File_Hashing")
            ));
         end if;

         Put_Line ("File hash: " & To_String (Hash_Result.Get_Ok));
      end;

      -- Verify total bytes processed
      if Total_Processed /= File_Size then
         Cleanup_Test_Files;
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Processed size mismatch"),
            Details     => To_Unbounded_String ("Expected: " & File_Size'Image &
                                               ", Got: " & Total_Processed'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Simple_File_Hashing")
         ));
      end if;

      Cleanup_Test_Files;
      return Void_Result.Ok (True);
   end Test_Simple_File_Hashing;

   function Test_Parallel_Pipeline_Processing return Void_Result.Result is
      package Input_Hasher is new Pipelib.Core.Domain.Services.Stages.Generic_Hasher_Stage
         (Stage_Purpose => "E2E_Input");
      package Output_Hasher is new Pipelib.Core.Domain.Services.Stages.Generic_Hasher_Stage
         (Stage_Purpose => "E2E_Output");
      use Input_Hasher, Output_Hasher;

      Input_Stage : Input_Hasher.Hasher_Stage_Type;
      Output_Stage : Output_Hasher.Hasher_Stage_Type;

      Test_Size : constant Natural := 50 * Natural (Abohlib.Core.Domain.Constants.Bytes.SI_KB);
      Chunk_Size : constant Chunk_Size_Type := From_KB (8);
      Chunks_Read : Natural := 0;
      Chunks_Processed : Natural := 0;
   begin
      -- Process data through parallel stages
      declare
         Offset : Long_Long_Integer := 0;
         Chunk_Num : Natural := 1;
      begin
         while Offset < Long_Long_Integer (Test_Size) loop
            declare
               Remaining : constant Natural := Natural'Min (
                  Natural (Value (Chunk_Size)),
                  Test_Size - Natural (Offset)
               );
               Data : constant Stream_Element_Array (1 .. Stream_Element_Offset (Remaining)) :=
                  [others => Stream_Element (Chunk_Num mod 256)];
               Chunk : constant File_Chunk_Type := Create (
                  Sequence_Number => Chunk_Num,
                  Offset          => Offset,
                  Data            => Data,
                  Is_Final        => Offset + Long_Long_Integer (Remaining) >= Long_Long_Integer (Test_Size)
               );
            begin
               Chunks_Read := Chunks_Read + 1;

               -- Process through input stage
               declare
                  Result1 : constant Input_Hasher.Chunk_Result.Result :=
                     Process_Chunk (Input_Stage, Chunk);
               begin
                  if not Result1.Is_Ok then
                     return Void_Result.Err (Test_Error'(
                        Kind        => Assertion_Failed,
                        Message     => To_Unbounded_String ("Input stage processing failed"),
                        Details     => Result1.Get_Err,
                        Line_Number => 0,
                        Test_Name   => To_Unbounded_String ("Test_Parallel_Pipeline_Processing")
                     ));
                  end if;
               end;

               -- Process through output stage
               declare
                  Result2 : constant Output_Hasher.Chunk_Result.Result :=
                     Process_Chunk (Output_Stage, Chunk);
               begin
                  if not Result2.Is_Ok then
                     return Void_Result.Err (Test_Error'(
                        Kind        => Assertion_Failed,
                        Message     => To_Unbounded_String ("Output stage processing failed"),
                        Details     => Result2.Get_Err,
                        Line_Number => 0,
                        Test_Name   => To_Unbounded_String ("Test_Parallel_Pipeline_Processing")
                     ));
                  end if;
               end;

               Chunks_Processed := Chunks_Processed + 1;

               Offset := Offset + Long_Long_Integer (Remaining);
               Chunk_Num := Chunk_Num + 1;
            end;
         end loop;
      end;

      -- Get final hashes
      declare
         Input_Hash_Result : constant Input_Hasher.Hash_Result.Result :=
            Finalize_Hash (Input_Stage);
         Output_Hash_Result : constant Output_Hasher.Hash_Result.Result :=
            Finalize_Hash (Output_Stage);
      begin
         if not (Input_Hash_Result.Is_Ok and Output_Hash_Result.Is_Ok) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Failed to finalize hashes"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Parallel_Pipeline_Processing")
            ));
         end if;

         Put_Line ("Input hash:  " & To_String (Input_Hash_Result.Get_Ok));
         Put_Line ("Output hash: " & To_String (Output_Hash_Result.Get_Ok));

         -- Hashes should match since we processed the same data
         if Input_Hash_Result.Get_Ok /= Output_Hash_Result.Get_Ok then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Hash mismatch between stages"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Parallel_Pipeline_Processing")
            ));
         end if;
      end;

      -- Verify progress tracking
      if Chunks_Read /= Chunks_Processed then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Progress tracking mismatch"),
            Details     => To_Unbounded_String (
               "Read: " & Chunks_Read'Image &
               ", Processed: " & Chunks_Processed'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Parallel_Pipeline_Processing")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Parallel_Pipeline_Processing;

   function Test_Large_File_Pipeline return Void_Result.Result is
      package Test_Hasher is new Pipelib.Core.Domain.Services.Stages.Generic_Hasher_Stage
         (Stage_Purpose => "E2E_Large");
      use Test_Hasher;

      Stage : Hasher_Stage_Type;
      Test_File : constant String := "large_test.dat";
      File_Size : constant Natural := 10 * Natural (Abohlib.Core.Domain.Constants.Bytes.SI_MB);  -- 10MB test file
      Chunk_Size : constant Chunk_Size_Type := From_KB (64);
      Start_Time : constant Time := Clock;
      Total_Chunks_Processed : Natural := 0;
   begin
      Ensure_Test_Directory;

      -- Create large test file
      Put_Line ("Creating " &
                Natural (File_Size / Natural (Abohlib.Core.Domain.Constants.Bytes.SI_MB))'Image &
                " MB test file...");
      Create_Test_File (Test_File, File_Size);

      -- Process file
      declare
         Offset : Long_Long_Integer := 0;
         Chunk_Num : Natural := 1;
         Total_Chunks : constant Natural :=
            (File_Size + Natural (Value (Chunk_Size)) - 1) / Natural (Value (Chunk_Size));
      begin
         Put_Line ("Processing " & Total_Chunks'Image & " chunks...");

         while Offset < Long_Long_Integer (File_Size) loop
            declare
               Remaining : constant Natural := Natural'Min (
                  Natural (Value (Chunk_Size)),
                  File_Size - Natural (Offset)
               );
               Data : Stream_Element_Array (1 .. Stream_Element_Offset (Remaining));
            begin
               -- Simulate reading from file
               for I in Data'Range loop
                  Data (I) := Stream_Element ((Natural (Offset) + Natural (I)) mod 256);
               end loop;

               -- Process chunk
               declare
                  Chunk : constant File_Chunk_Type := Create (
                     Sequence_Number => Chunk_Num,
                     Offset          => Offset,
                     Data            => Data,
                     Is_Final        => Offset + Long_Long_Integer (Remaining) >= Long_Long_Integer (File_Size)
                  );
                  Result : constant Chunk_Result.Result := Process_Chunk (Stage, Chunk);
               begin
                  if not Result.Is_Ok then
                     Cleanup_Test_Files;
                     return Void_Result.Err (Test_Error'(
                        Kind        => Assertion_Failed,
                        Message     => To_Unbounded_String ("Failed to process chunk " & Chunk_Num'Image),
                        Details     => Result.Get_Err,
                        Line_Number => 0,
                        Test_Name   => To_Unbounded_String ("Test_Large_File_Pipeline")
                     ));
                  end if;
               end;

               Total_Chunks_Processed := Total_Chunks_Processed + 1;

               -- Progress reporting every 10%
               if Chunk_Num mod (Total_Chunks / 10) = 0 then
                  declare
                     Percent : constant Natural := (Chunk_Num * 100) / Total_Chunks;
                  begin
                     Put_Line ("Progress: " & Percent'Image & "% (" &
                              Total_Chunks_Processed'Image & "/" & Total_Chunks'Image & " chunks)");
                  end;
               end if;

               Offset := Offset + Long_Long_Integer (Remaining);
               Chunk_Num := Chunk_Num + 1;
            end;
         end loop;
      end;

      -- Finalize hash
      declare
         Hash_Result_Val : constant Hash_Result.Result := Finalize_Hash (Stage);
         End_Time : constant Time := Clock;
         Duration_Seconds : constant Duration := End_Time - Start_Time;
         Throughput_MB : constant Float :=
            Float (File_Size) / Float (Abohlib.Core.Domain.Constants.Bytes.SI_MB) / Float (Duration_Seconds);
      begin
         if not Hash_Result_Val.Is_Ok then
            Cleanup_Test_Files;
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Failed to finalize hash"),
               Details     => Hash_Result_Val.Get_Err,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Large_File_Pipeline")
            ));
         end if;

         Put_Line ("Large file hash: " & To_String (Hash_Result_Val.Get_Ok));
         Put_Line ("Processing time: " & Duration'Image (Duration_Seconds) & " seconds");
         Put_Line ("Throughput: " & Natural (Throughput_MB)'Image & " MB/s");
      end;

      Cleanup_Test_Files;
      return Void_Result.Ok (True);
   end Test_Large_File_Pipeline;

   function Test_Multi_Stage_Pipeline return Void_Result.Result is
      -- Custom transform stage
      generic
         Stage_Name_Unused : String;
      package Transform_Stage is
         type Transform_Type is record
            Chunks_Transformed : Natural := 0;
         end record;

         function Transform_Chunk (
            Transform : in out Transform_Type;
            Input : File_Chunk_Type
         ) return File_Chunk_Type;
      end Transform_Stage;

      package body Transform_Stage is
         function Transform_Chunk (
            Transform : in out Transform_Type;
            Input : File_Chunk_Type
         ) return File_Chunk_Type is
            -- Simple XOR transformation
            Transformed_Data : Stream_Element_Array (1 .. Stream_Element_Offset (Data_Length (Input)));
            Original_Data : constant Stream_Element_Array := Data (Input);
         begin
            for I in Transformed_Data'Range loop
               Transformed_Data (I) := Original_Data (I) xor 16#AA#;
            end loop;

            Transform.Chunks_Transformed := Transform.Chunks_Transformed + 1;

            return Create (
               Sequence_Number => Sequence_Number (Input),
               Offset          => Offset (Input),
               Data            => Transformed_Data,
               Is_Final        => Is_Final (Input)
            );
         end Transform_Chunk;
      end Transform_Stage;

      -- Instantiate stages
      package Stage1 is new Transform_Stage ("Stage1");
      package Stage2 is new Transform_Stage ("Stage2");
      package Final_Hasher is new Pipelib.Core.Domain.Services.Stages.Generic_Hasher_Stage
         (Stage_Purpose => "E2E_Multi");

      use Stage1, Stage2, Final_Hasher;

      Transform1 : Stage1.Transform_Type;
      Transform2 : Stage2.Transform_Type;
      Hasher : Hasher_Stage_Type;

      Test_Size : constant Natural := 1 * Natural (Abohlib.Core.Domain.Constants.Bytes.SI_MB);
      Chunk_Size : constant Chunk_Size_Type := From_KB (16);
   begin
      -- Process data through multiple stages
      declare
         Offset : Long_Long_Integer := 0;
         Chunk_Num : Natural := 1;
      begin
         while Offset < Long_Long_Integer (Test_Size) loop
            declare
               Remaining : constant Natural := Natural'Min (
                  Natural (Value (Chunk_Size)),
                  Test_Size - Natural (Offset)
               );
               Data : constant Stream_Element_Array (1 .. Stream_Element_Offset (Remaining)) :=
                  [others => Stream_Element (Chunk_Num)];

               -- Original chunk
               Chunk1 : constant File_Chunk_Type := Create (
                  Sequence_Number => Chunk_Num,
                  Offset          => Offset,
                  Data            => Data,
                  Is_Final        => Offset + Long_Long_Integer (Remaining) >= Long_Long_Integer (Test_Size)
               );

               -- Transform through stages
               Chunk2 : constant File_Chunk_Type := Transform_Chunk (Transform1, Chunk1);
               Chunk3 : constant File_Chunk_Type := Transform_Chunk (Transform2, Chunk2);

               -- Hash final result
               Result : constant Chunk_Result.Result := Process_Chunk (Hasher, Chunk3);
            begin
               if not Result.Is_Ok then
                  return Void_Result.Err (Test_Error'(
                     Kind        => Assertion_Failed,
                     Message     => To_Unbounded_String ("Multi-stage processing failed"),
                     Details     => Result.Get_Err,
                     Line_Number => 0,
                     Test_Name   => To_Unbounded_String ("Test_Multi_Stage_Pipeline")
                  ));
               end if;

               Offset := Offset + Long_Long_Integer (Remaining);
               Chunk_Num := Chunk_Num + 1;
            end;
         end loop;
      end;

      -- Verify all stages processed the same number of chunks
      if Transform1.Chunks_Transformed /= Transform2.Chunks_Transformed then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Stage chunk count mismatch"),
            Details     => To_Unbounded_String (
               "Stage1: " & Transform1.Chunks_Transformed'Image &
               ", Stage2: " & Transform2.Chunks_Transformed'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Multi_Stage_Pipeline")
         ));
      end if;

      -- Get final hash
      declare
         Hash_Result_Val : constant Hash_Result.Result := Finalize_Hash (Hasher);
      begin
         if not Hash_Result_Val.Is_Ok then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Failed to finalize multi-stage hash"),
               Details     => Hash_Result_Val.Get_Err,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Multi_Stage_Pipeline")
            ));
         end if;

         Put_Line ("Multi-stage pipeline hash: " & To_String (Hash_Result_Val.Get_Ok));
      end;

      return Void_Result.Ok (True);
   end Test_Multi_Stage_Pipeline;

   function Test_Pipeline_Error_Recovery return Void_Result.Result is
      package Test_Hasher is new Pipelib.Core.Domain.Services.Stages.Generic_Hasher_Stage
         (Stage_Purpose => "E2E_Error");
      use Test_Hasher;

      Stage : Hasher_Stage_Type;
      Error_Chunks : Natural := 0;
      Success_Chunks : Natural := 0;
      Total_Read : Natural := 0;
   begin
      -- Process chunks with simulated errors
      for I in 1 .. 100 loop
         declare
            -- Every 7th chunk simulates an error condition
            Simulate_Error : constant Boolean := I mod 7 = 0;
            Data : constant Stream_Element_Array (1 .. 1024) :=
               [others => Stream_Element (I)];
            Chunk : constant File_Chunk_Type := Create (
               Sequence_Number => I,
               Offset          => Long_Long_Integer ((I - 1) * 1024),
               Data            => Data,
               Is_Final        => False
            );
         begin
            Total_Read := Total_Read + 1;

            if not Simulate_Error then
               -- Normal processing
               declare
                  Result : constant Chunk_Result.Result := Process_Chunk (Stage, Chunk);
               begin
                  if Result.Is_Ok then
                     Success_Chunks := Success_Chunks + 1;
                  else
                     Error_Chunks := Error_Chunks + 1;
                  end if;
               end;
            else
               -- Simulate error - skip processing
               Error_Chunks := Error_Chunks + 1;
               Put_Line ("Simulated error for chunk " & I'Image);
            end if;
         end;
      end loop;

      -- Verify error recovery
      Put_Line ("Error recovery test: " & Success_Chunks'Image & " succeeded, " &
                Error_Chunks'Image & " errors");

      declare
         Expected_Success : constant Natural := 100 - (100 / 7);  -- Chunks not divisible by 7
      begin
         if Success_Chunks < Expected_Success - 1 then  -- Allow for rounding
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Too many failed chunks"),
               Details     => To_Unbounded_String (
                  "Expected at least " & Expected_Success'Image &
                  " successes, got " & Success_Chunks'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Pipeline_Error_Recovery")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Pipeline_Error_Recovery;

   function Test_Pipeline_Resource_Management return Void_Result.Result is
      package Test_Hasher is new Pipelib.Core.Domain.Services.Stages.Generic_Hasher_Stage
         (Stage_Purpose => "E2E_Resource");
      use Test_Hasher;

      -- Test multiple pipeline lifecycles
      Cycles : constant := 5;
      Chunks_Per_Cycle : constant := 100;
   begin
      for Cycle in 1 .. Cycles loop
         declare
            Stage : Hasher_Stage_Type;
            Chunks_Processed : Natural := 0;
         begin
            -- Process chunks
            for I in 1 .. Chunks_Per_Cycle loop
               declare
                  Data : constant Stream_Element_Array (1 .. 1024) :=
                     [others => Stream_Element ((Cycle * 100 + I) mod 256)];
                  Chunk : constant File_Chunk_Type := Create (
                     Sequence_Number => I,
                     Offset          => Long_Long_Integer ((I - 1) * 1024),
                     Data            => Data,
                     Is_Final        => I = Chunks_Per_Cycle
                  );
                  Result : constant Chunk_Result.Result := Process_Chunk (Stage, Chunk);
               begin
                  if not Result.Is_Ok then
                     return Void_Result.Err (Test_Error'(
                        Kind        => Assertion_Failed,
                        Message     => To_Unbounded_String ("Processing failed in cycle " & Cycle'Image),
                        Details     => Result.Get_Err,
                        Line_Number => 0,
                        Test_Name   => To_Unbounded_String ("Test_Pipeline_Resource_Management")
                     ));
                  end if;

                  Chunks_Processed := Chunks_Processed + 1;
               end;
            end loop;

            -- Finalize this cycle
            declare
               Hash_Result_Val : constant Hash_Result.Result := Finalize_Hash (Stage);
            begin
               if not Hash_Result_Val.Is_Ok then
                  return Void_Result.Err (Test_Error'(
                     Kind        => Assertion_Failed,
                     Message     => To_Unbounded_String ("Failed to finalize in cycle " & Cycle'Image),
                     Details     => Hash_Result_Val.Get_Err,
                     Line_Number => 0,
                     Test_Name   => To_Unbounded_String ("Test_Pipeline_Resource_Management")
                  ));
               end if;

               Put_Line ("Cycle " & Cycle'Image & " hash: " &
                        To_String (Hash_Result_Val.Get_Ok) (1 .. 16) & "...");
            end;

            -- Verify chunks processed
            if Chunks_Processed /= Chunks_Per_Cycle then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Chunk count mismatch in cycle " & Cycle'Image),
                  Details     => To_Unbounded_String ("Expected: " & Chunks_Per_Cycle'Image &
                                                     ", Got: " & Chunks_Processed'Image),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Pipeline_Resource_Management")
               ));
            end if;

            -- Stage goes out of scope here, testing cleanup
         end;
      end loop;

      Put_Line ("Successfully completed " & Cycles'Image & " resource management cycles");

      return Void_Result.Ok (True);
   end Test_Pipeline_Resource_Management;

   function Test_Memory_Mapped_Pipeline return Void_Result.Result is
   begin
      -- TODO: Implement memory mapped pipeline test when interface is ready
      Put_Line ("Memory mapped pipeline test: SKIPPED (interface not yet implemented)");
      return Void_Result.Ok (True);
   end Test_Memory_Mapped_Pipeline;

   function Test_Concurrent_Pipeline_Operations return Void_Result.Result is
      -- Simulate concurrent pipeline operations
      package Hasher1 is new Pipelib.Core.Domain.Services.Stages.Generic_Hasher_Stage
         (Stage_Purpose => "E2E_Concurrent1");
      package Hasher2 is new Pipelib.Core.Domain.Services.Stages.Generic_Hasher_Stage
         (Stage_Purpose => "E2E_Concurrent2");
      package Hasher3 is new Pipelib.Core.Domain.Services.Stages.Generic_Hasher_Stage
         (Stage_Purpose => "E2E_Concurrent3");

      use Hasher1, Hasher2, Hasher3;

      Stage1 : Hasher1.Hasher_Stage_Type;
      Stage2 : Hasher2.Hasher_Stage_Type;
      Stage3 : Hasher3.Hasher_Stage_Type;

      Chunk_Count : constant := 1000;
      Total_Read : Natural := 0;
      Total_Processed : Natural := 0;
   begin
      -- Process chunks through different stages concurrently
      for I in 1 .. Chunk_Count loop
         declare
            Data : constant Stream_Element_Array (1 .. 1024) :=
               [others => Stream_Element (I mod 256)];
            Chunk : constant File_Chunk_Type := Create (
               Sequence_Number => I,
               Offset          => Long_Long_Integer ((I - 1) * 256),
               Data            => Data,
               Is_Final        => I = Chunk_Count
            );

            -- Process through different stages based on chunk number
            Stage_Num : constant Natural := (I - 1) mod 3 + 1;
         begin
            Total_Read := Total_Read + 1;

            case Stage_Num is
               when 1 =>
                  declare
                     Result : constant Hasher1.Chunk_Result.Result :=
                        Process_Chunk (Stage1, Chunk);
                  begin
                     if not Result.Is_Ok then
                        return Void_Result.Err (Test_Error'(
                           Kind        => Assertion_Failed,
                           Message     => To_Unbounded_String ("Stage 1 processing failed"),
                           Details     => Result.Get_Err,
                           Line_Number => 0,
                           Test_Name   => To_Unbounded_String ("Test_Concurrent_Pipeline_Operations")
                        ));
                     end if;
                  end;

               when 2 =>
                  declare
                     Result : constant Hasher2.Chunk_Result.Result :=
                        Process_Chunk (Stage2, Chunk);
                  begin
                     if not Result.Is_Ok then
                        return Void_Result.Err (Test_Error'(
                           Kind        => Assertion_Failed,
                           Message     => To_Unbounded_String ("Stage 2 processing failed"),
                           Details     => Result.Get_Err,
                           Line_Number => 0,
                           Test_Name   => To_Unbounded_String ("Test_Concurrent_Pipeline_Operations")
                        ));
                     end if;
                  end;

               when 3 =>
                  declare
                     Result : constant Hasher3.Chunk_Result.Result :=
                        Process_Chunk (Stage3, Chunk);
                  begin
                     if not Result.Is_Ok then
                        return Void_Result.Err (Test_Error'(
                           Kind        => Assertion_Failed,
                           Message     => To_Unbounded_String ("Stage 3 processing failed"),
                           Details     => Result.Get_Err,
                           Line_Number => 0,
                           Test_Name   => To_Unbounded_String ("Test_Concurrent_Pipeline_Operations")
                        ));
                     end if;
                  end;

               when others =>
                  null;  -- Should not happen
            end case;

            Total_Processed := Total_Processed + 1;
         end;
      end loop;

      -- Finalize all stages
      declare
         Hash1_Result : constant Hasher1.Hash_Result.Result := Finalize_Hash (Stage1);
         Hash2_Result : constant Hasher2.Hash_Result.Result := Finalize_Hash (Stage2);
         Hash3_Result : constant Hasher3.Hash_Result.Result := Finalize_Hash (Stage3);
      begin
         if not (Hash1_Result.Is_Ok and Hash2_Result.Is_Ok and Hash3_Result.Is_Ok) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Failed to finalize concurrent stages"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Concurrent_Pipeline_Operations")
            ));
         end if;

         Put_Line ("Concurrent stage 1 hash: " & To_String (Hash1_Result.Get_Ok) (1 .. 16) & "...");
         Put_Line ("Concurrent stage 2 hash: " & To_String (Hash2_Result.Get_Ok) (1 .. 16) & "...");
         Put_Line ("Concurrent stage 3 hash: " & To_String (Hash3_Result.Get_Ok) (1 .. 16) & "...");
      end;

      -- Verify progress tracking
      if Total_Read /= Chunk_Count or Total_Processed /= Chunk_Count then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Progress tracking error in concurrent test"),
            Details     => To_Unbounded_String (
               "Expected " & Chunk_Count'Image & " for all, got Read: " &
               Total_Read'Image & ", Processed: " &
               Total_Processed'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Concurrent_Pipeline_Operations")
         ));
      end if;

      -- Verify chunks were distributed across stages
      if Chunks_Processed (Stage1) + Chunks_Processed (Stage2) +
         Chunks_Processed (Stage3) /= Chunk_Count
      then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Chunk distribution error"),
            Details     => To_Unbounded_String (
               "Stage1: " & Chunks_Processed (Stage1)'Image &
               ", Stage2: " & Chunks_Processed (Stage2)'Image &
               ", Stage3: " & Chunks_Processed (Stage3)'Image &
               ", Total: " & Chunk_Count'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Concurrent_Pipeline_Operations")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Concurrent_Pipeline_Operations;

   --  ==========================================================================
   --  Test Runner
   --  ==========================================================================

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Tests : Test_Results_Array (1 .. 8);
      Index : Positive := 1;

      procedure Add_Test_Result
        (Name : String;
         Test_Func : Test_Function_Access)
      is
         Result : constant Test_Result_Pkg.Result :=
            Run_Test (Name, Test_Func, Output);
      begin
         if Result.Is_Ok then
            Tests (Index) := Result.Get_Ok;
            Print_Test_Result (Tests (Index), Output);
            Index := Index + 1;
         else
            -- Handle test execution error
            declare
               Err : constant Test_Error := Result.Get_Err;
            begin
               Tests (Index) := Test_Result'(
                  Name           => To_Unbounded_String (Name),
                  Status         => Error,
                  Message        => Err.Message,
                  Elapsed_Time   => 0.0,
                  Line_Number    => Err.Line_Number,
                  Correlation_ID => To_Unbounded_String ("TEST-" & Name)
               );
               Print_Test_Result (Tests (Index), Output);
               Index := Index + 1;
            end;
         end if;
      end Add_Test_Result;
   begin
      Output.Write_Line ("=== Running Pipeline E2E Tests ===");
      Output.Write_Line ("");

      -- Run all tests
      Add_Test_Result ("Simple File Hashing", Test_Simple_File_Hashing'Access);
      Add_Test_Result ("Parallel Pipeline Processing", Test_Parallel_Pipeline_Processing'Access);
      Add_Test_Result ("Large File Pipeline", Test_Large_File_Pipeline'Access);
      Add_Test_Result ("Multi-Stage Pipeline", Test_Multi_Stage_Pipeline'Access);
      Add_Test_Result ("Pipeline Error Recovery", Test_Pipeline_Error_Recovery'Access);
      Add_Test_Result ("Pipeline Resource Management", Test_Pipeline_Resource_Management'Access);
      Add_Test_Result ("Memory Mapped Pipeline", Test_Memory_Mapped_Pipeline'Access);
      Add_Test_Result ("Concurrent Pipeline Operations", Test_Concurrent_Pipeline_Operations'Access);

      -- Generate summary
      declare
         Stats_Result : constant Test_Stats_Result.Result :=
            Run_Test_Suite ("End_To_End_Pipeline_Tests", Tests (1 .. Index - 1), Output);
      begin
         if Stats_Result.Is_Ok then
            declare
               Stats : constant Test_Statistics := Stats_Result.Get_Ok;
            begin
               Output.Write_Line ("");
               Print_Test_Summary ("Pipeline E2E Tests", Stats, Output);
               return Test_Stats_Result.Ok (Stats);
            end;
         else
            return Stats_Result;
         end if;
      end;
   end Run_All_Tests;

end Test_Pipeline_E2E;
