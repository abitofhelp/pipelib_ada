--  =============================================================================
--  Test_Generic_Hasher_Stage - Generic Hasher Stage Unit Tests Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams; use Ada.Streams;
with Pipelib.Core.Domain.Services.Stages.Generic_Hasher_Stage;
with Pipelib.Core.Domain.Value_Objects.File_Chunk; use Pipelib.Core.Domain.Value_Objects.File_Chunk;

package body Test_Generic_Hasher_Stage is

   --  Instantiate the hasher stage for testing
   package Test_Hasher is new Pipelib.Core.Domain.Services.Stages.Generic_Hasher_Stage
      (Stage_Purpose => "Test_Hasher");
   use Test_Hasher;

   --  ==========================================================================
   --  Test Implementation
   --  ==========================================================================

   function Test_Create_Hasher_Stage return Void_Result.Result is
      Stage : Hasher_Stage_Type;
   begin
      -- Test stage creation and initial state
      if Is_Finalized (Stage) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("New stage should not be finalized"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_Hasher_Stage")
         ));
      end if;

      if Chunks_Processed (Stage) /= 0 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Initial chunks count should be 0"),
            Details     => To_Unbounded_String ("Got: " & Chunks_Processed (Stage)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_Hasher_Stage")
         ));
      end if;

      if Bytes_Hashed (Stage) /= 0 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Initial bytes count should be 0"),
            Details     => To_Unbounded_String ("Got: " & Bytes_Hashed (Stage)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_Hasher_Stage")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Create_Hasher_Stage;

   function Test_Process_Single_Chunk return Void_Result.Result is
      Stage : Hasher_Stage_Type;
      Test_Data : constant Stream_Element_Array (1 .. 1024) := [others => 42];
      Chunk : constant File_Chunk_Type := Create (
         Sequence_Number => 1,
         Offset          => 0,
         Data            => Test_Data,
         Is_Final        => False
      );
   begin
      -- Process a single chunk
      declare
         Result : constant Chunk_Result.Result := Process_Chunk (Stage, Chunk);
      begin
         if not Result.Is_Ok then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Process_Chunk failed"),
               Details     => Result.Get_Err,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Process_Single_Chunk")
            ));
         end if;
      end;

      -- Verify statistics updated
      if Chunks_Processed (Stage) /= 1 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Chunks count incorrect"),
            Details     => To_Unbounded_String ("Expected: 1, Got: " & Chunks_Processed (Stage)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Process_Single_Chunk")
         ));
      end if;

      if Bytes_Hashed (Stage) /= 1024 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Bytes count incorrect"),
            Details     => To_Unbounded_String ("Expected: 1024, Got: " & Bytes_Hashed (Stage)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Process_Single_Chunk")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Process_Single_Chunk;

   function Test_Process_Multiple_Chunks return Void_Result.Result is
      Stage : Hasher_Stage_Type;
      Test_Data1 : constant Stream_Element_Array (1 .. 1024) := [others => 1];
      Test_Data2 : constant Stream_Element_Array (1 .. 1024) := [others => 2];
      Test_Data3 : constant Stream_Element_Array (1 .. 1024) := [others => 3];
   begin
      -- Process multiple chunks
      declare
         Chunk1 : constant File_Chunk_Type := Create (1, 0, Test_Data1, False);
         Chunk2 : constant File_Chunk_Type := Create (2, 1024, Test_Data2, False);
         Chunk3 : constant File_Chunk_Type := Create (3, 2048, Test_Data3, False);

         Result1 : constant Chunk_Result.Result := Process_Chunk (Stage, Chunk1);
         Result2 : constant Chunk_Result.Result := Process_Chunk (Stage, Chunk2);
         Result3 : constant Chunk_Result.Result := Process_Chunk (Stage, Chunk3);
      begin
         if not (Result1.Is_Ok and Result2.Is_Ok and Result3.Is_Ok) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Failed to process chunks"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Process_Multiple_Chunks")
            ));
         end if;
      end;

      -- Verify cumulative statistics
      if Chunks_Processed (Stage) /= 3 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Chunks count incorrect"),
            Details     => To_Unbounded_String ("Expected: 3, Got: " & Chunks_Processed (Stage)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Process_Multiple_Chunks")
         ));
      end if;

      if Bytes_Hashed (Stage) /= 3072 then  -- 1024 + 1024 + 1024
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Bytes count incorrect"),
            Details     => To_Unbounded_String ("Expected: 1792, Got: " & Bytes_Hashed (Stage)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Process_Multiple_Chunks")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Process_Multiple_Chunks;

   function Test_Finalize_Hash return Void_Result.Result is
      Stage : Hasher_Stage_Type;
      Test_Data : constant Stream_Element_Array (1 .. 1024) := [others => 255];
      Chunk : constant File_Chunk_Type := Create (1, 0, Test_Data, True);
   begin
      -- Process a final chunk
      declare
         Result : constant Chunk_Result.Result := Process_Chunk (Stage, Chunk);
      begin
         if not Result.Is_Ok then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Failed to process final chunk"),
               Details     => Result.Get_Err,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Finalize_Hash")
            ));
         end if;
      end;

      -- Finalize the hash
      declare
         Result : constant Hash_Result.Result := Finalize_Hash (Stage);
      begin
         if not Result.Is_Ok then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Finalize_Hash failed"),
               Details     => Result.Get_Err,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Finalize_Hash")
            ));
         end if;

         -- Verify hash is 64 characters (SHA-256)
         declare
            Hash : constant String := To_String (Result.Get_Ok);
         begin
            if Hash'Length /= 64 then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Hash length incorrect"),
                  Details     => To_Unbounded_String ("Expected: 64, Got: " & Hash'Length'Image),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Finalize_Hash")
               ));
            end if;
         end;
      end;

      -- Verify stage is marked as finalized
      if not Is_Finalized (Stage) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Stage should be finalized"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Finalize_Hash")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Finalize_Hash;

   function Test_Reset_Stage return Void_Result.Result is
      Stage : Hasher_Stage_Type;
      Test_Data : constant Stream_Element_Array (1 .. 1024) := [others => 128];
      Chunk : constant File_Chunk_Type := Create (1, 0, Test_Data, False);
   begin
      -- Process a chunk
      declare
         Result : constant Chunk_Result.Result := Process_Chunk (Stage, Chunk);
      begin
         if not Result.Is_Ok then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Failed to process chunk"),
               Details     => Result.Get_Err,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Reset_Stage")
            ));
         end if;
      end;

      -- Reset the stage
      Reset (Stage);

      -- Verify statistics are reset
      if Chunks_Processed (Stage) /= 0 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Chunks count not reset"),
            Details     => To_Unbounded_String ("Got: " & Chunks_Processed (Stage)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Reset_Stage")
         ));
      end if;

      if Bytes_Hashed (Stage) /= 0 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Bytes count not reset"),
            Details     => To_Unbounded_String ("Got: " & Bytes_Hashed (Stage)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Reset_Stage")
         ));
      end if;

      if Is_Finalized (Stage) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Stage should not be finalized after reset"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Reset_Stage")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Reset_Stage;

   function Test_Statistics_Tracking return Void_Result.Result is
      Stage : Hasher_Stage_Type;
      Total_Bytes : Natural := 0;
      Total_Chunks : Natural := 0;
   begin
      -- Process chunks of various sizes
      for I in 1 .. 5 loop
         declare
            Size : constant Natural := 1024 + (I - 1) * 256;  -- 1024, 1280, 1536, 1792, 2048
            Test_Data : constant Stream_Element_Array (1 .. Stream_Element_Offset (Size)) :=
               [others => Stream_Element (I)];
            Chunk : constant File_Chunk_Type := Create (I,
               Long_Long_Integer (Total_Bytes), Test_Data, I = 5);
            Result : constant Chunk_Result.Result := Process_Chunk (Stage, Chunk);
         begin
            if not Result.Is_Ok then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Failed to process chunk " & I'Image),
                  Details     => Result.Get_Err,
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Statistics_Tracking")
               ));
            end if;

            Total_Bytes := Total_Bytes + Size;
            Total_Chunks := Total_Chunks + 1;
         end;
      end loop;

      -- Verify final statistics
      if Chunks_Processed (Stage) /= Total_Chunks then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Final chunks count incorrect"),
            Details     => To_Unbounded_String ("Expected: " & Total_Chunks'Image &
                                               ", Got: " & Chunks_Processed (Stage)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Statistics_Tracking")
         ));
      end if;

      if Bytes_Hashed (Stage) /= Long_Long_Integer (Total_Bytes) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Final bytes count incorrect"),
            Details     => To_Unbounded_String ("Expected: " & Total_Bytes'Image &
                                               ", Got: " & Bytes_Hashed (Stage)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Statistics_Tracking")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Statistics_Tracking;

   function Test_Invalid_Operations return Void_Result.Result is
      Stage : Hasher_Stage_Type;
      Test_Data : constant Stream_Element_Array (1 .. 1024) := [others => 50];
      Chunk : constant File_Chunk_Type := Create (1, 0, Test_Data, False);
   begin
      -- Process and finalize
      declare
         Process_Result : constant Chunk_Result.Result := Process_Chunk (Stage, Chunk);
         Hash_Result : constant Test_Hasher.Hash_Result.Result := Finalize_Hash (Stage);
      begin
         if not (Process_Result.Is_Ok and Hash_Result.Is_Ok) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Initial processing failed"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Invalid_Operations")
            ));
         end if;
      end;

      -- Try to process after finalization (should fail)
      begin
         declare
            Result : constant Chunk_Result.Result := Process_Chunk (Stage, Chunk);
            pragma Unreferenced (Result);
         begin
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Should not process after finalization"),
               Details     => To_Unbounded_String ("Expected precondition failure"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Invalid_Operations")
            ));
         end;
      exception
         when others =>
            null;  -- Expected precondition failure
      end;

      -- Try to finalize again (should fail)
      begin
         declare
            Result : constant Test_Hasher.Hash_Result.Result := Finalize_Hash (Stage);
            pragma Unreferenced (Result);
         begin
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Should not finalize twice"),
               Details     => To_Unbounded_String ("Expected precondition failure"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Invalid_Operations")
            ));
         end;
      exception
         when others =>
            null;  -- Expected precondition failure
      end;

      return Void_Result.Ok (True);
   end Test_Invalid_Operations;

   function Test_Empty_Data_Handling return Void_Result.Result is
      Stage : Hasher_Stage_Type;
   begin
      -- Finalize without processing any chunks
      declare
         Result : constant Hash_Result.Result := Finalize_Hash (Stage);
      begin
         if not Result.Is_Ok then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Failed to finalize empty hash"),
               Details     => Result.Get_Err,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Empty_Data_Handling")
            ));
         end if;

         -- Empty data should still produce a valid hash
         declare
            Hash : constant String := To_String (Result.Get_Ok);
         begin
            if Hash'Length /= 64 then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Empty data hash length incorrect"),
                  Details     => To_Unbounded_String ("Expected: 64, Got: " & Hash'Length'Image),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Empty_Data_Handling")
               ));
            end if;
         end;
      end;

      return Void_Result.Ok (True);
   end Test_Empty_Data_Handling;

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
      Output.Write_Line ("=== Running Generic Hasher Stage Unit Tests ===");
      Output.Write_Line ("");

      -- Run all tests
      Add_Test_Result ("Test_Create_Hasher_Stage", Test_Create_Hasher_Stage'Access);
      Add_Test_Result ("Test_Process_Single_Chunk", Test_Process_Single_Chunk'Access);
      Add_Test_Result ("Test_Process_Multiple_Chunks", Test_Process_Multiple_Chunks'Access);
      Add_Test_Result ("Test_Finalize_Hash", Test_Finalize_Hash'Access);
      Add_Test_Result ("Test_Reset_Stage", Test_Reset_Stage'Access);
      Add_Test_Result ("Test_Statistics_Tracking", Test_Statistics_Tracking'Access);
      Add_Test_Result ("Test_Invalid_Operations", Test_Invalid_Operations'Access);
      Add_Test_Result ("Test_Empty_Data_Handling", Test_Empty_Data_Handling'Access);

      -- Generate summary
      declare
         Stats_Result : constant Test_Stats_Result.Result :=
            Run_Test_Suite ("Generic_Hasher_Stage_Tests", Tests (1 .. Index - 1), Output);
      begin
         if Stats_Result.Is_Ok then
            declare
               Stats : constant Test_Statistics := Stats_Result.Get_Ok;
            begin
               Output.Write_Line ("");
               Print_Test_Summary ("Generic Hasher Stage Unit Tests", Stats, Output);
               return Test_Stats_Result.Ok (Stats);
            end;
         else
            return Stats_Result;
         end if;
      end;
   end Run_All_Tests;

end Test_Generic_Hasher_Stage;
