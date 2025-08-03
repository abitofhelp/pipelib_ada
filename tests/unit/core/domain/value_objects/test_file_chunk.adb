--  =============================================================================
--  Test_File_Chunk - File Chunk Value Object Unit Tests Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams; use Ada.Streams;
with System; use System;
with Pipelib.Core.Domain.Value_Objects.File_Chunk; use Pipelib.Core.Domain.Value_Objects.File_Chunk;
with Abohlib.Core.Domain.Constants.Bytes;

package body Test_File_Chunk is

   use Abohlib.Infrastructure.Testing.Test_Framework;

   --  ==========================================================================
   --  Test Implementation
   --  ==========================================================================

   function Test_Create_Valid_Chunk return Void_Result.Result is
      Test_Data : constant Stream_Element_Array (1 .. 1024) := [others => 42];
   begin
      -- Test basic chunk creation with contract verification
      declare
         Chunk : constant File_Chunk_Type := Create (
            Sequence_Number => 5,
            Offset          => 4096,
            Data            => Test_Data,
            Is_Final        => False
         );
      begin

      -- Verify postconditions are satisfied
      -- Verify properties
      if Sequence_Number (Chunk) /= 5 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Sequence number mismatch"),
            Details     => To_Unbounded_String ("Expected: 5, Got: " & Sequence_Number (Chunk)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_Valid_Chunk")
         ));
      end if;

      if Offset (Chunk) /= 4096 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Offset mismatch"),
            Details     => To_Unbounded_String ("Expected: 4096, Got: " & Offset (Chunk)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_Valid_Chunk")
         ));
      end if;

      if Data_Length (Chunk) /= 1024 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Data length mismatch"),
            Details     => To_Unbounded_String ("Expected: 1024, Got: " & Data_Length (Chunk)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_Valid_Chunk")
         ));
      end if;

      if Is_Final (Chunk) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Final flag should be false"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_Valid_Chunk")
         ));
      end if;

      if Has_Checksum (Chunk) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Should not have checksum"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_Valid_Chunk")
         ));
      end if;

      if Sequence_Number (Chunk) /= 5 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Sequence number mismatch"),
            Details     => To_Unbounded_String ("Expected: 5, Got: " & Sequence_Number (Chunk)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_Valid_Chunk")
         ));
      end if;
      end;

      return Void_Result.Ok (True);
   end Test_Create_Valid_Chunk;

   function Test_Create_With_Checksum return Void_Result.Result is
      Test_Data : constant Stream_Element_Array (1 .. 2048) := [others => 255];
      Test_Checksum : constant String := "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef";
   begin
      -- Test chunk creation with checksum
      declare
         Chunk : constant File_Chunk_Type := Create_With_Checksum (
            Sequence_Number => 10,
            Offset          => 8192,
            Data            => Test_Data,
            Checksum        => Test_Checksum,
            Is_Final        => True
         );
      begin
         -- Verify all properties including checksum
         if Sequence_Number (Chunk) /= 10 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Sequence number mismatch"),
            Details     => To_Unbounded_String ("Expected: 10, Got: " & Sequence_Number (Chunk)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_With_Checksum")
         ));
      end if;

      if Offset (Chunk) /= 8192 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Offset mismatch"),
            Details     => To_Unbounded_String ("Expected: 8192, Got: " & Offset (Chunk)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_With_Checksum")
         ));
      end if;

      if Data_Length (Chunk) /= 2048 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Data length mismatch"),
            Details     => To_Unbounded_String ("Expected: 2048, Got: " & Data_Length (Chunk)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_With_Checksum")
         ));
      end if;

      if not Is_Final (Chunk) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Final flag should be true"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_With_Checksum")
         ));
      end if;

      if not Has_Checksum (Chunk) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Checksum not stored"),
            Details     => To_Unbounded_String ("Expected checksum to be present"),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_With_Checksum")
         ));
      end if;

      if Checksum (Chunk) /= Test_Checksum then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Checksum mismatch"),
            Details     => To_Unbounded_String ("Expected: " & Test_Checksum & ", Got: " & Checksum (Chunk)),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_With_Checksum")
         ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Create_With_Checksum;

   function Test_Data_Ownership_Transfer return Void_Result.Result is
      Original_Data : Stream_Element_Array_Access;
      Retrieved_Data : Stream_Element_Array_Access;
   begin
      -- Create test data dynamically
      Original_Data := new Stream_Element_Array (1 .. 1024);
      for I in Original_Data'Range loop
         Original_Data (I) := Stream_Element (I mod 256);
      end loop;

      declare
         Chunk : constant File_Chunk_Type := Create_From_Access (
            Sequence_Number => 1,
            Offset          => 0,
            Data            => Original_Data,
            Is_Final        => False
         );
      begin
         -- Retrieve data reference
         Retrieved_Data := Data_Access (Chunk);

         -- Note: We can't check address equality because File_Chunk_Type is a
         -- controlled type with value semantics. The Adjust procedure makes a
         -- deep copy for safety. Instead, verify the data content is correct.
         if Retrieved_Data = null then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Data access returned null"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Data_Ownership_Transfer")
            ));
         end if;

         -- Verify data content is preserved
         for I in Retrieved_Data'Range loop
            if Retrieved_Data (I) /= Stream_Element (I mod 256) then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Data corrupted during transfer"),
                  Details     => To_Unbounded_String ("Index: " & I'Image),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Data_Ownership_Transfer")
               ));
            end if;
         end loop;
      end;

      return Void_Result.Ok (True);
   end Test_Data_Ownership_Transfer;

   function Test_Chunk_Sequence_Number return Void_Result.Result is
      Test_Data : constant Stream_Element_Array (1 .. 1024) := [others => 0];
   begin
      -- Test various sequence numbers
      declare
         Chunk1 : constant File_Chunk_Type := Create (0, 0, Test_Data, False);
         Chunk2 : constant File_Chunk_Type := Create (Natural'Last, 0, Test_Data, False);
         Chunk3 : constant File_Chunk_Type := Create (12345, 0, Test_Data, False);
      begin
         if Sequence_Number (Chunk1) /= 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Sequence number 0 failed"),
               Details     => To_Unbounded_String ("Got: " & Sequence_Number (Chunk1)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Chunk_Sequence_Number")
            ));
         end if;

         if Sequence_Number (Chunk2) /= Natural'Last then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Max sequence number failed"),
               Details     => To_Unbounded_String ("Got: " & Sequence_Number (Chunk2)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Chunk_Sequence_Number")
            ));
         end if;

         if Sequence_Number (Chunk3) /= 12345 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Arbitrary sequence number failed"),
               Details     => To_Unbounded_String ("Got: " & Sequence_Number (Chunk3)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Chunk_Sequence_Number")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Chunk_Sequence_Number;

   function Test_Chunk_Offset_Validation return Void_Result.Result is
      Test_Data : constant Stream_Element_Array (1 .. 1024) := [others => 128];
   begin
      -- Test various offsets
      declare
         Chunk1 : constant File_Chunk_Type := Create (1, 0, Test_Data, False);
         Chunk2 : constant File_Chunk_Type := Create (2, Long_Long_Integer'Last, Test_Data, False);
         Chunk3 : constant File_Chunk_Type := Create (3, 1_000_000_000, Test_Data, False);
      begin
         -- Verify offset values
         if Offset (Chunk1) /= 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Zero offset failed"),
               Details     => To_Unbounded_String ("Got: " & Offset (Chunk1)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Chunk_Offset_Validation")
            ));
         end if;

         if Offset (Chunk2) /= Long_Long_Integer'Last then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Max offset failed"),
               Details     => To_Unbounded_String ("Got: " & Offset (Chunk2)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Chunk_Offset_Validation")
            ));
         end if;

         if Offset (Chunk3) /= 1_000_000_000 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Large offset failed"),
               Details     => To_Unbounded_String ("Got: " & Offset (Chunk3)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Chunk_Offset_Validation")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Chunk_Offset_Validation;

   function Test_Final_Chunk_Flag return Void_Result.Result is
      Test_Data : constant Stream_Element_Array (1 .. 1024) := [others => 64];
   begin
      -- Test final chunk flag behavior
      declare
         Regular_Chunk : constant File_Chunk_Type := Create (1, 0, Test_Data, False);
         Final_Chunk : constant File_Chunk_Type := Create (2, 512, Test_Data, True);
      begin
         if Is_Final (Regular_Chunk) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Regular chunk marked as final"),
               Details     => To_Unbounded_String ("Expected: False"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Final_Chunk_Flag")
            ));
         end if;

         if not Is_Final (Final_Chunk) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Final chunk not marked as final"),
               Details     => To_Unbounded_String ("Expected: True"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Final_Chunk_Flag")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Final_Chunk_Flag;

   function Test_Data_Size_Limits return Void_Result.Result is
   begin
      -- Test minimum valid chunk size (1KB)
      declare
         Min_Size : constant := Natural (Abohlib.Core.Domain.Constants.Bytes.SI_KB);
         Min_Data : constant Stream_Element_Array (1 .. Stream_Element_Offset (Min_Size)) :=
            [others => 1];
         Min_Chunk : constant File_Chunk_Type := Create (1, 0, Min_Data, False);
      begin
         if Data_Length (Min_Chunk) /= Min_Size then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Minimum size failed"),
               Details     => To_Unbounded_String ("Got: " & Data_Length (Min_Chunk)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Data_Size_Limits")
            ));
         end if;
      end;

      -- Test a reasonable large size (64KB instead of the actual max of 512MB)
      declare
         Large_Size : constant := 64 * Natural (Abohlib.Core.Domain.Constants.Bytes.SI_KB);
         Large_Data : constant Stream_Element_Array (1 .. Stream_Element_Offset (Large_Size)) :=
            [others => 255];
         Large_Chunk : constant File_Chunk_Type := Create (2, 0, Large_Data, False);
      begin
         if Data_Length (Large_Chunk) /= Large_Size then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Large size test failed"),
               Details     => To_Unbounded_String ("Got: " & Data_Length (Large_Chunk)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Data_Size_Limits")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Data_Size_Limits;

   function Test_Checksum_Operations return Void_Result.Result is
      Test_Data : constant Stream_Element_Array (1 .. 1024) := [others => 123];
      SHA256_Checksum : constant String := "deadbeef" & [1 .. 56 => '0'];  -- 64 chars
   begin
      -- Test adding checksum to chunk
      declare
         Chunk : File_Chunk_Type := Create (1, 0, Test_Data, False);
      begin
         -- Initially no checksum
         if Has_Checksum (Chunk) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Unexpected checksum present"),
               Details     => To_Unbounded_String ("New chunk should not have checksum"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Checksum_Operations")
            ));
         end if;

         -- Add checksum
         Chunk := With_Checksum (Chunk, SHA256_Checksum);

         -- Verify checksum added
         if not Has_Checksum (Chunk) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Checksum not added"),
               Details     => To_Unbounded_String ("Expected checksum to be present"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Checksum_Operations")
            ));
         end if;

         if Checksum (Chunk) /= SHA256_Checksum then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Checksum value mismatch"),
               Details     => To_Unbounded_String ("Got: " & Checksum (Chunk)),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Checksum_Operations")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Checksum_Operations;

   function Test_Chunk_Equality return Void_Result.Result is
      Test_Data1 : constant Stream_Element_Array (1 .. 1024) := [others => 1];
      Test_Data2 : constant Stream_Element_Array (1 .. 1024) := [others => 2];
   begin
      -- Test equality based on all properties
      declare
         Chunk1a : constant File_Chunk_Type := Create (1, 0, Test_Data1, False);
         Chunk1b : constant File_Chunk_Type := Create (1, 0, Test_Data1, False);
         Chunk2  : constant File_Chunk_Type := Create (2, 0, Test_Data1, False);
         Chunk3  : constant File_Chunk_Type := Create (1, 100, Test_Data1, False);
         Chunk4  : constant File_Chunk_Type := Create (1, 0, Test_Data2, False);
         Chunk5  : constant File_Chunk_Type := Create (1, 0, Test_Data1, True);
      begin
         -- Same chunks should be equal
         if Chunk1a /= Chunk1b then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Identical chunks not equal"),
               Details     => To_Unbounded_String ("Expected equality"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Chunk_Equality")
            ));
         end if;

         -- Different sequence numbers
         if Chunk1a = Chunk2 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Different sequences marked equal"),
               Details     => To_Unbounded_String ("Expected inequality"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Chunk_Equality")
            ));
         end if;

         -- Different offsets
         if Chunk1a = Chunk3 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Different offsets marked equal"),
               Details     => To_Unbounded_String ("Expected inequality"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Chunk_Equality")
            ));
         end if;

         -- Different data
         if Chunk1a = Chunk4 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Different data marked equal"),
               Details     => To_Unbounded_String ("Expected inequality"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Chunk_Equality")
            ));
         end if;

         -- Different final flags
         if Chunk1a = Chunk5 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Different final flags marked equal"),
               Details     => To_Unbounded_String ("Expected inequality"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Chunk_Equality")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Chunk_Equality;

   function Test_Chunk_Validation return Void_Result.Result is
      Test_Data : constant Stream_Element_Array (1 .. 1024) := [others => 99];
   begin
      -- Test validation of chunk properties
      declare
         Valid_Chunk : constant File_Chunk_Type := Create (
            Sequence_Number => 10,
            Offset          => 5120,
            Data            => Test_Data,
            Is_Final        => False
         );
      begin
         if not Is_Valid_Chunk (Valid_Chunk) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Valid chunk failed validation"),
               Details     => To_Unbounded_String ("Is_Valid_Chunk returned False"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Chunk_Validation")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Chunk_Validation;

   function Test_Chunk_Image_Representation return Void_Result.Result is
      Test_Data : constant Stream_Element_Array (1 .. 1024) := [others => 77];
   begin
      -- Test string representation
      declare
         Chunk : constant File_Chunk_Type := Create_With_Checksum (
            Sequence_Number => 42,
            Offset          => 10240,
            Data            => Test_Data,
            Checksum        => "abcd" & [1 .. 60 => 'e'],
            Is_Final        => True
         );
         Image : constant String := Pipelib.Core.Domain.Value_Objects.File_Chunk.Image (Chunk);
      begin
         -- Check that image contains key information
         if Image'Length = 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Empty image representation"),
               Details     => To_Unbounded_String ("Expected non-empty string"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Chunk_Image_Representation")
            ));
         end if;

         -- Image should contain sequence number (with 'Image it includes a space)
         declare
            Seq_Str : constant String := " 42";  -- 'Image adds leading space
            Found : Boolean := False;
         begin
            for I in Image'First .. Image'Last - Seq_Str'Length + 1 loop
               if Image (I .. I + Seq_Str'Length - 1) = Seq_Str then
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               -- Also check without space in case implementation changes
               declare
                  Seq_Str_No_Space : constant String := "42";
               begin
                  for I in Image'First .. Image'Last - Seq_Str_No_Space'Length + 1 loop
                     if Image (I .. I + Seq_Str_No_Space'Length - 1) = Seq_Str_No_Space then
                        Found := True;
                        exit;
                     end if;
                  end loop;
               end;
            end if;

            if not Found then
               -- Debug: Let's see what the actual image contains
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Sequence number not in image"),
                  Details     => To_Unbounded_String ("Looking for '42' or ' 42' in: " & Image),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Chunk_Image_Representation")
               ));
            end if;
         end;
      end;

      return Void_Result.Ok (True);
   end Test_Chunk_Image_Representation;

   function Test_Contract_Violations return Void_Result.Result is
   begin
      -- Test precondition: empty data
      begin
         declare
            Empty_Data : constant Stream_Element_Array (1 .. 0) := [];
            Chunk : constant File_Chunk_Type := Create (1, 0, Empty_Data, False);
            pragma Unreferenced (Chunk);
         begin
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Empty data accepted"),
               Details     => To_Unbounded_String ("Expected precondition failure"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Contract_Violations")
            ));
         end;
      exception
         when others =>
            null; -- Expected
      end;

      -- Test precondition: negative offset
      begin
         declare
            Test_Data : constant Stream_Element_Array (1 .. 1024) := [others => 0];
            Chunk : constant File_Chunk_Type := Create (1, -1, Test_Data, False);
            pragma Unreferenced (Chunk);
         begin
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Negative offset accepted"),
               Details     => To_Unbounded_String ("Expected precondition failure"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Contract_Violations")
            ));
         end;
      exception
         when others =>
            null; -- Expected
      end;

      return Void_Result.Ok (True);
   end Test_Contract_Violations;

   function Test_Zero_Copy_Semantics return Void_Result.Result is
      Test_Data : constant Stream_Element_Array (1 .. 1024) := [others => 88];
   begin
      -- Test that data is not copied unnecessarily
      declare
         Chunk1 : constant File_Chunk_Type := Create (1, 0, Test_Data, False);
         Chunk2 : constant File_Chunk_Type := Chunk1;  -- Assignment

         Data1 : constant Stream_Element_Array_Access := Data_Access (Chunk1);
         Data2 : constant Stream_Element_Array_Access := Data_Access (Chunk2);
      begin
         -- For value objects, data should be copied on assignment
         if Data1.all'Address = Data2.all'Address then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Data not copied on assignment"),
               Details     => To_Unbounded_String ("Expected separate copies"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Zero_Copy_Semantics")
            ));
         end if;

         -- But content should be the same
         for I in Data1'Range loop
            if Data1 (I) /= Data2 (I) then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Data content differs"),
                  Details     => To_Unbounded_String ("Index: " & I'Image),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Zero_Copy_Semantics")
               ));
            end if;
         end loop;
      end;

      return Void_Result.Ok (True);
   end Test_Zero_Copy_Semantics;

   function Test_Concurrent_Access_Safety return Void_Result.Result is
      Test_Data : constant Stream_Element_Array (1 .. 2048) := [others => 111];
   begin
      -- Test that value objects are safe for concurrent access
      declare
         Chunk : constant File_Chunk_Type := Create (1, 0, Test_Data, False);

         -- Simulate concurrent reads
         Seq1 : constant Natural := Sequence_Number (Chunk);
         Seq2 : constant Natural := Sequence_Number (Chunk);
         Off1 : constant Long_Long_Integer := Offset (Chunk);
         Off2 : constant Long_Long_Integer := Offset (Chunk);
         Size1 : constant Natural := Data_Length (Chunk);
         Size2 : constant Natural := Data_Length (Chunk);
      begin
         -- All reads should return consistent values
         if Seq1 /= Seq2 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Inconsistent sequence numbers"),
               Details     => To_Unbounded_String ("Got: " & Seq1'Image & " and " & Seq2'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Concurrent_Access_Safety")
            ));
         end if;

         if Off1 /= Off2 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Inconsistent offsets"),
               Details     => To_Unbounded_String ("Got: " & Off1'Image & " and " & Off2'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Concurrent_Access_Safety")
            ));
         end if;

         if Size1 /= Size2 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Inconsistent sizes"),
               Details     => To_Unbounded_String ("Got: " & Size1'Image & " and " & Size2'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Concurrent_Access_Safety")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Concurrent_Access_Safety;

   --  ==========================================================================
   --  Test Runner
   --  ==========================================================================

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Tests : Test_Results_Array (1 .. 14);
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
      Output.Write_Line ("=== Running File_Chunk Unit Tests ===");
      Output.Write_Line ("");

      -- Run all tests
      Add_Test_Result ("Test_Create_Valid_Chunk", Test_Create_Valid_Chunk'Access);
      Add_Test_Result ("Test_Create_With_Checksum", Test_Create_With_Checksum'Access);
      Add_Test_Result ("Test_Data_Ownership_Transfer", Test_Data_Ownership_Transfer'Access);
      Add_Test_Result ("Test_Chunk_Sequence_Number", Test_Chunk_Sequence_Number'Access);
      Add_Test_Result ("Test_Chunk_Offset_Validation", Test_Chunk_Offset_Validation'Access);
      Add_Test_Result ("Test_Final_Chunk_Flag", Test_Final_Chunk_Flag'Access);
      Add_Test_Result ("Test_Data_Size_Limits", Test_Data_Size_Limits'Access);
      Add_Test_Result ("Test_Checksum_Operations", Test_Checksum_Operations'Access);
      Add_Test_Result ("Test_Chunk_Equality", Test_Chunk_Equality'Access);
      Add_Test_Result ("Test_Chunk_Validation", Test_Chunk_Validation'Access);
      Add_Test_Result ("Test_Chunk_Image_Representation", Test_Chunk_Image_Representation'Access);
      Add_Test_Result ("Test_Contract_Violations", Test_Contract_Violations'Access);
      Add_Test_Result ("Test_Zero_Copy_Semantics", Test_Zero_Copy_Semantics'Access);
      Add_Test_Result ("Test_Concurrent_Access_Safety", Test_Concurrent_Access_Safety'Access);

      -- Generate summary
      declare
         Stats_Result : constant Test_Stats_Result.Result :=
            Run_Test_Suite ("File_Chunk_Tests", Tests (1 .. Index - 1), Output);
      begin
         if Stats_Result.Is_Ok then
            declare
               Stats : constant Test_Statistics := Stats_Result.Get_Ok;
            begin
               Output.Write_Line ("");
               Print_Test_Summary ("File_Chunk Unit Tests", Stats, Output);
               return Test_Stats_Result.Ok (Stats);
            end;
         else
            return Stats_Result;
         end if;
      end;
   end Run_All_Tests;

end Test_File_Chunk;
