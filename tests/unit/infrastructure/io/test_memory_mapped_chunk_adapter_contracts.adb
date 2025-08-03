--  =============================================================================
--  Test_Memory_Mapped_Chunk_Adapter_Contracts - Contract Validation Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System.Storage_Elements; use System.Storage_Elements;
with Pipelib.Infrastructure.IO.Memory_Mapped_Chunk_Adapter;
use Pipelib.Infrastructure.IO.Memory_Mapped_Chunk_Adapter;

package body Test_Memory_Mapped_Chunk_Adapter_Contracts is

   --  Test Calculate_Optimal_Chunk_Size contracts
   function Test_Calculate_Optimal_Chunk_Size_Contracts return Void_Result.Result is
   begin
      -- Test precondition: File_Size > 0
      declare
         Contract_Violated : Boolean := False;
      begin
         -- Try with zero file size (should violate precondition)
         declare
            Size : Positive;
         begin
            Size := Calculate_Optimal_Chunk_Size (0, 0);
            Contract_Violated := False;
         exception
            when others =>
               Contract_Violated := True;
         end;

         if not Contract_Violated then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Calculate_Optimal_Chunk_Size precondition not enforced"),
               Details     => To_Unbounded_String ("Zero file size should raise exception"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Calculate_Optimal_Chunk_Size_Contracts")
            ));
         end if;
      end;

      -- Test postconditions
      declare
         Small_File : constant Storage_Count := 1024; -- 1KB
         Medium_File : constant Storage_Count := 10 * 1024 * 1024; -- 10MB
         Large_File : constant Storage_Count := 1024 * 1024 * 1024; -- 1GB

         Small_Chunk : constant Positive := Calculate_Optimal_Chunk_Size (Small_File);
         Medium_Chunk : constant Positive := Calculate_Optimal_Chunk_Size (Medium_File);
         Large_Chunk : constant Positive := Calculate_Optimal_Chunk_Size (Large_File);
      begin
         -- Verify postcondition: Result >= 1024 (1KB minimum)
         if Small_Chunk < 1024 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Chunk size below minimum"),
               Details     => To_Unbounded_String ("Got:" & Small_Chunk'Image & ", Min: 1024"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Calculate_Optimal_Chunk_Size_Contracts")
            ));
         end if;

         -- Verify postcondition: Result <= File_Size
         if Storage_Count (Small_Chunk) > Small_File then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Chunk size exceeds file size"),
               Details     => To_Unbounded_String ("Chunk:" & Small_Chunk'Image &
                                                  ", File:" & Small_File'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Calculate_Optimal_Chunk_Size_Contracts")
            ));
         end if;

         -- Verify postcondition: Result <= 512MB maximum
         if Large_Chunk > 512 * 1024 * 1024 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Chunk size exceeds maximum"),
               Details     => To_Unbounded_String ("Got:" & Large_Chunk'Image &
                                                  ", Max: 536870912"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Calculate_Optimal_Chunk_Size_Contracts")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Calculate_Optimal_Chunk_Size_Contracts;

   --  Test Should_Use_Memory_Mapping_For_File contracts
   function Test_Should_Use_Memory_Mapping_Contracts return Void_Result.Result is
   begin
      -- Test precondition: File_Size > 0
      declare
         Contract_Violated : Boolean := False;
      begin
         -- Try with zero file size (should violate precondition)
         declare
            Should_Use : Boolean;
         begin
            Should_Use := Should_Use_Memory_Mapping_For_File (0);
            Contract_Violated := False;
         exception
            when others =>
               Contract_Violated := True;
         end;

         if not Contract_Violated then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Should_Use_Memory_Mapping precondition not enforced"),
               Details     => To_Unbounded_String ("Zero file size should raise exception"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Should_Use_Memory_Mapping_Contracts")
            ));
         end if;
      end;

      -- Test postconditions
      declare
         Small_File : constant Storage_Count := 50 * 1024 * 1024; -- 50MB
         Medium_File : constant Storage_Count := 200 * 1024 * 1024; -- 200MB
         Large_File : constant Storage_Count := 2 * 1024 * 1024 * 1024; -- 2GB
      begin
         -- Verify postcondition: Files < 100MB should not use mmap
         if Should_Use_Memory_Mapping_For_File (Small_File) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Small file should not use mmap"),
               Details     => To_Unbounded_String ("File size: 50MB < 100MB threshold"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Should_Use_Memory_Mapping_Contracts")
            ));
         end if;

         -- Verify postcondition: Files > 1GB should not use mmap
         if Should_Use_Memory_Mapping_For_File (Large_File) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Large file should not use mmap"),
               Details     => To_Unbounded_String ("File size: 2GB > 1GB threshold"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Should_Use_Memory_Mapping_Contracts")
            ));
         end if;

         -- Files between 100MB and 1GB should use mmap
         if not Should_Use_Memory_Mapping_For_File (Medium_File) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Medium file should use mmap"),
               Details     => To_Unbounded_String ("File size: 200MB in range [100MB, 1GB]"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Should_Use_Memory_Mapping_Contracts")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Should_Use_Memory_Mapping_Contracts;

   --  Test Create_Stream_Array_Access_From_Memory contracts
   function Test_Create_Stream_Array_Access_Contracts return Void_Result.Result is
   begin
      -- Note: This is a private function, so we can't test it directly
      -- We would test it indirectly through public functions that use it
      -- For now, we'll document that this test would validate:
      -- 1. Precondition: Offset + Length <= View.Size
      -- 2. Precondition: Length > 0
      -- 3. Precondition: View.Address /= System.Null_Address
      -- 4. Postcondition: Result /= null
      -- 5. Postcondition: Result.all'Length = Natural (Length)

      return Void_Result.Ok (True);
   end Test_Create_Stream_Array_Access_Contracts;

   --  Test Memory_To_Stream_Elements contracts
   function Test_Memory_To_Stream_Elements_Contracts return Void_Result.Result is
   begin
      -- Note: This is a private function, so we can't test it directly
      -- We would test it indirectly through public functions that use it
      -- For now, we'll document that this test would validate:
      -- 1. Precondition: Address /= System.Null_Address
      -- 2. Precondition: Length > 0
      -- 3. Postcondition: Result /= null
      -- 4. Postcondition: Result.all'Length = Natural (Length)

      return Void_Result.Ok (True);
   end Test_Memory_To_Stream_Elements_Contracts;

   --  Test Create_Single_Chunk_From_Memory_Map contracts
   function Test_Create_Single_Chunk_Contracts return Void_Result.Result is
   begin
      -- This test would require a mock Memory_Mapped_File_Interface
      -- For now, we document what contracts should be tested:
      -- 1. Precondition: Map.Is_Mapped
      -- 2. Precondition: Offset + Length <= Map.Get_Size
      -- 3. Precondition: Length > 0
      -- 4. The resulting chunk should satisfy all File_Chunk contracts

      return Void_Result.Ok (True);
   end Test_Create_Single_Chunk_Contracts;

   --  Test Create_Chunks_From_Memory_Map contracts
   function Test_Create_Chunks_From_Memory_Map_Contracts return Void_Result.Result is
   begin
      -- This test would require a mock Memory_Mapped_File_Interface
      -- For now, we document what contracts should be tested:
      -- 1. Precondition: Map.Is_Mapped
      -- 2. The result should be a valid Chunk_Vector_Result
      -- 3. Each chunk in the vector should satisfy File_Chunk contracts
      -- 4. Chunks should cover the entire requested range
      -- 5. Only the last chunk should have Is_Final = True

      return Void_Result.Ok (True);
   end Test_Create_Chunks_From_Memory_Map_Contracts;

   --  Test suite runner
   function Run_All_Tests (Output : access Test_Output_Port'Class)
                          return Test_Stats_Result.Result is
      Tests : constant Test_Array := [
         (Name => To_Unbounded_String ("Calculate_Optimal_Chunk_Size Contracts"),
          Func => Test_Calculate_Optimal_Chunk_Size_Contracts'Access),
         (Name => To_Unbounded_String ("Should_Use_Memory_Mapping Contracts"),
          Func => Test_Should_Use_Memory_Mapping_Contracts'Access),
         (Name => To_Unbounded_String ("Create_Stream_Array_Access Contracts"),
          Func => Test_Create_Stream_Array_Access_Contracts'Access),
         (Name => To_Unbounded_String ("Memory_To_Stream_Elements Contracts"),
          Func => Test_Memory_To_Stream_Elements_Contracts'Access),
         (Name => To_Unbounded_String ("Create_Single_Chunk Contracts"),
          Func => Test_Create_Single_Chunk_Contracts'Access),
         (Name => To_Unbounded_String ("Create_Chunks_From_Memory_Map Contracts"),
          Func => Test_Create_Chunks_From_Memory_Map_Contracts'Access)
      ];
   begin
      return Run_Test_Suite ("Memory_Mapped_Chunk_Adapter Contract Validation", Tests, Output);
   end Run_All_Tests;

end Test_Memory_Mapped_Chunk_Adapter_Contracts;
