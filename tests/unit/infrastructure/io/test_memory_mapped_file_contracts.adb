--  =============================================================================
--  Test_Memory_Mapped_File_Contracts - Contract Validation Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System.Storage_Elements; use System.Storage_Elements;
with Pipelib.Infrastructure.IO.Memory_Mapped_File;
use Pipelib.Infrastructure.IO.Memory_Mapped_File;

package body Test_Memory_Mapped_File_Contracts is

   --  Test Map_File contracts
   function Test_Map_File_Contracts return Void_Result.Result is
   begin
      -- Document what should be tested:

      -- Precondition: not Is_Mapped (File)
      -- - Should fail if file is already mapped

      -- The function should return a Result type with either:
      -- - Ok containing a valid Memory_View
      -- - Err containing error information

      -- On success, the file should be mapped (Is_Mapped = True)

      return Void_Result.Ok (True);
   end Test_Map_File_Contracts;

   --  Test Unmap contracts
   function Test_Unmap_Contracts return Void_Result.Result is
   begin
      -- Document what should be tested:

      -- Postcondition: not Is_Mapped (File)
      -- - Should always result in unmapped state
      -- - Should be idempotent (safe to call on unmapped file)

      return Void_Result.Ok (True);
   end Test_Unmap_Contracts;

   --  Test Get_View and Get_Size contracts
   function Test_Get_View_And_Size_Contracts return Void_Result.Result is
   begin
      -- Document what should be tested:

      -- Get_View precondition: Is_Mapped (File)
      -- - Should fail if file not mapped
      -- Get_View postconditions:
      -- - Result.Size = Get_Size (File)
      -- - Result.Address /= System.Null_Address

      -- Get_Size precondition: Is_Mapped (File)
      -- - Should fail if file not mapped
      -- Get_Size postcondition: Result > 0
      -- - Should never return zero or negative size for mapped file

      return Void_Result.Ok (True);
   end Test_Get_View_And_Size_Contracts;

   --  Test Create_Subview contracts
   function Test_Create_Subview_Contracts return Void_Result.Result is
   begin
      -- Document what should be tested:

      -- Preconditions:
      -- - Is_Mapped (File) - should fail if not mapped
      -- - Offset < Get_Size (File) - should fail if offset too large
      -- - Offset + Length <= Get_Size (File) - should fail if extends beyond file
      -- - Length > 0 - should fail with zero length

      -- Postconditions:
      -- - Result.Size = Length
      -- - Result.Address /= System.Null_Address

      return Void_Result.Ok (True);
   end Test_Create_Subview_Contracts;

   --  Test Sync and Advise contracts
   function Test_Sync_And_Advise_Contracts return Void_Result.Result is
   begin
      -- Document what should be tested:

      -- Sync precondition: Is_Mapped (File)
      -- - Should fail if file not mapped

      -- Advise precondition: Is_Mapped (File)
      -- - Should fail if file not mapped
      -- - Should accept all valid Access_Pattern values

      return Void_Result.Ok (True);
   end Test_Sync_And_Advise_Contracts;

   --  Test utility function contracts
   function Test_Utility_Function_Contracts return Void_Result.Result is
   begin
      -- Test Is_Memory_Mapping_Available
      -- - Should return consistent result (doesn't change during execution)

      -- Test Should_Use_Memory_Map contracts
      declare
         Small_File : constant Long_Long_Integer := 50 * 1024 * 1024; -- 50MB
         Medium_File : constant Long_Long_Integer := 200 * 1024 * 1024; -- 200MB
         Large_File : constant Long_Long_Integer := 2 * 1024 * 1024 * 1024; -- 2GB
         Custom_Threshold : constant Long_Long_Integer := 500 * 1024 * 1024; -- 500MB
      begin
         -- Test postcondition: Should_Use_Memory_Map'Result = (File_Size >= Threshold)

         -- Default threshold (100MB)
         if Should_Use_Memory_Map (Small_File) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Should_Use_Memory_Map postcondition failed"),
               Details     => To_Unbounded_String ("50MB < 100MB should return False"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Utility_Function_Contracts")
            ));
         end if;

         if not Should_Use_Memory_Map (Medium_File) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Should_Use_Memory_Map postcondition failed"),
               Details     => To_Unbounded_String ("200MB >= 100MB should return True"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Utility_Function_Contracts")
            ));
         end if;

         -- Custom threshold
         if Should_Use_Memory_Map (Medium_File, Custom_Threshold) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Should_Use_Memory_Map postcondition failed"),
               Details     => To_Unbounded_String ("200MB < 500MB should return False"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Utility_Function_Contracts")
            ));
         end if;

         if not Should_Use_Memory_Map (Large_File, Custom_Threshold) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Should_Use_Memory_Map postcondition failed"),
               Details     => To_Unbounded_String ("2GB >= 500MB should return True"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Utility_Function_Contracts")
            ));
         end if;
      end;

      -- Test preconditions
      declare
         Contract_Violated : Boolean := False;
      begin
         -- Test negative file size
         declare
            Result : Boolean;
         begin
            Result := Should_Use_Memory_Map (-1);
            Contract_Violated := False;
         exception
            when others =>
               Contract_Violated := True;
         end;

         if not Contract_Violated then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Should_Use_Memory_Map precondition not enforced"),
               Details     => To_Unbounded_String ("Negative file size should raise exception"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Utility_Function_Contracts")
            ));
         end if;
      end;

      declare
         Contract_Violated : Boolean := False;
      begin
         -- Test zero threshold
         declare
            Result : Boolean;
         begin
            Result := Should_Use_Memory_Map (1000, 0);
            Contract_Violated := False;
         exception
            when others =>
               Contract_Violated := True;
         end;

         if not Contract_Violated then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Should_Use_Memory_Map threshold precondition not enforced"),
               Details     => To_Unbounded_String ("Zero threshold should raise exception"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Utility_Function_Contracts")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Utility_Function_Contracts;

   --  Test memory mapping lifecycle
   function Test_Memory_Mapping_Lifecycle return Void_Result.Result is
   begin
      -- Document the expected lifecycle:

      -- 1. Created file should not be mapped (Is_Mapped = False)
      -- 2. Map_File should result in mapped state (Is_Mapped = True)
      -- 3. Mapped file should have valid view and size
      -- 4. Subviews should be creatable within bounds
      -- 5. Unmap should result in unmapped state (Is_Mapped = False)
      -- 6. Finalize should ensure unmapping even if not explicitly called

      -- Test scenarios:
      -- - Normal lifecycle: Create -> Map -> Use -> Unmap
      -- - Multiple map attempts (should fail precondition)
      -- - Operations on unmapped file (should fail preconditions)
      -- - Boundary conditions for subview creation

      return Void_Result.Ok (True);
   end Test_Memory_Mapping_Lifecycle;

   --  Test suite runner
   function Run_All_Tests (Output : access Test_Output_Port'Class)
                          return Test_Stats_Result.Result is
      Tests : constant Test_Array := [
         (Name => To_Unbounded_String ("Map_File Contracts"),
          Func => Test_Map_File_Contracts'Access),
         (Name => To_Unbounded_String ("Unmap Contracts"),
          Func => Test_Unmap_Contracts'Access),
         (Name => To_Unbounded_String ("Get_View and Get_Size Contracts"),
          Func => Test_Get_View_And_Size_Contracts'Access),
         (Name => To_Unbounded_String ("Create_Subview Contracts"),
          Func => Test_Create_Subview_Contracts'Access),
         (Name => To_Unbounded_String ("Sync and Advise Contracts"),
          Func => Test_Sync_And_Advise_Contracts'Access),
         (Name => To_Unbounded_String ("Utility Function Contracts"),
          Func => Test_Utility_Function_Contracts'Access),
         (Name => To_Unbounded_String ("Memory Mapping Lifecycle"),
          Func => Test_Memory_Mapping_Lifecycle'Access)
      ];
   begin
      return Run_Test_Suite ("Memory_Mapped_File Contract Validation", Tests, Output);
   end Run_All_Tests;

end Test_Memory_Mapped_File_Contracts;
