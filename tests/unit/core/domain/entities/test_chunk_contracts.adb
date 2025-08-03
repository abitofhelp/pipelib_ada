--  =============================================================================
--  Test_Chunk_Contracts - Contract Validation Tests Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams; use Ada.Streams;
with Pipelib.Core.Domain.Entities.Chunk; use Pipelib.Core.Domain.Entities.Chunk;
with Pipelib.Core.Domain.Value_Objects.Chunk_Size;
use Pipelib.Core.Domain.Value_Objects.Chunk_Size;

package body Test_Chunk_Contracts is

   --  Test valid state transitions according to postcondition
   function Test_Valid_State_Transitions return Void_Result.Result is
   begin
      -- Test Created -> Reading (valid)
      if not Is_Valid_Transition (Created, Reading) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Created -> Reading should be valid"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Valid_State_Transitions")
         ));
      end if;

      -- Test Created -> Read (valid)
      if not Is_Valid_Transition (Created, Read) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Created -> Read should be valid"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Valid_State_Transitions")
         ));
      end if;

      -- Test Reading -> Created (valid - retry)
      if not Is_Valid_Transition (Reading, Created) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Reading -> Created should be valid (retry)"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Valid_State_Transitions")
         ));
      end if;

      -- Test Processing -> Read (valid - retry)
      if not Is_Valid_Transition (Processing, Read) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Processing -> Read should be valid (retry)"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Valid_State_Transitions")
         ));
      end if;

      -- Test Writing -> Processed (valid - retry)
      if not Is_Valid_Transition (Writing, Processed) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Writing -> Processed should be valid (retry)"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Valid_State_Transitions")
         ));
      end if;

      -- Test Written -> anything (invalid - terminal state)
      if Is_Valid_Transition (Written, Created) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Written -> Created should be invalid (terminal)"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Valid_State_Transitions")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Valid_State_Transitions;

   --  Test invalid state transitions
   function Test_Invalid_State_Transitions return Void_Result.Result is
   begin
      -- Test Created -> Processing (invalid - must read first)
      if Is_Valid_Transition (Created, Processing) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Created -> Processing should be invalid"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Invalid_State_Transitions")
         ));
      end if;

      -- Test Reading -> Processing (invalid)
      if Is_Valid_Transition (Reading, Processing) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Reading -> Processing should be invalid"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Invalid_State_Transitions")
         ));
      end if;

      -- Test Processed -> Read (invalid)
      if Is_Valid_Transition (Processed, Read) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Processed -> Read should be invalid"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Invalid_State_Transitions")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Invalid_State_Transitions;

   --  Test Set_State precondition enforcement
   function Test_Set_State_Precondition return Void_Result.Result is
   begin
      declare
         Chunk : Chunk_Type := Create (1, Create (4096));
         Contract_Violated : Boolean := False;
      begin
         -- Try invalid transition Created -> Processing
         begin
            Set_State (Chunk, Processing);
            -- If we get here, precondition was not enforced
            Contract_Violated := False;
         exception
            when others =>
               -- Expected - precondition should be violated
               Contract_Violated := True;
         end;

         if not Contract_Violated then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Set_State precondition not enforced"),
               Details     => To_Unbounded_String ("Invalid transition should raise exception"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Set_State_Precondition")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Set_State_Precondition;

   --  Test Is_Valid postcondition consistency
   function Test_Is_Valid_Postcondition return Void_Result.Result is
   begin
      declare
         Chunk : Chunk_Type := Create (1, Create (4096));
         Data : Stream_Element_Array_Access := new Stream_Element_Array (1 .. 100);
      begin
         -- Fill with test data
         for I in Data'Range loop
            Data (I) := Stream_Element (I mod 256);
         end loop;

         -- Set data
         Set_Data (Chunk, Data);
         Set_Data_Size (Chunk, 100);

         -- Check Is_Valid postcondition components
         if not Is_Valid (Chunk) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Chunk should be valid after setting data"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Is_Valid_Postcondition")
            ));
         end if;

         -- Test compression consistency
         Set_Compressed (Chunk, True);
         Set_Original_Size (Chunk, 200); -- Original was larger

         if not Is_Valid (Chunk) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Chunk should be valid with compression info"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Is_Valid_Postcondition")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Is_Valid_Postcondition;

   --  Test Create postconditions
   function Test_Create_Postconditions return Void_Result.Result is
   begin
      declare
         Number : constant Natural := 42;
         Size : constant Chunk_Size_Type := Create (8192);
         Chunk : constant Chunk_Type := Create (Number, Size);
      begin
         -- Verify all postconditions
         if Chunk.Number /= Number then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Create postcondition: Number mismatch"),
               Details     => To_Unbounded_String ("Expected: 42, Got: " & Chunk.Number'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Create_Postconditions")
            ));
         end if;

         if State (Chunk) /= Created then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Create postcondition: State should be Created"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Create_Postconditions")
            ));
         end if;

         if Data (Chunk) /= null then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Create postcondition: Data should be null"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Create_Postconditions")
            ));
         end if;

         if Data_Size (Chunk) /= 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Create postcondition: Data_Size should be 0"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Create_Postconditions")
            ));
         end if;

         if Is_Compressed (Chunk) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Create postcondition: Should not be compressed"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Create_Postconditions")
            ));
         end if;

         if Retry_Count (Chunk) /= 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Create postcondition: Retry_Count should be 0"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Create_Postconditions")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Create_Postconditions;

   --  Test Set_Data ownership transfer postcondition
   function Test_Set_Data_Ownership_Transfer return Void_Result.Result is
   begin
      declare
         Chunk : Chunk_Type := Create (1, Create (4096));
         Data : Stream_Element_Array_Access := new Stream_Element_Array (1 .. 100);
      begin
         -- Set data (transfers ownership)
         Set_Data (Chunk, Data);

         -- Verify postcondition: Data should be null after transfer
         if Data /= null then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Set_Data postcondition: ownership not transferred"),
               Details     => To_Unbounded_String ("Data should be null after Set_Data"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Set_Data_Ownership_Transfer")
            ));
         end if;

         -- Verify chunk now has the data
         if Chunk.Data = null then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Chunk should have the transferred data"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Set_Data_Ownership_Transfer")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Set_Data_Ownership_Transfer;

   --  Test Increment_Retry_Count pre/postconditions
   function Test_Increment_Retry_Count_Contracts return Void_Result.Result is
   begin
      declare
         Chunk : Chunk_Type := Create (1, Create (4096));
         Old_Count : constant Natural := Retry_Count (Chunk);
      begin
         -- Increment retry count
         Increment_Retry_Count (Chunk);

         -- Verify postcondition
         if Retry_Count (Chunk) /= Old_Count + 1 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Increment_Retry_Count postcondition failed"),
               Details     => To_Unbounded_String ("Count not incremented by 1"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Increment_Retry_Count_Contracts")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Increment_Retry_Count_Contracts;

   --  Test Reset postconditions
   function Test_Reset_Postconditions return Void_Result.Result is
   begin
      declare
         Chunk : Chunk_Type := Create (1, Create (4096));
         Data_Array : Stream_Element_Array_Access := new Stream_Element_Array (1 .. 100);
      begin
         -- Set up chunk with data and state
         Set_Data (Chunk, Data_Array);
         Set_State (Chunk, Reading);
         Set_State (Chunk, Read);
         Increment_Retry_Count (Chunk);

         -- Reset the chunk
         Reset (Chunk);

         -- Verify all postconditions
         if State (Chunk) /= Created then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Reset postcondition: State should be Created"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Reset_Postconditions")
            ));
         end if;

         if Data (Chunk) /= null then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Reset postcondition: Data should be null"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Reset_Postconditions")
            ));
         end if;

         if Data_Size (Chunk) /= 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Reset postcondition: Data_Size should be 0"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Reset_Postconditions")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Reset_Postconditions;

   --  Test suite runner
   function Run_All_Tests (Output : access Test_Output_Port'Class)
                          return Test_Stats_Result.Result is
      Pass_Count : Natural := 0;
      Fail_Count : Natural := 0;
      Total_Count : constant Natural := 8;

      procedure Run_Contract_Test (Name : String; Test_Func : access function return Void_Result.Result) is
         Result : constant Void_Result.Result := Test_Func.all;
      begin
         if Result.Is_Ok then
            Output.Write_Line ("PASS: " & Name);
            Pass_Count := Pass_Count + 1;
         else
            Output.Write_Line ("FAIL: " & Name & " - " & To_String (Result.Get_Err.Message));
            Fail_Count := Fail_Count + 1;
         end if;
      end Run_Contract_Test;

   begin
      Output.Write_Line ("=== Running Chunk Contract Tests ===");
      Output.Write_Line ("");

      -- Run all tests manually
      Run_Contract_Test ("Valid State Transitions", Test_Valid_State_Transitions'Access);
      Run_Contract_Test ("Invalid State Transitions", Test_Invalid_State_Transitions'Access);
      Run_Contract_Test ("Set_State Precondition", Test_Set_State_Precondition'Access);
      Run_Contract_Test ("Is_Valid Postcondition", Test_Is_Valid_Postcondition'Access);
      Run_Contract_Test ("Create Postconditions", Test_Create_Postconditions'Access);
      Run_Contract_Test ("Set_Data Ownership Transfer", Test_Set_Data_Ownership_Transfer'Access);
      Run_Contract_Test ("Increment_Retry_Count Contracts", Test_Increment_Retry_Count_Contracts'Access);
      Run_Contract_Test ("Reset Postconditions", Test_Reset_Postconditions'Access);

      -- Generate summary
      Output.Write_Line ("");
      Output.Write_Line ("=== Test Summary ===");
      Output.Write_Line ("Total: " & Total_Count'Image & " tests");
      Output.Write_Line ("Passed: " & Pass_Count'Image);
      Output.Write_Line ("Failed: " & Fail_Count'Image);

      -- Return basic statistics
      declare
         Stats : constant Test_Statistics := (
            Total_Tests => Total_Count,
            Passed_Tests => Pass_Count,
            Failed_Tests => Fail_Count,
            Error_Tests => 0,
            Skipped_Tests => 0,
            Total_Duration => 0.0
         );
      begin
         return Test_Stats_Result.Ok (Stats);
      end;
   end Run_All_Tests;

end Test_Chunk_Contracts;
