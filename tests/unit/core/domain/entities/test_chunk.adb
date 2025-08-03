--  =============================================================================
--  Test_Chunk - Chunk Entity Unit Tests Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams; use Ada.Streams;
with System; use System;
with Pipelib.Core.Domain.Entities.Chunk; use Pipelib.Core.Domain.Entities.Chunk;
with Pipelib.Core.Domain.Value_Objects.Chunk_Size; use Pipelib.Core.Domain.Value_Objects.Chunk_Size;

package body Test_Chunk is

   --  ==========================================================================
   --  Test Implementation
   --  ==========================================================================

   function Test_Create_Chunk return Void_Result.Result is
      Chunk_Size : constant Chunk_Size_Type := From_KB (64);
      Chunk : Chunk_Type;
   begin
      -- Test chunk creation with contract verification
      Chunk := Create (Number => 42, Size => Chunk_Size);

      -- Verify postconditions are satisfied
      pragma Assert (Number (Chunk) = 42, "Postcondition: Number must match");
      pragma Assert (State (Chunk) = Created, "Postcondition: Initial state must be Created");
      pragma Assert (Is_Valid (Chunk), "Postcondition: Chunk must be valid");
      pragma Assert (Data (Chunk) = null, "Postcondition: Data initially null");
      pragma Assert (Data_Size (Chunk) = 0, "Postcondition: Data size initially 0");

      if Number (Chunk) /= 42 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Chunk number mismatch"),
            Details     => To_Unbounded_String ("Expected: 42, Got: " & Number (Chunk)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_Chunk")
         ));
      end if;

      if State (Chunk) /= Created then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Initial state incorrect"),
            Details     => To_Unbounded_String ("Expected: Created, Got: " & State (Chunk)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_Chunk")
         ));
      end if;

      if not Is_Valid (Chunk) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Newly created chunk should be valid"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_Chunk")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Create_Chunk;

   function Test_Identity_Immutability return Void_Result.Result is
      Chunk_Size : constant Chunk_Size_Type := From_MB (1);
      Chunk1 : Chunk_Type := Create (Number => 100, Size => Chunk_Size);
      Chunk2 : constant Chunk_Type := Create (Number => 200, Size => Chunk_Size);
   begin
      -- Test that entity identity (number) is immutable
      pragma Assert (Number (Chunk1) = 100, "Identity must be preserved");
      pragma Assert (Number (Chunk2) = 200, "Identity must be preserved");

      -- Verify identity remains constant through state changes
      Set_State (Chunk1, Reading);
      Set_State (Chunk1, Read);
      Set_State (Chunk1, Processing);

      if Number (Chunk1) /= 100 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Identity changed during state transitions"),
            Details     => To_Unbounded_String ("Expected: 100, Got: " & Number (Chunk1)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Identity_Immutability")
         ));
      end if;

      -- Verify different chunks have different identities
      if Number (Chunk1) = Number (Chunk2) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Different chunks should have different identities"),
            Details     => To_Unbounded_String ("Both have number: " & Number (Chunk1)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Identity_Immutability")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Identity_Immutability;

   function Test_State_Machine_Transitions return Void_Result.Result is
      Chunk_Size : constant Chunk_Size_Type := Default;
      Chunk : Chunk_Type := Create (Number => 1, Size => Chunk_Size);
   begin
      -- Test valid state transitions according to the state machine

      -- Created -> Reading
      pragma Assert (Is_Valid_Transition (State (Chunk), Reading),
                    "Precondition: Created -> Reading should be valid");
      Set_State (Chunk, Reading);
      pragma Assert (State (Chunk) = Reading, "Postcondition: State should be Reading");

      if State (Chunk) /= Reading then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("State transition Created -> Reading failed"),
            Details     => To_Unbounded_String ("Expected: Reading, Got: " & State (Chunk)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_State_Machine_Transitions")
         ));
      end if;

      -- Reading -> Read
      pragma Assert (Is_Valid_Transition (State (Chunk), Read));
      Set_State (Chunk, Read);
      pragma Assert (State (Chunk) = Read);

      -- Read -> Processing
      pragma Assert (Is_Valid_Transition (State (Chunk), Processing));
      Set_State (Chunk, Processing);
      pragma Assert (State (Chunk) = Processing);

      -- Processing -> Processed
      pragma Assert (Is_Valid_Transition (State (Chunk), Processed));
      Set_State (Chunk, Processed);
      pragma Assert (State (Chunk) = Processed);

      -- Processed -> Writing
      pragma Assert (Is_Valid_Transition (State (Chunk), Writing));
      Set_State (Chunk, Writing);
      pragma Assert (State (Chunk) = Writing);

      -- Writing -> Written
      pragma Assert (Is_Valid_Transition (State (Chunk), Written));
      Set_State (Chunk, Written);
      pragma Assert (State (Chunk) = Written);

      if State (Chunk) /= Written then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Final state should be Written"),
            Details     => To_Unbounded_String ("Got: " & State (Chunk)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_State_Machine_Transitions")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_State_Machine_Transitions;

   function Test_Invalid_State_Transitions return Void_Result.Result is
      Chunk_Size : constant Chunk_Size_Type := Default;
      Chunk : Chunk_Type := Create (Number => 1, Size => Chunk_Size);
   begin
      -- Test that invalid transitions are rejected by preconditions

      -- Created -> Processed (should be invalid)
      if Is_Valid_Transition (Created, Processed) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Created -> Processed should be invalid"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Invalid_State_Transitions")
         ));
      end if;

      -- Reading -> Writing (should be invalid)
      if Is_Valid_Transition (Reading, Writing) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Reading -> Writing should be invalid"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Invalid_State_Transitions")
         ));
      end if;

      -- Written is a terminal state - no transitions allowed
      -- Chunk reuse is handled by the Reset procedure, not state transition
      if Is_Valid_Transition (Written, Created) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Written -> Created should be invalid (use Reset for reuse)"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Invalid_State_Transitions")
         ));
      end if;

      -- Test contract violation attempt
      begin
         Set_State (Chunk, Processed); -- Invalid from Created
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Expected precondition violation"),
            Details     => To_Unbounded_String ("Set_State should have failed"),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Invalid_State_Transitions")
         ));
      exception
         when others =>
            null; -- Expected - precondition violation
      end;

      return Void_Result.Ok (True);
   end Test_Invalid_State_Transitions;

   function Test_Data_Management return Void_Result.Result is
      Chunk_Size : constant Chunk_Size_Type := From_KB (4);
      Chunk : Chunk_Type := Create (Number => 1, Size => Chunk_Size);
      Test_Data : Stream_Element_Array_Access;
   begin
      -- Test data size management
      Set_Data_Size (Chunk, 1024);
      pragma Assert (Data_Size (Chunk) = 1024, "Postcondition: Data size should be set");

      if Data_Size (Chunk) /= 1024 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Data size not set correctly"),
            Details     => To_Unbounded_String ("Expected: 1024, Got: " & Data_Size (Chunk)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Data_Management")
         ));
      end if;

      -- Create test data
      Test_Data := new Stream_Element_Array (1 .. 1024);
      for I in Test_Data'Range loop
         Test_Data (I) := Stream_Element (Natural (I) mod 256);
      end loop;

      -- Test data assignment (should transfer ownership)
      pragma Assert (Test_Data /= null, "Precondition: Test data should not be null");
      Set_Data (Chunk, Test_Data);
      pragma Assert (Test_Data = null, "Postcondition: Ownership transferred");
      pragma Assert (Data (Chunk) /= null, "Postcondition: Chunk should have data");

      if Test_Data /= null then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Data ownership not transferred"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Data_Management")
         ));
      end if;

      if Data (Chunk) = null then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Chunk should have received data"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Data_Management")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Data_Management;

   function Test_Zero_Copy_Operations return Void_Result.Result is
      Chunk_Size : constant Chunk_Size_Type := From_KB (8);
      Chunk : Chunk_Type := Create (Number => 1, Size => Chunk_Size);
      Original_Data : Stream_Element_Array_Access;
      Retrieved_Data : Stream_Element_Array_Access;
   begin
      -- Create test data
      Original_Data := new Stream_Element_Array (1 .. 2048);
      for I in Original_Data'Range loop
         Original_Data (I) := Stream_Element (42);
      end loop;

      declare
         Original_Address : constant System.Address := Original_Data.all'Address;
      begin
         -- Set data (transfers ownership)
         Set_Data (Chunk, Original_Data);

         -- Retrieve data (should be zero-copy)
         Retrieved_Data := Data (Chunk);

         if Retrieved_Data = null then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Retrieved data should not be null"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Zero_Copy_Operations")
            ));
         end if;

         -- Verify zero-copy (same memory address)
         declare
            Retrieved_Address : constant System.Address := Retrieved_Data.all'Address;
         begin
            if Original_Address /= Retrieved_Address then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Data was copied, not zero-copy"),
                  Details     => Null_Unbounded_String,
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Zero_Copy_Operations")
               ));
            end if;
         end;

         -- Verify data integrity
         if Retrieved_Data (1) /= Stream_Element (42) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Data integrity lost"),
               Details     => To_Unbounded_String ("Expected: 42, Got: " &
                                                  Retrieved_Data (1)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Zero_Copy_Operations")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Zero_Copy_Operations;

   function Test_Compression_Info return Void_Result.Result is
      Chunk_Size : constant Chunk_Size_Type := Default;
      Chunk : Chunk_Type := Create (Number => 1, Size => Chunk_Size);
   begin
      -- Test compression flag management
      pragma Assert (not Is_Compressed (Chunk), "Initially not compressed");

      Set_Compressed (Chunk, True);
      pragma Assert (Is_Compressed (Chunk), "Postcondition: Should be compressed");

      if not Is_Compressed (Chunk) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Compression flag not set"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Compression_Info")
         ));
      end if;

      -- Test original size tracking
      Set_Original_Size (Chunk, 4096);
      pragma Assert (Original_Size (Chunk) = 4096, "Postcondition: Original size set");

      if Original_Size (Chunk) /= 4096 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Original size not set correctly"),
            Details     => To_Unbounded_String ("Expected: 4096, Got: " &
                                               Original_Size (Chunk)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Compression_Info")
         ));
      end if;

      -- Test decompression
      Set_Compressed (Chunk, False);
      pragma Assert (not Is_Compressed (Chunk), "Postcondition: Not compressed");

      if Is_Compressed (Chunk) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Compression flag not cleared"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Compression_Info")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Compression_Info;

   function Test_Processing_Metrics return Void_Result.Result is
      Chunk_Size : constant Chunk_Size_Type := Default;
      Chunk : Chunk_Type := Create (Number => 1, Size => Chunk_Size);
   begin
      -- Test retry count management
      pragma Assert (Retry_Count (Chunk) = 0, "Initially zero retries");

      -- Increment retry count multiple times
      Increment_Retry_Count (Chunk);
      pragma Assert (Retry_Count (Chunk) = 1, "Postcondition: Retry count incremented");

      if Retry_Count (Chunk) /= 1 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Retry count not incremented"),
            Details     => To_Unbounded_String ("Expected: 1, Got: " &
                                               Retry_Count (Chunk)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Processing_Metrics")
         ));
      end if;

      Increment_Retry_Count (Chunk);
      Increment_Retry_Count (Chunk);

      if Retry_Count (Chunk) /= 3 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Multiple retry increments failed"),
            Details     => To_Unbounded_String ("Expected: 3, Got: " &
                                               Retry_Count (Chunk)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Processing_Metrics")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Processing_Metrics;

   function Test_Chunk_Validation return Void_Result.Result is
      Chunk_Size : constant Chunk_Size_Type := Default;
      Valid_Chunk : Chunk_Type := Create (Number => 1, Size => Chunk_Size);
   begin
      -- Test that a properly created chunk is valid
      if not Is_Valid (Valid_Chunk) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Valid chunk reported as invalid"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Chunk_Validation")
         ));
      end if;

      -- Validation should remain true through state transitions
      Set_State (Valid_Chunk, Reading);
      if not Is_Valid (Valid_Chunk) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Chunk invalid after state change"),
            Details     => To_Unbounded_String ("State: " & State (Valid_Chunk)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Chunk_Validation")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Chunk_Validation;

   function Test_Chunk_Reset return Void_Result.Result is
      Chunk_Size : constant Chunk_Size_Type := Default;
      Chunk : Chunk_Type := Create (Number => 42, Size => Chunk_Size);
      Test_Data : Stream_Element_Array_Access;
   begin
      -- Setup chunk with some state
      Set_State (Chunk, Reading);
      Set_State (Chunk, Read);
      Set_Data_Size (Chunk, 1024);
      Set_Compressed (Chunk, True);
      Set_Original_Size (Chunk, 2048);
      Increment_Retry_Count (Chunk);

      Test_Data := new Stream_Element_Array (1 .. 1024);
      Set_Data (Chunk, Test_Data);

      -- Reset chunk
      Reset (Chunk);

      -- Verify postconditions
      pragma Assert (State (Chunk) = Created, "Postcondition: State reset to Created");
      pragma Assert (Data (Chunk) = null, "Postcondition: Data cleared");
      pragma Assert (Data_Size (Chunk) = 0, "Postcondition: Data size reset");

      if State (Chunk) /= Created then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("State not reset to Created"),
            Details     => To_Unbounded_String ("Got: " & State (Chunk)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Chunk_Reset")
         ));
      end if;

      if Data (Chunk) /= null then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Data not cleared after reset"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Chunk_Reset")
         ));
      end if;

      if Data_Size (Chunk) /= 0 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Data size not reset"),
            Details     => To_Unbounded_String ("Got: " & Data_Size (Chunk)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Chunk_Reset")
         ));
      end if;

      -- Identity should be preserved
      if Number (Chunk) /= 42 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Identity changed after reset"),
            Details     => To_Unbounded_String ("Expected: 42, Got: " & Number (Chunk)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Chunk_Reset")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Chunk_Reset;

   function Test_Contract_Violations return Void_Result.Result is
      Chunk_Size : constant Chunk_Size_Type := Default;
      Chunk : Chunk_Type := Create (Number => 1, Size => Chunk_Size);
   begin
      -- Test Set_Data with null pointer (should violate precondition)
      declare
         Null_Data : Stream_Element_Array_Access := null;
      begin
         Set_Data (Chunk, Null_Data);
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Expected precondition violation for null data"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Contract_Violations")
         ));
      exception
         when others =>
            null; -- Expected - precondition violation
      end;

      return Void_Result.Ok (True);
   end Test_Contract_Violations;

   function Test_Entity_Equality return Void_Result.Result is
      Chunk_Size : constant Chunk_Size_Type := Default;
      Chunk1 : constant Chunk_Type := Create (Number => 100, Size => Chunk_Size);
      Chunk2 : constant Chunk_Type := Create (Number => 100, Size => Chunk_Size);
      Chunk3 : constant Chunk_Type := Create (Number => 200, Size => Chunk_Size);
   begin
      -- Entities with same identity should be considered equal
      if Number (Chunk1) /= Number (Chunk2) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Chunks with same number should have same identity"),
            Details     => To_Unbounded_String ("Chunk1: " & Number (Chunk1)'Image &
                                               ", Chunk2: " & Number (Chunk2)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Entity_Equality")
         ));
      end if;

      -- Entities with different identity should not be equal
      if Number (Chunk1) = Number (Chunk3) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Chunks with different numbers should have different identity"),
            Details     => To_Unbounded_String ("Both have number: " & Number (Chunk1)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Entity_Equality")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Entity_Equality;

   function Test_State_Transition_Contract_Violations return Void_Result.Result is
      Chunk_Size : constant Chunk_Size_Type := From_KB (64);
   begin
      -- Test all invalid state transitions that should violate preconditions

      -- From Created state
      declare
         Chunk : Chunk_Type := Create (Number => 1, Size => Chunk_Size);
      begin
         -- Invalid: Created -> Processing (must go through Reading or Read)
         begin
            Set_State (Chunk, Processing);
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Invalid transition accepted"),
               Details     => To_Unbounded_String ("Created -> Processing should fail"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_State_Transition_Contract_Violations")
            ));
         exception
            when others => null; -- Expected
         end;
      end;

      -- From Processing state
      declare
         Chunk : Chunk_Type := Create (Number => 2, Size => Chunk_Size);
      begin
         -- Need valid transitions to get to Processing state
         Set_State (Chunk, Read);  -- Created -> Read is valid
         Set_State (Chunk, Processing);  -- Read -> Processing is valid

         -- Valid: Processing -> Read (retry path) or Processed
         -- Test an actually invalid transition: Processing -> Created
         begin
            Set_State (Chunk, Created);
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Invalid transition accepted"),
               Details     => To_Unbounded_String ("Processing -> Created should fail"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_State_Transition_Contract_Violations")
            ));
         exception
            when others => null; -- Expected
         end;
      end;

      -- From Written state
      declare
         Chunk : Chunk_Type := Create (Number => 3, Size => Chunk_Size);
      begin
         -- Need valid transitions to get to Written state
         Set_State (Chunk, Read);  -- Created -> Read is valid
         Set_State (Chunk, Processing);  -- Read -> Processing is valid
         Set_State (Chunk, Processed);  -- Processing -> Processed is valid
         Set_State (Chunk, Writing);  -- Processed -> Writing is valid
         Set_State (Chunk, Written);  -- Writing -> Written is valid

         -- Invalid: Written is terminal state - no transitions allowed
         begin
            Set_State (Chunk, Processing);
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Invalid transition accepted"),
               Details     => To_Unbounded_String ("Written -> Processing should fail"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_State_Transition_Contract_Violations")
            ));
         exception
            when others => null; -- Expected
         end;
      end;

      return Void_Result.Ok (True);
   end Test_State_Transition_Contract_Violations;

   function Test_State_Transition_Matrix return Void_Result.Result is
      All_States : constant array (1 .. 7) of Chunk_State :=
        [Created, Reading, Read, Processing, Processed, Writing, Written];
   begin
      -- Test complete state transition matrix
      for From_State of All_States loop
         for To_State of All_States loop
            declare
               Is_Valid : constant Boolean := Is_Valid_Transition (From_State, To_State);
               Expected_Valid : Boolean;
            begin
               -- Define expected valid transitions based on actual implementation
               case From_State is
                  when Created =>
                     Expected_Valid := To_State in Reading | Read;
                  when Reading =>
                     Expected_Valid := To_State in Read | Created;  -- Can retry
                  when Read =>
                     Expected_Valid := To_State in Processing | Writing;
                  when Processing =>
                     Expected_Valid := To_State in Processed | Read;  -- Can retry
                  when Processed =>
                     Expected_Valid := To_State = Writing;
                  when Writing =>
                     Expected_Valid := To_State in Written | Processed;  -- Can retry
                  when Written =>
                     Expected_Valid := False;  -- Terminal state - use Reset for reuse
               end case;

               if Is_Valid /= Expected_Valid then
                  return Void_Result.Err (Test_Error'(
                     Kind        => Assertion_Failed,
                     Message     => To_Unbounded_String ("State transition validation mismatch"),
                     Details     => To_Unbounded_String ("From: " & From_State'Image &
                                                        " To: " & To_State'Image &
                                                        " Expected: " & Expected_Valid'Image &
                                                        " Got: " & Is_Valid'Image),
                     Line_Number => 0,
                     Test_Name   => To_Unbounded_String ("Test_State_Transition_Matrix")
                  ));
               end if;
            end;
         end loop;
      end loop;

      return Void_Result.Ok (True);
   end Test_State_Transition_Matrix;

   function Test_Data_Ownership_Transfer return Void_Result.Result is
      Chunk_Size : constant Chunk_Size_Type := Default;
      Chunk : Chunk_Type := Create (Number => 1, Size => Chunk_Size);
      Test_Data1 : Stream_Element_Array_Access;
      Test_Data2 : Stream_Element_Array_Access;
   begin
      -- Create test data
      Test_Data1 := new Stream_Element_Array (1 .. 1024);
      Test_Data2 := new Stream_Element_Array (1 .. 2048);

      -- Transfer ownership of first data
      Set_Data (Chunk, Test_Data1);

      if Test_Data1 /= null then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("First data ownership not transferred"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Data_Ownership_Transfer")
         ));
      end if;

      -- Transfer ownership of second data (should replace first)
      Set_Data (Chunk, Test_Data2);

      if Test_Data2 /= null then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Second data ownership not transferred"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Data_Ownership_Transfer")
         ));
      end if;

      -- Verify chunk now has the second data
      if Data (Chunk) = null then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Chunk should have second data"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Data_Ownership_Transfer")
         ));
      end if;

      -- Verify data size matches second array
      if Data (Chunk)'Length /= 2048 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Data size should match second array"),
            Details     => To_Unbounded_String ("Expected: 2048, Got: " &
                                               Data (Chunk)'Length'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Data_Ownership_Transfer")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Data_Ownership_Transfer;

   function Test_Concurrent_Access_Safety return Void_Result.Result is
      Chunk_Size : constant Chunk_Size_Type := Default;
      Chunk : constant Chunk_Type := Create (Number => 1, Size => Chunk_Size);
   begin
      -- Test that basic operations are safe for concurrent read access
      -- Note: This is a basic test - full concurrency testing would require tasks

      -- Simulate concurrent reads
      for I in 1 .. 1000 loop
         declare
            Num : constant Natural := Number (Chunk);
            State_Val : constant Chunk_State := State (Chunk);
            Valid : constant Boolean := Is_Valid (Chunk);
            Data_Ptr : constant Stream_Element_Array_Access := Data (Chunk);
            Size : constant Natural := Data_Size (Chunk);
            Compressed : constant Boolean := Is_Compressed (Chunk);
            Orig_Size : constant Natural := Original_Size (Chunk);
            Retries : constant Natural := Retry_Count (Chunk);
            pragma Unreferenced (Num, State_Val, Valid, Data_Ptr, Size,
                               Compressed, Orig_Size, Retries);
         begin
            null; -- Just accessing the values
         end;
      end loop;

      -- Verify chunk is still valid after repeated access
      if not Is_Valid (Chunk) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Chunk invalid after concurrent access"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Concurrent_Access_Safety")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Concurrent_Access_Safety;

   --  ==========================================================================
   --  Test Suite Runner
   --  ==========================================================================

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Tests : Test_Results_Array (1 .. 16);
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
      Output.Write_Line ("=== Running Chunk Unit Tests ===");
      Output.Write_Line ("");

      -- Run all tests
      Add_Test_Result ("Test_Create_Chunk", Test_Create_Chunk'Access);
      Add_Test_Result ("Test_Identity_Immutability", Test_Identity_Immutability'Access);
      Add_Test_Result ("Test_State_Machine_Transitions", Test_State_Machine_Transitions'Access);
      Add_Test_Result ("Test_Invalid_State_Transitions", Test_Invalid_State_Transitions'Access);
      Add_Test_Result ("Test_Data_Management", Test_Data_Management'Access);
      Add_Test_Result ("Test_Zero_Copy_Operations", Test_Zero_Copy_Operations'Access);
      Add_Test_Result ("Test_Compression_Info", Test_Compression_Info'Access);
      Add_Test_Result ("Test_Processing_Metrics", Test_Processing_Metrics'Access);
      Add_Test_Result ("Test_Chunk_Validation", Test_Chunk_Validation'Access);
      Add_Test_Result ("Test_Chunk_Reset", Test_Chunk_Reset'Access);
      Add_Test_Result ("Test_Contract_Violations", Test_Contract_Violations'Access);
      Add_Test_Result ("Test_Entity_Equality", Test_Entity_Equality'Access);
      Add_Test_Result ("Test_State_Transition_Matrix", Test_State_Transition_Matrix'Access);
      Add_Test_Result ("Test_State_Transition_Contract_Violations", Test_State_Transition_Contract_Violations'Access);
      Add_Test_Result ("Test_Data_Ownership_Transfer", Test_Data_Ownership_Transfer'Access);
      Add_Test_Result ("Test_Concurrent_Access_Safety", Test_Concurrent_Access_Safety'Access);

      -- Generate summary
      declare
         Stats_Result : constant Test_Stats_Result.Result :=
            Run_Test_Suite ("Chunk_Entity_Tests", Tests (1 .. Index - 1), Output);
      begin
         if Stats_Result.Is_Ok then
            declare
               Stats : constant Test_Statistics := Stats_Result.Get_Ok;
            begin
               Output.Write_Line ("");
               Print_Test_Summary ("Chunk Unit Tests", Stats, Output);
               return Test_Stats_Result.Ok (Stats);
            end;
         else
            return Stats_Result;
         end if;
      end;
   end Run_All_Tests;

end Test_Chunk;
