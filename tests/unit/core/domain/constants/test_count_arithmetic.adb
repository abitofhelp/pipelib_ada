--  =============================================================================
--  Test_Count_Arithmetic - Unit Tests for Count Type Arithmetic Operations
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Pipelib.Core.Domain.Constants; use Pipelib.Core.Domain.Constants;
with Pipelib.Core.Domain.Constants.Count_Arithmetic; use Pipelib.Core.Domain.Constants.Count_Arithmetic;
with Abohlib.Core.Domain.Result;
with Abohlib.Core.Testing;

procedure Test_Count_Arithmetic is

   use Abohlib.Core.Testing;

   package Test_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => Boolean, Err_Type => Unbounded_String);

   --  Test output implementation
   type Test_Output_Port is new Output_Port_Interface with null record;

   overriding procedure Log_Message
     (Self : Test_Output_Port; Message : String) is
   begin
      Put_Line (Message);
   end Log_Message;

   Output : aliased Test_Output_Port;

   --  =========================================================================
   --  Read_Count_Type Tests
   --  =========================================================================

   function Test_Read_Count_Addition
     (Output : access Test_Output_Port'Class) return Boolean is
      Count1 : constant Read_Count_Type := 10;
      Count2 : constant Read_Count_Type := 20;
      Result : Read_Count_Type;
   begin
      -- Test same type addition
      Result := Count1 + Count2;
      Assert (Natural (Result) = 30, "Read_Count + Read_Count failed");

      -- Test addition with Natural
      Result := Count1 + 5;
      Assert (Natural (Result) = 15, "Read_Count + Natural failed");

      -- Test Natural + Read_Count
      Result := 7 + Count2;
      Assert (Natural (Result) = 27, "Natural + Read_Count failed");

      return True;
   end Test_Read_Count_Addition;

   function Test_Read_Count_Subtraction
     (Output : access Test_Output_Port'Class) return Boolean is
      Count1 : constant Read_Count_Type := 30;
      Count2 : constant Read_Count_Type := 10;
      Result : Read_Count_Type;
   begin
      -- Test same type subtraction
      Result := Count1 - Count2;
      Assert (Natural (Result) = 20, "Read_Count - Read_Count failed");

      -- Test subtraction with Natural
      Result := Count1 - 5;
      Assert (Natural (Result) = 25, "Read_Count - Natural failed");

      return True;
   end Test_Read_Count_Subtraction;

   function Test_Read_Count_Multiplication
     (Output : access Test_Output_Port'Class) return Boolean is
      Count : constant Read_Count_Type := 10;
      Result : Read_Count_Type;
   begin
      -- Test same type multiplication
      Result := Count * Count;
      Assert (Natural (Result) = 100, "Read_Count * Read_Count failed");

      -- Test multiplication with Natural
      Result := Count * 3;
      Assert (Natural (Result) = 30, "Read_Count * Natural failed");

      -- Test Natural * Read_Count
      Result := 4 * Count;
      Assert (Natural (Result) = 40, "Natural * Read_Count failed");

      return True;
   end Test_Read_Count_Multiplication;

   function Test_Read_Count_Division
     (Output : access Test_Output_Port'Class) return Boolean is
      Count : constant Read_Count_Type := 20;
      Result : Read_Count_Type;
   begin
      -- Test same type division
      Result := Count / Read_Count_Type (4);
      Assert (Natural (Result) = 5, "Read_Count / Read_Count failed");

      -- Test division by Natural
      Result := Count / 5;
      Assert (Natural (Result) = 4, "Read_Count / Natural failed");

      return True;
   end Test_Read_Count_Division;

   function Test_Read_Count_Modulo
     (Output : access Test_Output_Port'Class) return Boolean is
      Count : constant Read_Count_Type := 17;
      Result : Read_Count_Type;
   begin
      -- Test same type modulo
      Result := Count mod Read_Count_Type (5);
      Assert (Natural (Result) = 2, "Read_Count mod Read_Count failed");

      -- Test modulo with Natural
      Result := Count mod 3;
      Assert (Natural (Result) = 2, "Read_Count mod Natural failed");

      return True;
   end Test_Read_Count_Modulo;

   function Test_Read_Count_Comparisons
     (Output : access Test_Output_Port'Class) return Boolean is
      Count1 : constant Read_Count_Type := 10;
      Count2 : constant Read_Count_Type := 20;
      Count3 : constant Read_Count_Type := 10;
   begin
      Assert (Count1 < Count2, "Read_Count < failed");
      Assert (Count1 <= Count2, "Read_Count <= failed (less)");
      Assert (Count1 <= Count3, "Read_Count <= failed (equal)");
      Assert (Count2 > Count1, "Read_Count > failed");
      Assert (Count2 >= Count1, "Read_Count >= failed (greater)");
      Assert (Count1 >= Count3, "Read_Count >= failed (equal)");
      Assert (Count1 = Count3, "Read_Count = failed");
      Assert (Count1 /= Count2, "Read_Count /= failed");

      return True;
   end Test_Read_Count_Comparisons;

   function Test_Read_Count_Increment_Decrement
     (Output : access Test_Output_Port'Class) return Boolean is
      Count : Read_Count_Type := 10;
   begin
      Count := Increment (Count);
      Assert (Natural (Count) = 11, "Increment failed");

      Count := Decrement (Count);
      Assert (Natural (Count) = 10, "Decrement failed");

      return True;
   end Test_Read_Count_Increment_Decrement;

   --  =========================================================================
   --  Processed_Count_Type Tests
   --  =========================================================================

   function Test_Processed_Count_Addition
     (Output : access Test_Output_Port'Class) return Boolean is
      Count1 : constant Processed_Count_Type := 15;
      Count2 : constant Processed_Count_Type := 25;
      Result : Processed_Count_Type;
   begin
      Result := Count1 + Count2;
      Assert (Natural (Result) = 40, "Processed_Count addition failed");

      Result := Count1 + 10;
      Assert (Natural (Result) = 25, "Processed_Count + Natural failed");

      return True;
   end Test_Processed_Count_Addition;

   function Test_Processed_Count_Complex_Expression
     (Output : access Test_Output_Port'Class) return Boolean is
      Total : constant Processed_Count_Type := 100;
      Batch : constant Processed_Count_Type := 10;
      Done  : constant Processed_Count_Type := 45;
      Result : Processed_Count_Type;
   begin
      -- Test complex expression: (Total - Done) / Batch
      Result := (Total - Done) / Batch;
      Assert (Natural (Result) = 5, "Complex expression failed");

      -- Test percentage calculation pattern
      Result := Done * 100 / Total;
      Assert (Natural (Result) = 45, "Percentage calculation failed");

      return True;
   end Test_Processed_Count_Complex_Expression;

   --  =========================================================================
   --  Written_Count_Type Tests
   --  =========================================================================

   function Test_Written_Count_Operations
     (Output : access Test_Output_Port'Class) return Boolean is
      Written : Written_Count_Type := 0;
      Batch   : constant Natural := 5;
   begin
      -- Simulate writing batches
      for I in 1 .. 10 loop
         Written := Written + Batch;
      end loop;

      Assert (Natural (Written) = 50, "Batch writing count failed");

      -- Test averaging
      declare
         Average : constant Written_Count_Type := Written / 10;
      begin
         Assert (Natural (Average) = 5, "Average calculation failed");
      end;

      return True;
   end Test_Written_Count_Operations;

   --  =========================================================================
   --  Error_Count_Type Tests
   --  =========================================================================

   function Test_Error_Count_Operations
     (Output : access Test_Output_Port'Class) return Boolean is
      Errors : Error_Count_Type := 0;
      Total  : constant Natural := 100;
   begin
      -- Simulate error accumulation
      Errors := Errors + 3;
      Errors := Increment (Errors);
      Errors := Errors + 1;

      Assert (Natural (Errors) = 5, "Error accumulation failed");

      -- Test error rate calculation pattern
      declare
         Error_Rate : constant Natural := Natural (Errors) * 100 / Total;
      begin
         Assert (Error_Rate = 5, "Error rate calculation failed");
      end;

      return True;
   end Test_Error_Count_Operations;

   --  =========================================================================
   --  Chunk_Count_Type Tests
   --  =========================================================================

   function Test_Chunk_Count_Operations
     (Output : access Test_Output_Port'Class) return Boolean is
      Total_Chunks : constant Chunk_Count_Type := 1000;
      Workers      : constant Natural := 8;
      Per_Worker   : Chunk_Count_Type;
      Remainder    : Chunk_Count_Type;
   begin
      -- Test work distribution pattern
      Per_Worker := Total_Chunks / Workers;
      Remainder  := Total_Chunks mod Workers;

      Assert (Natural (Per_Worker) = 125, "Chunks per worker failed");
      Assert (Natural (Remainder) = 0, "Remainder calculation failed");

      -- Test reconstruction
      declare
         Reconstructed : constant Chunk_Count_Type := Per_Worker * Workers + Remainder;
      begin
         Assert (Reconstructed = Total_Chunks, "Reconstruction failed");
      end;

      return True;
   end Test_Chunk_Count_Operations;

   --  =========================================================================
   --  Mixed Type Safety Tests
   --  =========================================================================

   function Test_Type_Safety
     (Output : access Test_Output_Port'Class) return Boolean is
      Read_Count  : constant Read_Count_Type := 10;
      Error_Count : constant Error_Count_Type := 2;
   begin
      -- These should not compile (commented out):
      -- declare
      --    Invalid : Read_Count_Type := Read_Count + Error_Count;  -- Type error
      -- begin
      --    null;
      -- end;

      -- But conversion is explicit and safe
      declare
         Total : constant Natural := Natural (Read_Count) + Natural (Error_Count);
      begin
         Assert (Total = 12, "Explicit conversion failed");
      end;

      return True;
   end Test_Type_Safety;

   --  =========================================================================
   --  Edge Case Tests
   --  =========================================================================

   function Test_Edge_Cases
     (Output : access Test_Output_Port'Class) return Boolean is
      Zero  : constant Chunk_Count_Type := 0;
      Large : constant Chunk_Count_Type := Chunk_Count_Type (Natural'Last / 2);
   begin
      -- Test zero operations
      Assert (Zero + Zero = Zero, "Zero + Zero failed");
      Assert (Zero * 100 = Zero, "Zero * N failed");
      Assert (100 * Zero = Zero, "N * Zero failed");

      -- Test large values (avoid overflow)
      declare
         Result : constant Chunk_Count_Type := Large + Large;
      begin
         Assert (Natural (Result) = Natural'Last / 2 * 2, "Large addition failed");
      end;

      return True;
   end Test_Edge_Cases;

   --  =========================================================================
   --  Real-World Usage Pattern Tests
   --  =========================================================================

   function Test_Pipeline_Progress_Pattern
     (Output : access Test_Output_Port'Class) return Boolean is
      Total_Chunks    : constant Chunk_Count_Type := 100;
      Chunks_Read     : Read_Count_Type := 0;
      Chunks_Processed: Processed_Count_Type := 0;
      Chunks_Written  : Written_Count_Type := 0;
      Chunks_Errors   : Error_Count_Type := 0;
   begin
      -- Simulate pipeline processing
      for I in 1 .. Natural (Total_Chunks) loop
         -- Read chunk
         Chunks_Read := Increment (Chunks_Read);

         -- Process chunk (90% success rate simulation)
         if I mod 10 /= 0 then
            Chunks_Processed := Increment (Chunks_Processed);
            Chunks_Written := Increment (Chunks_Written);
         else
            Chunks_Errors := Increment (Chunks_Errors);
         end if;
      end loop;

      -- Verify counts
      Assert (Chunks_Read = Read_Count_Type (Total_Chunks), "Read count mismatch");
      Assert (Natural (Chunks_Processed) = 90, "Processed count mismatch");
      Assert (Natural (Chunks_Written) = 90, "Written count mismatch");
      Assert (Natural (Chunks_Errors) = 10, "Error count mismatch");

      -- Calculate statistics
      declare
         Success_Rate : constant Natural :=
           Natural (Chunks_Processed) * 100 / Natural (Chunks_Read);
         Error_Rate   : constant Natural :=
           Natural (Chunks_Errors) * 100 / Natural (Chunks_Read);
      begin
         Assert (Success_Rate = 90, "Success rate calculation failed");
         Assert (Error_Rate = 10, "Error rate calculation failed");
      end;

      return True;
   end Test_Pipeline_Progress_Pattern;

   --  Test suite
   Suite : constant Test_Suite := Create_Suite ("Count Type Arithmetic Operations");

begin
   -- Read_Count_Type tests
   Add_Test (Suite, "Read_Count_Type Addition", Test_Read_Count_Addition'Access);
   Add_Test (Suite, "Read_Count_Type Subtraction", Test_Read_Count_Subtraction'Access);
   Add_Test (Suite, "Read_Count_Type Multiplication", Test_Read_Count_Multiplication'Access);
   Add_Test (Suite, "Read_Count_Type Division", Test_Read_Count_Division'Access);
   Add_Test (Suite, "Read_Count_Type Modulo", Test_Read_Count_Modulo'Access);
   Add_Test (Suite, "Read_Count_Type Comparisons", Test_Read_Count_Comparisons'Access);
   Add_Test (Suite, "Read_Count_Type Increment/Decrement", Test_Read_Count_Increment_Decrement'Access);

   -- Other type tests
   Add_Test (Suite, "Processed_Count_Type Addition", Test_Processed_Count_Addition'Access);
   Add_Test (Suite, "Processed_Count_Type Complex Expression", Test_Processed_Count_Complex_Expression'Access);
   Add_Test (Suite, "Written_Count_Type Operations", Test_Written_Count_Operations'Access);
   Add_Test (Suite, "Error_Count_Type Operations", Test_Error_Count_Operations'Access);
   Add_Test (Suite, "Chunk_Count_Type Operations", Test_Chunk_Count_Operations'Access);

   -- Safety and edge cases
   Add_Test (Suite, "Type Safety", Test_Type_Safety'Access);
   Add_Test (Suite, "Edge Cases", Test_Edge_Cases'Access);
   Add_Test (Suite, "Pipeline Progress Pattern", Test_Pipeline_Progress_Pattern'Access);

   -- Run tests
   Run_Suite (Suite, Output'Access);
end Test_Count_Arithmetic;
