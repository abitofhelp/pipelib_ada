--  =============================================================================
--  Test_Stage_Interface - Stage Interface Unit Tests Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;
with Pipelib.Core.Domain.Services.Stages.Stage_Interface;

package body Test_Stage_Interface is

   --  Test types for interface instantiation
   type Test_Input_Type is new Integer;
   type Test_Output_Type is new String (1 .. 10);

   --  Instantiate the stage interface for testing
   package Test_Stage_Interface_Pkg is new Pipelib.Core.Domain.Services.Stages.Stage_Interface (
      Input_Type  => Test_Input_Type,
      Output_Type => Test_Output_Type
   );
   use Test_Stage_Interface_Pkg;

   --  Mock implementations for testing
   type Mock_Stage_Type is new Stage_Interface with record
      Processed_Count : Natural := 0;
      Bytes_Count     : Long_Long_Integer := 0;
      Stage_Name      : Unbounded_String := To_Unbounded_String ("Mock_Stage");
      Ready_State     : Boolean := True;
      Initialized     : Boolean := False;
   end record;

   --  All overriding operations must be declared immediately after the type
   overriding function Process (Stage : in out Mock_Stage_Type; Input : Test_Input_Type)
      return Process_Result.Result;
   overriding function Initialize (Stage : in out Mock_Stage_Type)
      return Status_Result.Result;
   overriding function Cleanup (Stage : in out Mock_Stage_Type)
      return Status_Result.Result;
   overriding function Name (Stage : Mock_Stage_Type) return String;
   overriding function Is_Ready (Stage : Mock_Stage_Type) return Boolean;
   overriding function Items_Processed (Stage : Mock_Stage_Type) return Natural;
   overriding function Bytes_Processed (Stage : Mock_Stage_Type) return Long_Long_Integer;
   overriding function Can_Process_In_Parallel (Stage : Mock_Stage_Type) return Boolean;
   overriding function Estimated_Throughput_MB_Per_Sec (Stage : Mock_Stage_Type) return Float;

   overriding function Process (Stage : in out Mock_Stage_Type; Input : Test_Input_Type) return Process_Result.Result is
   begin
      Stage.Processed_Count := Stage.Processed_Count + 1;
      Stage.Bytes_Count := Stage.Bytes_Count + Long_Long_Integer (Integer (Input));

      -- Simple processing: convert input to string
      declare
         Input_Str : constant String := Integer (Input)'Image;
         Trimmed_Str : constant String := (if Input_Str (1) = ' ' then Input_Str (2 .. Input_Str'Last) else Input_Str);
         Output_Str : Test_Output_Type;
      begin
         -- Pad or truncate to fit the output type
         for I in Output_Str'Range loop
            if I - Output_Str'First + 1 <= Trimmed_Str'Length then
               Output_Str (I) := Trimmed_Str (Trimmed_Str'First + (I - Output_Str'First));
            else
               Output_Str (I) := ' ';
            end if;
         end loop;
         return Process_Result.Ok (Output_Str);
      end;
   end Process;

   overriding function Initialize (Stage : in out Mock_Stage_Type) return Status_Result.Result is
   begin
      Stage.Initialized := True;
      Stage.Processed_Count := 0;
      Stage.Bytes_Count := 0;
      return Status_Result.Ok (True);
   end Initialize;

   overriding function Cleanup (Stage : in out Mock_Stage_Type) return Status_Result.Result is
   begin
      Stage.Initialized := False;
      return Status_Result.Ok (True);
   end Cleanup;

   overriding function Name (Stage : Mock_Stage_Type) return String is
   begin
      return To_String (Stage.Stage_Name);
   end Name;

   overriding function Is_Ready (Stage : Mock_Stage_Type) return Boolean is
   begin
      return Stage.Ready_State and Stage.Initialized;
   end Is_Ready;

   overriding function Items_Processed (Stage : Mock_Stage_Type) return Natural is
   begin
      return Stage.Processed_Count;
   end Items_Processed;

   overriding function Bytes_Processed (Stage : Mock_Stage_Type) return Long_Long_Integer is
   begin
      return Stage.Bytes_Count;
   end Bytes_Processed;

   overriding function Can_Process_In_Parallel (Stage : Mock_Stage_Type) return Boolean is
   begin
      return False;  -- Basic mock doesn't support parallel processing
   end Can_Process_In_Parallel;

   overriding function Estimated_Throughput_MB_Per_Sec (Stage : Mock_Stage_Type) return Float is
   begin
      return 0.0;  -- Basic mock has no throughput estimate
   end Estimated_Throughput_MB_Per_Sec;

   --  Second mock implementation for polymorphism testing
   type Advanced_Mock_Stage_Type is new Stage_Interface with record
      Processed_Count : Natural := 0;
      Bytes_Count     : Long_Long_Integer := 0;
      Stage_Name      : Unbounded_String := To_Unbounded_String ("Advanced_Mock_Stage");
      Ready_State     : Boolean := True;
      Initialized     : Boolean := False;
      Parallel_Capable : Boolean := True;
      Throughput       : Float := 100.5;
   end record;

   --  All overriding operations must be declared immediately after the type
   overriding function Process (Stage : in out Advanced_Mock_Stage_Type; Input : Test_Input_Type)
      return Process_Result.Result;
   overriding function Initialize (Stage : in out Advanced_Mock_Stage_Type)
      return Status_Result.Result;
   overriding function Cleanup (Stage : in out Advanced_Mock_Stage_Type)
      return Status_Result.Result;
   overriding function Name (Stage : Advanced_Mock_Stage_Type) return String;
   overriding function Is_Ready (Stage : Advanced_Mock_Stage_Type) return Boolean;
   overriding function Items_Processed (Stage : Advanced_Mock_Stage_Type) return Natural;
   overriding function Bytes_Processed (Stage : Advanced_Mock_Stage_Type) return Long_Long_Integer;
   overriding function Can_Process_In_Parallel (Stage : Advanced_Mock_Stage_Type) return Boolean;
   overriding function Estimated_Throughput_MB_Per_Sec (Stage : Advanced_Mock_Stage_Type) return Float;

   overriding function Process (Stage : in out Advanced_Mock_Stage_Type; Input : Test_Input_Type)
      return Process_Result.Result is
   begin
      Stage.Processed_Count := Stage.Processed_Count + 1;
      Stage.Bytes_Count := Stage.Bytes_Count + Long_Long_Integer (Integer (Input)) * 2; -- Different processing

      -- Advanced processing: double the input and convert
      declare
         Doubled_Input : constant Integer := Integer (Input) * 2;
         Input_Str : constant String := Doubled_Input'Image;
         Trimmed_Str : constant String := (if Input_Str (1) = ' ' then Input_Str (2 .. Input_Str'Last) else Input_Str);
         Output_Str : Test_Output_Type;
      begin
         for I in Output_Str'Range loop
            if I - Output_Str'First + 1 <= Trimmed_Str'Length then
               Output_Str (I) := Trimmed_Str (Trimmed_Str'First + (I - Output_Str'First));
            else
               Output_Str (I) := ' ';
            end if;
         end loop;
         return Process_Result.Ok (Output_Str);
      end;
   end Process;

   overriding function Initialize (Stage : in out Advanced_Mock_Stage_Type) return Status_Result.Result is
   begin
      Stage.Initialized := True;
      Stage.Processed_Count := 0;
      Stage.Bytes_Count := 0;
      return Status_Result.Ok (True);
   end Initialize;

   overriding function Cleanup (Stage : in out Advanced_Mock_Stage_Type) return Status_Result.Result is
   begin
      Stage.Initialized := False;
      return Status_Result.Ok (True);
   end Cleanup;

   overriding function Name (Stage : Advanced_Mock_Stage_Type) return String is
   begin
      return To_String (Stage.Stage_Name);
   end Name;

   overriding function Is_Ready (Stage : Advanced_Mock_Stage_Type) return Boolean is
   begin
      return Stage.Ready_State and Stage.Initialized;
   end Is_Ready;

   overriding function Items_Processed (Stage : Advanced_Mock_Stage_Type) return Natural is
   begin
      return Stage.Processed_Count;
   end Items_Processed;

   overriding function Bytes_Processed (Stage : Advanced_Mock_Stage_Type) return Long_Long_Integer is
   begin
      return Stage.Bytes_Count;
   end Bytes_Processed;

   -- Override default implementations
   overriding function Can_Process_In_Parallel (Stage : Advanced_Mock_Stage_Type) return Boolean is
   begin
      return Stage.Parallel_Capable;
   end Can_Process_In_Parallel;

   overriding function Estimated_Throughput_MB_Per_Sec (Stage : Advanced_Mock_Stage_Type) return Float is
   begin
      return Stage.Throughput;
   end Estimated_Throughput_MB_Per_Sec;

   --  ==========================================================================
   --  Test Implementation
   --  ==========================================================================

   function Test_Interface_Instantiation return Void_Result.Result is
   begin
      -- Test that the interface can be instantiated with proper types
      -- This is mainly a compilation test - if we get here, instantiation worked

      -- Verify result types exist and can be used
      declare
         Success_Result : constant Process_Result.Result := Process_Result.Ok (Test_Output_Type'("test      "));
         Error_Result : constant Process_Result.Result := Process_Result.Err (To_Unbounded_String ("test error"));
         Status_Ok : constant Status_Result.Result := Status_Result.Ok (True);
         Status_Err : constant Status_Result.Result := Status_Result.Err (To_Unbounded_String ("status error"));
      begin
         if not Success_Result.Is_Ok then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Success result should be Ok"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Interface_Instantiation")
            ));
         end if;

         if Error_Result.Is_Ok then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Error result should be Err"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Interface_Instantiation")
            ));
         end if;

         if not Status_Ok.Is_Ok then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Status Ok should be Ok"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Interface_Instantiation")
            ));
         end if;

         if Status_Err.Is_Ok then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Status Err should be Err"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Interface_Instantiation")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Interface_Instantiation;

   function Test_Concrete_Implementation return Void_Result.Result is
      Stage : Mock_Stage_Type;
   begin
      -- Test initialization
      declare
         Init_Result : constant Status_Result.Result := Initialize (Stage);
      begin
         if not Init_Result.Is_Ok then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Initialize should succeed"),
               Details     => Init_Result.Get_Err,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Concrete_Implementation")
            ));
         end if;
      end;

      -- Test ready state
      if not Is_Ready (Stage) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Stage should be ready after initialization"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Concrete_Implementation")
         ));
      end if;

      -- Test name
      if Name (Stage) /= "Mock_Stage" then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Stage name incorrect"),
            Details     => To_Unbounded_String ("Expected: Mock_Stage, Got: " & Name (Stage)),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Concrete_Implementation")
         ));
      end if;

      -- Test processing
      declare
         Input : constant Test_Input_Type := Test_Input_Type (42);
         Process_Result_Val : constant Process_Result.Result := Process (Stage, Input);
      begin
         if not Process_Result_Val.Is_Ok then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Process should succeed"),
               Details     => Process_Result_Val.Get_Err,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Concrete_Implementation")
            ));
         end if;

         -- Verify output format
         declare
            Output : constant Test_Output_Type := Process_Result_Val.Get_Ok;
         begin
            if Output (1) /= '4' or Output (2) /= '2' then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Process output incorrect"),
                  Details     => To_Unbounded_String ("Got: " & String (Output)),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Concrete_Implementation")
               ));
            end if;
         end;
      end;

      -- Test statistics
      if Items_Processed (Stage) /= 1 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Items processed count incorrect"),
            Details     => To_Unbounded_String ("Expected: 1, Got: " & Items_Processed (Stage)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Concrete_Implementation")
         ));
      end if;

      if Bytes_Processed (Stage) /= 42 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Bytes processed count incorrect"),
            Details     => To_Unbounded_String ("Expected: 42, Got: " & Bytes_Processed (Stage)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Concrete_Implementation")
         ));
      end if;

      -- Test cleanup
      declare
         Cleanup_Result : constant Status_Result.Result := Cleanup (Stage);
      begin
         if not Cleanup_Result.Is_Ok then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Cleanup should succeed"),
               Details     => Cleanup_Result.Get_Err,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Concrete_Implementation")
            ));
         end if;
      end;

      -- Verify not ready after cleanup
      if Is_Ready (Stage) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Stage should not be ready after cleanup"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Concrete_Implementation")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Concrete_Implementation;

   function Test_Interface_Polymorphism return Void_Result.Result is
      Stage1 : Mock_Stage_Type;
      Stage2 : Advanced_Mock_Stage_Type;
   begin
      -- Initialize both stages
      declare
         Init1 : constant Status_Result.Result := Initialize (Stage1);
         Init2 : constant Status_Result.Result := Initialize (Stage2);
      begin
         if not Init1.Is_Ok or not Init2.Is_Ok then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Initialization should succeed for both stages"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Interface_Polymorphism")
            ));
         end if;
      end;

      -- Test polymorphic behavior through interface
      declare
         procedure Test_Stage_Via_Interface (Stage : in out Stage_Interface'Class; Expected_Name : String) is
            Input : constant Test_Input_Type := Test_Input_Type (10);
            Process_Result_Val : constant Process_Result.Result := Process (Stage, Input);
         begin
            -- Test that all interface methods work polymorphically
            if Name (Stage) /= Expected_Name then
               raise Program_Error with "Name mismatch: expected " & Expected_Name & ", got " & Name (Stage);
            end if;

            if not Is_Ready (Stage) then
               raise Program_Error with "Stage should be ready";
            end if;

            if not Process_Result_Val.Is_Ok then
               raise Program_Error with "Process should succeed";
            end if;

            if Items_Processed (Stage) = 0 then
               raise Program_Error with "Items processed should be > 0";
            end if;

            if Bytes_Processed (Stage) = 0 then
               raise Program_Error with "Bytes processed should be > 0";
            end if;
         end Test_Stage_Via_Interface;
      begin
         Test_Stage_Via_Interface (Stage1, "Mock_Stage");
         Test_Stage_Via_Interface (Stage2, "Advanced_Mock_Stage");
      exception
         when E : others =>
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Polymorphic interface test failed"),
               Details     => To_Unbounded_String (Ada.Exceptions.Exception_Message (E)),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Interface_Polymorphism")
            ));
      end;

      -- Test that different implementations produce different results
      declare
         Input : constant Test_Input_Type := Test_Input_Type (5);
         Result1 : constant Process_Result.Result := Process (Stage1, Input);
         Result2 : constant Process_Result.Result := Process (Stage2, Input);
      begin
         if not Result1.Is_Ok or not Result2.Is_Ok then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Both processes should succeed"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Interface_Polymorphism")
            ));
         end if;

         -- Verify different byte processing (Stage2 doubles the bytes)
         if Bytes_Processed (Stage1) = Bytes_Processed (Stage2) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Different implementations should produce different byte counts"),
               Details     => To_Unbounded_String ("Both got: " & Bytes_Processed (Stage1)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Interface_Polymorphism")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Interface_Polymorphism;

   function Test_Default_Implementations return Void_Result.Result is
      Basic_Stage : Mock_Stage_Type;
      Advanced_Stage : Advanced_Mock_Stage_Type;
   begin
      -- Test default implementations
      -- Basic stage should use defaults
      if Can_Process_In_Parallel (Basic_Stage) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Basic stage should not support parallel processing (default)"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Default_Implementations")
         ));
      end if;

      if Estimated_Throughput_MB_Per_Sec (Basic_Stage) /= 0.0 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Basic stage should have 0.0 throughput (default)"),
            Details     => To_Unbounded_String ("Got: " & Estimated_Throughput_MB_Per_Sec (Basic_Stage)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Default_Implementations")
         ));
      end if;

      -- Advanced stage should override defaults
      if not Can_Process_In_Parallel (Advanced_Stage) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Advanced stage should support parallel processing (overridden)"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Default_Implementations")
         ));
      end if;

      if Estimated_Throughput_MB_Per_Sec (Advanced_Stage) /= 100.5 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Advanced stage should have custom throughput (overridden)"),
            Details     => To_Unbounded_String ("Expected: 100.5, Got: " &
                                               Estimated_Throughput_MB_Per_Sec (Advanced_Stage)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Default_Implementations")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Default_Implementations;

   function Test_Multiple_Implementations return Void_Result.Result is
      Stages : array (1 .. 3) of access Stage_Interface'Class;
      Stage1 : aliased Mock_Stage_Type;
      Stage2 : aliased Advanced_Mock_Stage_Type;
      Stage3 : aliased Mock_Stage_Type;
   begin
      -- Initialize array of different stage implementations
      Stages (1) := Stage1'Access;
      Stages (2) := Stage2'Access;
      Stages (3) := Stage3'Access;

      -- Initialize all stages
      for I in Stages'Range loop
         declare
            Init_Result : constant Status_Result.Result := Initialize (Stages (I).all);
         begin
            if not Init_Result.Is_Ok then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Initialize failed for stage " & I'Image),
                  Details     => Init_Result.Get_Err,
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Multiple_Implementations")
               ));
            end if;
         end;
      end loop;

      -- Process same input through all stages
      declare
         Input : constant Test_Input_Type := Test_Input_Type (7);
      begin
         for I in Stages'Range loop
            declare
               Process_Result_Val : constant Process_Result.Result := Process (Stages (I).all, Input);
            begin
               if not Process_Result_Val.Is_Ok then
                  return Void_Result.Err (Test_Error'(
                     Kind        => Assertion_Failed,
                     Message     => To_Unbounded_String ("Process failed for stage " & I'Image),
                     Details     => Process_Result_Val.Get_Err,
                     Line_Number => 0,
                     Test_Name   => To_Unbounded_String ("Test_Multiple_Implementations")
                  ));
               end if;

               -- Verify all stages processed the item
               if Items_Processed (Stages (I).all) /= 1 then
                  return Void_Result.Err (Test_Error'(
                     Kind        => Assertion_Failed,
                     Message     => To_Unbounded_String ("Items processed incorrect for stage " & I'Image),
                     Details     => To_Unbounded_String ("Expected: 1, Got: " & Items_Processed (Stages (I).all)'Image),
                     Line_Number => 0,
                     Test_Name   => To_Unbounded_String ("Test_Multiple_Implementations")
                  ));
               end if;
            end;
         end loop;
      end;

      -- Verify that different stage types produced different byte counts
      -- Stage2 (Advanced) should have different byte processing than Stage1 and Stage3 (Mock)
      declare
         Bytes1 : constant Long_Long_Integer := Bytes_Processed (Stages (1).all);
         Bytes2 : constant Long_Long_Integer := Bytes_Processed (Stages (2).all);
         Bytes3 : constant Long_Long_Integer := Bytes_Processed (Stages (3).all);
      begin
         if Bytes1 /= Bytes3 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Same stage types should produce same byte counts"),
               Details     => To_Unbounded_String ("Stage1: " & Bytes1'Image & ", Stage3: " & Bytes3'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Multiple_Implementations")
            ));
         end if;

         if Bytes2 = Bytes1 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Different stage types should produce different byte counts"),
               Details     => To_Unbounded_String ("Both produced: " & Bytes1'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Multiple_Implementations")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Multiple_Implementations;

   function Test_Interface_Contract_Validation return Void_Result.Result is
      Stage : Mock_Stage_Type;
   begin
      -- Test that interface contracts are enforced

      -- Test uninitialized stage behavior
      if Is_Ready (Stage) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Uninitialized stage should not be ready"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Interface_Contract_Validation")
         ));
      end if;

      -- Test interface methods exist and return proper types
      declare
         Stage_Name : constant String := Name (Stage);
         Items_Count : constant Natural := Items_Processed (Stage);
         Bytes_Count : constant Long_Long_Integer := Bytes_Processed (Stage);
         Can_Parallel : constant Boolean := Can_Process_In_Parallel (Stage);
         Throughput : constant Float := Estimated_Throughput_MB_Per_Sec (Stage);
      begin
         -- Just verify the methods can be called and return expected types
         if Stage_Name'Length = 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Stage name should not be empty"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Interface_Contract_Validation")
            ));
         end if;

         -- Items_Count is Natural, so it's always >= 0
         -- This check documents the contract but can never fail
         pragma Warnings (Off, "condition can only be True if invalid values present");
         pragma Warnings (Off, "condition is always False");
         if Items_Count < 0 then
         pragma Warnings (On, "condition can only be True if invalid values present");
         pragma Warnings (On, "condition is always False");
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Items count should be non-negative"),
               Details     => To_Unbounded_String ("Got: " & Items_Count'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Interface_Contract_Validation")
            ));
         end if;

         if Bytes_Count < 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Bytes count should be non-negative"),
               Details     => To_Unbounded_String ("Got: " & Bytes_Count'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Interface_Contract_Validation")
            ));
         end if;

         -- Can_Parallel and Throughput are implementation-dependent, just verify they're accessible
         pragma Unreferenced (Can_Parallel, Throughput);
      end;

      return Void_Result.Ok (True);
   end Test_Interface_Contract_Validation;

   --  ==========================================================================
   --  Test Suite Runner
   --  ==========================================================================

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Tests : Test_Results_Array (1 .. 6);
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
      Output.Write_Line ("=== Running Stage Interface Unit Tests ===");
      Output.Write_Line ("");

      -- Run all tests
      Add_Test_Result ("Test_Interface_Instantiation", Test_Interface_Instantiation'Access);
      Add_Test_Result ("Test_Concrete_Implementation", Test_Concrete_Implementation'Access);
      Add_Test_Result ("Test_Interface_Polymorphism", Test_Interface_Polymorphism'Access);
      Add_Test_Result ("Test_Default_Implementations", Test_Default_Implementations'Access);
      Add_Test_Result ("Test_Multiple_Implementations", Test_Multiple_Implementations'Access);
      Add_Test_Result ("Test_Interface_Contract_Validation", Test_Interface_Contract_Validation'Access);

      -- Generate summary
      declare
         Stats_Result : constant Test_Stats_Result.Result :=
            Run_Test_Suite ("Stage_Interface_Tests", Tests (1 .. Index - 1), Output);
      begin
         if Stats_Result.Is_Ok then
            declare
               Stats : constant Test_Statistics := Stats_Result.Get_Ok;
            begin
               Output.Write_Line ("");
               Print_Test_Summary ("Stage Interface Unit Tests", Stats, Output);
               return Test_Stats_Result.Ok (Stats);
            end;
         else
            return Stats_Result;
         end if;
      end;
   end Run_All_Tests;

end Test_Stage_Interface;
