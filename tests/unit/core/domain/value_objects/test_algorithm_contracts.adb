--  =============================================================================
--  Test_Algorithm_Contracts - Contract Validation Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Pipelib.Core.Domain.Value_Objects.Algorithm;
use Pipelib.Core.Domain.Value_Objects.Algorithm;

package body Test_Algorithm_Contracts is

   --  Test Create function contracts
   function Test_Create_Contracts return Void_Result.Result is
   begin
      -- Test precondition: Name'Length > 0
      declare
         Contract_Violated : Boolean := False;
      begin
         declare
            Alg : Algorithm_Type;
         begin
            Alg := Create ("");  -- Empty name should violate precondition
            Contract_Violated := False;
         exception
            when others =>
               Contract_Violated := True;
         end;

         if not Contract_Violated then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Create precondition not enforced"),
               Details     => To_Unbounded_String ("Empty name should raise exception"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Create_Contracts")
            ));
         end if;
      end;

      -- Test precondition: Is_Valid_Format (Name)
      declare
         Contract_Violated : Boolean := False;
      begin
         declare
            Alg : Algorithm_Type;
         begin
            Alg := Create ("Invalid-NAME");  -- Uppercase should violate precondition
            Contract_Violated := False;
         exception
            when others =>
               Contract_Violated := True;
         end;

         if not Contract_Violated then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Create format precondition not enforced"),
               Details     => To_Unbounded_String ("Invalid format should raise exception"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Create_Contracts")
            ));
         end if;
      end;

      -- Test valid creation
      declare
         Alg : constant Algorithm_Type := Create ("custom-algorithm");
      begin
         if Name (Alg) /= "custom-algorithm" then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Algorithm name not preserved"),
               Details     => To_Unbounded_String ("Expected: custom-algorithm, Got: " & Name (Alg)),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Create_Contracts")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Create_Contracts;

   --  Test Name accessor contracts
   function Test_Name_Accessor_Contracts return Void_Result.Result is
   begin
      declare
         Alg : constant Algorithm_Type := Create ("test-alg");
         Alg_Name : constant String := Name (Alg);
      begin
         -- Test postcondition: Name'Result'Length > 0
         if Alg_Name'Length = 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Name postcondition failed"),
               Details     => To_Unbounded_String ("Name should not be empty"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Name_Accessor_Contracts")
            ));
         end if;

         -- Test postcondition: Is_Valid_Format (Name'Result)
         if not Is_Valid_Format (Alg_Name) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Name format postcondition failed"),
               Details     => To_Unbounded_String ("Name format should be valid: " & Alg_Name),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Name_Accessor_Contracts")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Name_Accessor_Contracts;

   --  Test Is_Valid_Format contracts
   function Test_Is_Valid_Format_Contracts return Void_Result.Result is
   begin
      -- Test postcondition: exact character validation

      -- Valid formats
      if not Is_Valid_Format ("abc") then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Valid format rejected"),
            Details     => To_Unbounded_String ("'abc' should be valid"),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Is_Valid_Format_Contracts")
         ));
      end if;

      if not Is_Valid_Format ("test-123") then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Valid format rejected"),
            Details     => To_Unbounded_String ("'test-123' should be valid"),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Is_Valid_Format_Contracts")
         ));
      end if;

      if not Is_Valid_Format ("a1b2c3") then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Valid format rejected"),
            Details     => To_Unbounded_String ("'a1b2c3' should be valid"),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Is_Valid_Format_Contracts")
         ));
      end if;

      -- Invalid formats
      if Is_Valid_Format ("ABC") then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Invalid format accepted"),
            Details     => To_Unbounded_String ("'ABC' should be invalid (uppercase)"),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Is_Valid_Format_Contracts")
         ));
      end if;

      if Is_Valid_Format ("test_algo") then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Invalid format accepted"),
            Details     => To_Unbounded_String ("'test_algo' should be invalid (underscore)"),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Is_Valid_Format_Contracts")
         ));
      end if;

      if Is_Valid_Format ("test.algo") then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Invalid format accepted"),
            Details     => To_Unbounded_String ("'test.algo' should be invalid (period)"),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Is_Valid_Format_Contracts")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Is_Valid_Format_Contracts;

   --  Test factory function contracts
   function Test_Factory_Function_Contracts return Void_Result.Result is
   begin
      -- Test Brotli factory
      declare
         Brotli_Alg : constant Algorithm_Type := Brotli;
      begin
         if Name (Brotli_Alg) /= "brotli" then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Brotli name postcondition failed"),
               Details     => To_Unbounded_String ("Expected: brotli, Got: " & Name (Brotli_Alg)),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Factory_Function_Contracts")
            ));
         end if;

         if Category (Brotli_Alg) /= Compression then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Brotli category postcondition failed"),
               Details     => To_Unbounded_String ("Expected: Compression, Got: " & Category (Brotli_Alg)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Factory_Function_Contracts")
            ));
         end if;
      end;

      -- Test SHA256 factory
      declare
         SHA256_Alg : constant Algorithm_Type := SHA256;
      begin
         if Name (SHA256_Alg) /= "sha256" then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("SHA256 name postcondition failed"),
               Details     => To_Unbounded_String ("Expected: sha256, Got: " & Name (SHA256_Alg)),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Factory_Function_Contracts")
            ));
         end if;

         if Category (SHA256_Alg) /= Hashing then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("SHA256 category postcondition failed"),
               Details     => To_Unbounded_String ("Expected: Hashing, Got: " & Category (SHA256_Alg)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Factory_Function_Contracts")
            ));
         end if;
      end;

      -- Test AES_256_GCM factory
      declare
         AES_Alg : constant Algorithm_Type := AES_256_GCM;
      begin
         if Name (AES_Alg) /= "aes-256-gcm" then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("AES name postcondition failed"),
               Details     => To_Unbounded_String ("Expected: aes-256-gcm, Got: " & Name (AES_Alg)),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Factory_Function_Contracts")
            ));
         end if;

         if Category (AES_Alg) /= Encryption then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("AES category postcondition failed"),
               Details     => To_Unbounded_String ("Expected: Encryption, Got: " & Category (AES_Alg)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Factory_Function_Contracts")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Factory_Function_Contracts;

   --  Test type invariant
   function Test_Type_Invariant return Void_Result.Result is
   begin
      -- The type invariant should ensure that every Algorithm_Type instance
      -- has a valid name and format. This is enforced automatically by Ada.

      -- Create various algorithms and verify they maintain the invariant
      declare
         Algs : constant array (1 .. 4) of Algorithm_Type := [
            Brotli,
            SHA256,
            AES_256_GCM,
            Create ("custom-alg")
         ];
      begin
         for I in Algs'Range loop
            -- The type invariant should guarantee these properties
            if Name (Algs (I))'Length = 0 then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Type invariant violated: empty name"),
                  Details     => To_Unbounded_String ("Algorithm at index" & I'Image),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Type_Invariant")
               ));
            end if;

            if not Is_Valid_Format (Name (Algs (I))) then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Type invariant violated: invalid format"),
                  Details     => To_Unbounded_String ("Algorithm: " & Name (Algs (I))),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Type_Invariant")
               ));
            end if;
         end loop;
      end;

      return Void_Result.Ok (True);
   end Test_Type_Invariant;

   --  Test value object semantics
   function Test_Value_Object_Semantics return Void_Result.Result is
   begin
      -- Test equality based on content, not identity
      declare
         Alg1 : constant Algorithm_Type := Create ("test-alg");
         Alg2 : constant Algorithm_Type := Create ("test-alg");
         Alg3 : constant Algorithm_Type := Create ("other-alg");
      begin
         if not (Alg1 = Alg2) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Value equality failed"),
               Details     => To_Unbounded_String ("Same content should be equal"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Value_Object_Semantics")
            ));
         end if;

         if Alg1 = Alg3 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Value inequality failed"),
               Details     => To_Unbounded_String ("Different content should not be equal"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Value_Object_Semantics")
            ));
         end if;
      end;

      -- Test immutability (value objects should not change after creation)
      declare
         Alg : constant Algorithm_Type := Create ("immutable-test");
         Original_Name : constant String := Name (Alg);
      begin
         -- Since it's a constant and value object, the name should never change
         if Name (Alg) /= Original_Name then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Immutability violated"),
               Details     => To_Unbounded_String ("Value object content changed"),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Value_Object_Semantics")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Value_Object_Semantics;

   --  Test suite runner
   function Run_All_Tests (Output : access Test_Output_Port'Class)
                          return Test_Stats_Result.Result is
      Tests : constant Test_Array := [
         (Name => To_Unbounded_String ("Create Contracts"),
          Func => Test_Create_Contracts'Access),
         (Name => To_Unbounded_String ("Name Accessor Contracts"),
          Func => Test_Name_Accessor_Contracts'Access),
         (Name => To_Unbounded_String ("Is_Valid_Format Contracts"),
          Func => Test_Is_Valid_Format_Contracts'Access),
         (Name => To_Unbounded_String ("Factory Function Contracts"),
          Func => Test_Factory_Function_Contracts'Access),
         (Name => To_Unbounded_String ("Type Invariant"),
          Func => Test_Type_Invariant'Access),
         (Name => To_Unbounded_String ("Value Object Semantics"),
          Func => Test_Value_Object_Semantics'Access)
      ];
   begin
      return Run_Test_Suite ("Algorithm Contract Validation", Tests, Output);
   end Run_All_Tests;

end Test_Algorithm_Contracts;
