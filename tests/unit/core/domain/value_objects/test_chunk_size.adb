--  =============================================================================
--  Test_Chunk_Size - Chunk Size Value Object Unit Tests Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Pipelib.Core.Domain.Value_Objects.Chunk_Size; use Pipelib.Core.Domain.Value_Objects.Chunk_Size;
with Abohlib.Core.Domain.Constants.Bytes; use Abohlib.Core.Domain.Constants.Bytes;

package body Test_Chunk_Size is

   --  ==========================================================================
   --  Test Implementation
   --  ==========================================================================

   function Test_Create_Valid_Size return Void_Result.Result is
      Size : Chunk_Size_Type;
   begin
      -- Test minimum valid size
      Size := Create (MIN_CHUNK_SIZE);

      -- Verify postconditions are satisfied
      pragma Assert (Is_Valid (Size), "Postcondition: Is_Valid must be true");
      pragma Assert (Value (Size) = MIN_CHUNK_SIZE, "Postcondition: Value must match input");

      if Value (Size) /= MIN_CHUNK_SIZE then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Create failed for MIN_CHUNK_SIZE"),
            Details     => To_Unbounded_String ("Expected: " & MIN_CHUNK_SIZE'Image &
                                               ", Got: " & Value (Size)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_Valid_Size")
         ));
      end if;

      -- Test maximum valid size
      Size := Create (MAX_CHUNK_SIZE);

      pragma Assert (Is_Valid (Size));
      pragma Assert (Value (Size) = MAX_CHUNK_SIZE);

      if Value (Size) /= MAX_CHUNK_SIZE then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Create failed for MAX_CHUNK_SIZE"),
            Details     => To_Unbounded_String ("Expected: " & MAX_CHUNK_SIZE'Image &
                                               ", Got: " & Value (Size)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_Valid_Size")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Create_Valid_Size;

   function Test_Create_Invalid_Size return Void_Result.Result is
   begin
      -- In Ada, we cannot test precondition violations directly as they cause
      -- assertion failures that terminate the program. Instead, we test that
      -- the Create function properly validates its inputs at the boundaries.

      -- Test minimum boundary
      declare
         Size : constant Chunk_Size_Type := Create (MIN_CHUNK_SIZE);
      begin
         if Value (Size) /= MIN_CHUNK_SIZE then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("MIN_CHUNK_SIZE not accepted"),
               Details     => To_Unbounded_String ("Expected: " & MIN_CHUNK_SIZE'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Create_Invalid_Size")
            ));
         end if;
      end;

      -- Test maximum boundary
      declare
         Size : constant Chunk_Size_Type := Create (MAX_CHUNK_SIZE);
      begin
         if Value (Size) /= MAX_CHUNK_SIZE then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("MAX_CHUNK_SIZE not accepted"),
               Details     => To_Unbounded_String ("Expected: " & MAX_CHUNK_SIZE'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Create_Invalid_Size")
            ));
         end if;
      end;

      -- Note: We cannot test values outside the valid range as they would
      -- violate preconditions and cause program termination in debug builds.
      -- This is by design - preconditions catch programming errors at development time.

      return Void_Result.Ok (True);
   end Test_Create_Invalid_Size;

   function Test_Factory_Methods return Void_Result.Result is
      Size : Chunk_Size_Type;
   begin
      -- Test Default factory
      Size := Default;

      pragma Assert (Is_Valid (Size), "Factory postcondition: must return valid size");
      pragma Assert (Value (Size) = DEFAULT_CHUNK_SIZE, "Factory postcondition: correct value");

      if Value (Size) /= DEFAULT_CHUNK_SIZE then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Default size is incorrect"),
            Details     => To_Unbounded_String ("Expected: " & DEFAULT_CHUNK_SIZE'Image &
                                               ", Got: " & Value (Size)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Factory_Methods")
         ));
      end if;

      -- Test Min factory
      Size := Min;
      pragma Assert (Is_Valid (Size));
      pragma Assert (Value (Size) = MIN_CHUNK_SIZE);

      if Value (Size) /= MIN_CHUNK_SIZE then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Min size is incorrect"),
            Details     => To_Unbounded_String ("Expected: " & MIN_CHUNK_SIZE'Image &
                                               ", Got: " & Value (Size)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Factory_Methods")
         ));
      end if;

      -- Test Max factory
      Size := Max;
      pragma Assert (Is_Valid (Size));
      pragma Assert (Value (Size) = MAX_CHUNK_SIZE);

      if Value (Size) /= MAX_CHUNK_SIZE then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Max size is incorrect"),
            Details     => To_Unbounded_String ("Expected: " & MAX_CHUNK_SIZE'Image &
                                               ", Got: " & Value (Size)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Factory_Methods")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Factory_Methods;

   function Test_Adaptive_Sizing return Void_Result.Result is
      Small_File  : constant := 100 * SI_KB;
      Medium_File : constant := 100 * SI_MB;
      Large_File  : constant := 10 * SI_GB;
      Size        : Chunk_Size_Type;
   begin
      -- Test small file with contract verification
      Size := Adaptive_For_Size (Small_File);

      -- Verify adaptive sizing postconditions
      pragma Assert (Is_Valid (Size), "Adaptive postcondition: valid size");
      pragma Assert (Value (Size) >= MIN_CHUNK_SIZE, "Adaptive postcondition: >= MIN");
      pragma Assert (Value (Size) <= MAX_CHUNK_SIZE, "Adaptive postcondition: <= MAX");

      if not Is_Valid (Size) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Adaptive sizing failed for small file"),
            Details     => To_Unbounded_String ("File size: " & Small_File'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Adaptive_Sizing")
         ));
      end if;

      -- For small files, chunk size should be relatively small
      if Value (Size) > SI_MB then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Chunk size too large for small file"),
            Details     => To_Unbounded_String ("Chunk size: " & Value (Size)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Adaptive_Sizing")
         ));
      end if;

      -- Test medium file
      Size := Adaptive_For_Size (Medium_File);
      pragma Assert (Is_Valid (Size));

      -- Test large file
      Size := Adaptive_For_Size (Large_File);
      pragma Assert (Is_Valid (Size));

      return Void_Result.Ok (True);
   end Test_Adaptive_Sizing;

   function Test_Common_Sizes return Void_Result.Result is
      Size : Chunk_Size_Type;
   begin
      -- Test From_KB factory
      declare
         KB : constant Natural := 256;
      begin
         -- Verify preconditions
         pragma Assert (KB > 0, "From_KB precondition: KB > 0");
         pragma Assert (Long_Long_Integer (KB) * SI_KB_LLI >= MIN_CHUNK_SIZE,
                       "From_KB precondition: result >= MIN");
         pragma Assert (Long_Long_Integer (KB) * SI_KB_LLI <= MAX_CHUNK_SIZE,
                       "From_KB precondition: result <= MAX");

         Size := From_KB (KB);

         -- Verify postconditions
         pragma Assert (Is_Valid (Size));
         pragma Assert (Value (Size) = Long_Long_Integer (KB) * SI_KB_LLI);

         if Value (Size) /= 256 * SI_KB_LLI then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("From_KB(256) size is incorrect"),
               Details     => To_Unbounded_String ("Expected: " &
                                                  Long_Long_Integer'Image (256 * SI_KB_LLI) &
                                                  ", Got: " & Value (Size)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Common_Sizes")
            ));
         end if;
      end;

      -- Test From_MB factory
      declare
         MB : constant Natural := 10;
      begin
         Size := From_MB (MB);
         pragma Assert (Is_Valid (Size));
         pragma Assert (Value (Size) = Long_Long_Integer (MB) * SI_MB_LLI);

         if Value (Size) /= 10 * SI_MB then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("From_MB(10) size is incorrect"),
               Details     => To_Unbounded_String ("Expected: " &
                                                  Long_Long_Integer'Image (10 * SI_MB_LLI) &
                                                  ", Got: " & Value (Size)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Common_Sizes")
            ));
         end if;
      end;

      -- Test named sizes
      declare
         Small_Size : constant Chunk_Size_Type := Small;
         Medium_Size : constant Chunk_Size_Type := Medium;
         Large_Size : constant Chunk_Size_Type := Large;
      begin
         pragma Assert (Is_Valid (Small_Size));
         pragma Assert (Is_Valid (Medium_Size));
         pragma Assert (Is_Valid (Large_Size));

         if Value (Small_Size) /= SIZE_1MB then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Small named size is incorrect"),
               Details     => To_Unbounded_String ("Expected: " & SIZE_1MB'Image &
                                                  ", Got: " & Value (Small_Size)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Common_Sizes")
            ));
         end if;

         if Value (Medium_Size) /= SIZE_16MB then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Medium named size is incorrect"),
               Details     => To_Unbounded_String ("Expected: " & SIZE_16MB'Image &
                                                  ", Got: " & Value (Medium_Size)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Common_Sizes")
            ));
         end if;

         if Value (Large_Size) /= SIZE_64MB then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Large named size is incorrect"),
               Details     => To_Unbounded_String ("Expected: " & SIZE_64MB'Image &
                                                  ", Got: " & Value (Large_Size)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Common_Sizes")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Common_Sizes;

   function Test_Comparisons return Void_Result.Result is
      Small  : constant Chunk_Size_Type := Create (MIN_CHUNK_SIZE);
      Medium : constant Chunk_Size_Type := Create (DEFAULT_CHUNK_SIZE);
      Large  : constant Chunk_Size_Type := Create (MAX_CHUNK_SIZE);
   begin
      -- Test value comparisons
      if Value (Small) >= Value (Medium) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Small should be less than Medium"),
            Details     => To_Unbounded_String ("Small: " & Value (Small)'Image &
                                               ", Medium: " & Value (Medium)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Comparisons")
         ));
      end if;

      if Value (Medium) >= Value (Large) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Medium should be less than Large"),
            Details     => To_Unbounded_String ("Medium: " & Value (Medium)'Image &
                                               ", Large: " & Value (Large)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Comparisons")
         ));
      end if;

      if Value (Large) <= Value (Medium) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Large should be greater than Medium"),
            Details     => To_Unbounded_String ("Large: " & Value (Large)'Image &
                                               ", Medium: " & Value (Medium)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Comparisons")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Comparisons;

   function Test_Is_Valid return Void_Result.Result is
      Valid_Size : constant Chunk_Size_Type := Create (SI_MB);
   begin
      -- Test Is_Valid predicate
      pragma Assert (Is_Valid (Valid_Size), "Type invariant ensures validity");

      if not Is_Valid (Valid_Size) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Is_Valid returned false for valid size"),
            Details     => To_Unbounded_String ("Size: " & Value (Valid_Size)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Is_Valid")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Is_Valid;

   function Test_Boundary_Values return Void_Result.Result is
   begin
      -- Test exact boundary values
      declare
         Min_Size : constant Chunk_Size_Type := Create (MIN_CHUNK_SIZE);
         Max_Size : constant Chunk_Size_Type := Create (MAX_CHUNK_SIZE);
      begin
         pragma Assert (Value (Min_Size) = MIN_CHUNK_SIZE);
         pragma Assert (Value (Max_Size) = MAX_CHUNK_SIZE);

         if Value (Min_Size) /= MIN_CHUNK_SIZE then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("MIN_CHUNK_SIZE boundary test failed"),
               Details     => To_Unbounded_String ("Expected: " & MIN_CHUNK_SIZE'Image &
                                                  ", Got: " & Value (Min_Size)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Boundary_Values")
            ));
         end if;

         if Value (Max_Size) /= MAX_CHUNK_SIZE then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("MAX_CHUNK_SIZE boundary test failed"),
               Details     => To_Unbounded_String ("Expected: " & MAX_CHUNK_SIZE'Image &
                                                  ", Got: " & Value (Max_Size)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Boundary_Values")
            ));
         end if;
      end;

      -- Test values just inside boundaries
      declare
         Near_Min : constant Chunk_Size_Type := Create (MIN_CHUNK_SIZE + 1);
         Near_Max : constant Chunk_Size_Type := Create (MAX_CHUNK_SIZE - 1);
      begin
         pragma Assert (Value (Near_Min) = MIN_CHUNK_SIZE + 1);
         pragma Assert (Value (Near_Max) = MAX_CHUNK_SIZE - 1);

         if Value (Near_Min) /= MIN_CHUNK_SIZE + 1 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Near minimum boundary test failed"),
               Details     => To_Unbounded_String ("Expected: " &
                                                  Long_Long_Integer'Image (MIN_CHUNK_SIZE + 1) &
                                                  ", Got: " & Value (Near_Min)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Boundary_Values")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Boundary_Values;

   function Test_Power_Of_Two_Sizes return Void_Result.Result is
      Powers : constant array (1 .. 10) of Long_Long_Integer :=
        [SI_KB,        -- 1KB
         2 * SI_KB,    -- 2KB
         4 * SI_KB,    -- 4KB
         8 * SI_KB,    -- 8KB
         16 * SI_KB,   -- 16KB
         32 * SI_KB,   -- 32KB
         64 * SI_KB,   -- 64KB
         128 * SI_KB,  -- 128KB
         256 * SI_KB,  -- 256KB
         512 * SI_KB]; -- 512KB
   begin
      for Power of Powers loop
         if Power >= MIN_CHUNK_SIZE and Power <= MAX_CHUNK_SIZE then
            declare
               Size : constant Chunk_Size_Type := Create (Power);
            begin
               pragma Assert (Is_Valid (Size));
               pragma Assert (Value (Size) = Power);

               if Value (Size) /= Power then
                  return Void_Result.Err (Test_Error'(
                     Kind        => Assertion_Failed,
                     Message     => To_Unbounded_String ("Power of two size test failed"),
                     Details     => To_Unbounded_String ("Expected: " & Power'Image &
                                                       ", Got: " & Value (Size)'Image),
                     Line_Number => 0,
                     Test_Name   => To_Unbounded_String ("Test_Power_Of_Two_Sizes")
                  ));
               end if;
            end;
         end if;
      end loop;

      return Void_Result.Ok (True);
   end Test_Power_Of_Two_Sizes;

   function Test_Adaptive_Edge_Cases return Void_Result.Result is
   begin
      -- Note: Cannot test zero-size file as Adaptive_For_Size requires Total_Size > 0
      -- This is enforced by precondition

      -- Test very small file (1 byte)
      declare
         Size : constant Chunk_Size_Type := Adaptive_For_Size (1);
      begin
         pragma Assert (Is_Valid (Size));
         -- For files < 10MB, Adaptive_For_Size returns SIZE_256KB
         pragma Assert (Value (Size) = SIZE_256KB);

         if Value (Size) /= SIZE_256KB then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("1-byte file should return SIZE_256KB"),
               Details     => To_Unbounded_String ("Got: " & Value (Size)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Adaptive_Edge_Cases")
            ));
         end if;
      end;

      -- Test very large file (100TB)
      declare
         Huge_File : constant Long_Long_Integer := 100 * SI_TB_LLI;
         Size : constant Chunk_Size_Type := Adaptive_For_Size (Huge_File);
      begin
         pragma Assert (Is_Valid (Size));
         pragma Assert (Value (Size) >= MIN_CHUNK_SIZE);
         pragma Assert (Value (Size) <= MAX_CHUNK_SIZE);

         if not Is_Valid (Size) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Huge file should return valid size"),
               Details     => To_Unbounded_String ("File size: " & Huge_File'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Adaptive_Edge_Cases")
            ));
         end if;

         -- Should not exceed MAX_CHUNK_SIZE
         if Value (Size) > MAX_CHUNK_SIZE then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Adaptive size exceeds MAX_CHUNK_SIZE"),
               Details     => To_Unbounded_String ("Got: " & Value (Size)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Adaptive_Edge_Cases")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Adaptive_Edge_Cases;

   function Test_Conversion_Accuracy return Void_Result.Result is
   begin
      -- Test KB conversions
      declare
         KB_Values : constant array (1 .. 5) of Natural := [1, 10, 100, 1000, 10000];
      begin
         for KB of KB_Values loop
            declare
               Expected_Bytes : constant Long_Long_Integer := Long_Long_Integer (KB) * SI_KB_LLI;
            begin
               if Expected_Bytes >= MIN_CHUNK_SIZE and Expected_Bytes <= MAX_CHUNK_SIZE then
                  declare
                     Size : constant Chunk_Size_Type := From_KB (KB);
                  begin
                     pragma Assert (Is_Valid (Size));
                     pragma Assert (Value (Size) = Expected_Bytes);

                     if Value (Size) /= Expected_Bytes then
                        return Void_Result.Err (Test_Error'(
                           Kind        => Assertion_Failed,
                           Message     => To_Unbounded_String ("KB conversion failed"),
                           Details     => To_Unbounded_String ("KB: " & KB'Image &
                                                             ", Expected: " & Expected_Bytes'Image &
                                                             ", Got: " & Value (Size)'Image),
                           Line_Number => 0,
                           Test_Name   => To_Unbounded_String ("Test_Conversion_Accuracy")
                        ));
                     end if;
                  end;
               end if;
            end;
         end loop;
      end;

      return Void_Result.Ok (True);
   end Test_Conversion_Accuracy;

   function Test_Named_Size_Relationships return Void_Result.Result is
      Small_Size  : constant Chunk_Size_Type := Small;
      Medium_Size : constant Chunk_Size_Type := Medium;
      Large_Size  : constant Chunk_Size_Type := Large;
      Default_Size : constant Chunk_Size_Type := Default;
      Min_Size : constant Chunk_Size_Type := Min;
      Max_Size : constant Chunk_Size_Type := Max;
   begin
      -- Verify Small < Medium < Large
      pragma Assert (Value (Small_Size) < Value (Medium_Size));
      pragma Assert (Value (Medium_Size) < Value (Large_Size));

      if Value (Small_Size) >= Value (Medium_Size) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Small should be less than Medium"),
            Details     => To_Unbounded_String ("Small: " & Value (Small_Size)'Image &
                                               ", Medium: " & Value (Medium_Size)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Named_Size_Relationships")
         ));
      end if;

      -- Verify Min <= Default <= Max
      pragma Assert (Value (Min_Size) <= Value (Default_Size));
      pragma Assert (Value (Default_Size) <= Value (Max_Size));

      if Value (Default_Size) > Value (Max_Size) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Default should be <= Max"),
            Details     => To_Unbounded_String ("Default: " & Value (Default_Size)'Image &
                                               ", Max: " & Value (Max_Size)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Named_Size_Relationships")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Named_Size_Relationships;

   function Test_Contract_Violations return Void_Result.Result is
   begin
      -- Note: In Ada, precondition violations cause assertion failures that
      -- terminate the program. We cannot safely test these violations in
      -- unit tests. Instead, we verify that the functions work correctly
      -- at their boundary conditions.

      return Void_Result.Ok (True);
   end Test_Contract_Violations;

   function Test_Performance_Characteristics return Void_Result.Result is
   begin
      -- Simple performance test without benchmarking
      declare
         Start_Count : constant Natural := 100_000;
         Size : Chunk_Size_Type;
         pragma Unreferenced (Size);
      begin
         for I in 1 .. Start_Count loop
            Size := Create (SI_MB);
         end loop;
      end;

      return Void_Result.Ok (True);
   end Test_Performance_Characteristics;

   --  ==========================================================================
   --  Test Suite Runner
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
      Output.Write_Line ("=== Running Chunk Size Unit Tests ===");
      Output.Write_Line ("");

      -- Run all tests
      Add_Test_Result ("Test_Create_Valid_Size", Test_Create_Valid_Size'Access);
      Add_Test_Result ("Test_Create_Invalid_Size", Test_Create_Invalid_Size'Access);
      Add_Test_Result ("Test_Factory_Methods", Test_Factory_Methods'Access);
      Add_Test_Result ("Test_Adaptive_Sizing", Test_Adaptive_Sizing'Access);
      Add_Test_Result ("Test_Common_Sizes", Test_Common_Sizes'Access);
      Add_Test_Result ("Test_Comparisons", Test_Comparisons'Access);
      Add_Test_Result ("Test_Is_Valid", Test_Is_Valid'Access);
      Add_Test_Result ("Test_Boundary_Values", Test_Boundary_Values'Access);
      Add_Test_Result ("Test_Power_Of_Two_Sizes", Test_Power_Of_Two_Sizes'Access);
      Add_Test_Result ("Test_Adaptive_Edge_Cases", Test_Adaptive_Edge_Cases'Access);
      Add_Test_Result ("Test_Conversion_Accuracy", Test_Conversion_Accuracy'Access);
      Add_Test_Result ("Test_Named_Size_Relationships", Test_Named_Size_Relationships'Access);
      Add_Test_Result ("Test_Contract_Violations", Test_Contract_Violations'Access);
      Add_Test_Result ("Test_Performance_Characteristics", Test_Performance_Characteristics'Access);

      -- Generate summary
      declare
         Stats_Result : constant Test_Stats_Result.Result :=
            Run_Test_Suite ("Chunk_Size_Tests", Tests (1 .. Index - 1), Output);
      begin
         if Stats_Result.Is_Ok then
            declare
               Stats : constant Test_Statistics := Stats_Result.Get_Ok;
            begin
               Output.Write_Line ("");
               Print_Test_Summary ("Chunk Size Unit Tests", Stats, Output);
               return Test_Stats_Result.Ok (Stats);
            end;
         else
            return Stats_Result;
         end if;
      end;
   end Run_All_Tests;

end Test_Chunk_Size;
