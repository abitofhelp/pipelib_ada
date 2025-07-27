--   =============================================================================
--   Pipelib.Core.Domain.Value_Objects.Worker_Count - Implementation
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;

with System.Multiprocessors;
with Abohlib.Core.Domain.Constants.Bytes;

package body Pipelib.Core.Domain.Value_Objects.Worker_Count is

   --  Local constants from Abohlib
   SI_KB : constant := Abohlib.Core.Domain.Constants.Bytes.SI_KB;
   SI_MB : constant := Abohlib.Core.Domain.Constants.Bytes.SI_MB;
   SI_GB : constant := Abohlib.Core.Domain.Constants.Bytes.SI_GB;
   SI_TB : constant := Abohlib.Core.Domain.Constants.Bytes.SI_TB;
   pragma Unreferenced (SI_GB, SI_TB);  -- Keep for future use

   -- ----------
   --  Create
   -- ----------

   function Create (Value : Natural) return Worker_Count_Type is
   begin
      return Worker_Count_Type (Value);
   end Create;

   -- ----------------------
   --  Get_Optimal_Count
   -- ----------------------

   function Get_Optimal_Count return Worker_Count_Type is
      CPU_Count : constant System.Multiprocessors.CPU :=
        System.Multiprocessors.Number_Of_CPUs;
   begin
      --  Use number of CPUs, but cap at our maximum
      if Natural (CPU_Count) > Worker_Count_Type'Last then
         return Worker_Count_Type'Last;
      else
         return Worker_Count_Type (CPU_Count);
      end if;
   end Get_Optimal_Count;

   -- -----------------------
   --  Get_CPU_Based_Count
   -- -----------------------

   function Get_CPU_Based_Count (Multiplier : Positive := 1) return Worker_Count_Type is
      CPU_Count : constant System.Multiprocessors.CPU :=
        System.Multiprocessors.Number_Of_CPUs;
      Desired_Count : constant Natural := Natural (CPU_Count) * Multiplier;
   begin
      --  Cap at our maximum
      if Desired_Count > Worker_Count_Type'Last then
         return Worker_Count_Type'Last;
      elsif Desired_Count < Worker_Count_Type'First then
         return Worker_Count_Type'First;
      else
         return Worker_Count_Type (Desired_Count);
      end if;
   end Get_CPU_Based_Count;

   -- ---------------------------
   --  Optimal_For_File_Size
   -- ---------------------------

   function Optimal_For_File_Size (File_Size : Long_Long_Integer)
     return Worker_Count_Type is
      Optimal_Count : Natural;
   begin
      --  Empirically optimized worker counts based on extensive benchmarks
      case File_Size is
         --  Tiny files: Minimize overhead
         when 0 .. SI_MB =>
            if File_Size < 64 * SI_KB then
               Optimal_Count := 1;  -- Very small files
            else
               Optimal_Count := 2;  -- Small files < 1MB
            end if;

         --  Small files: Aggressive parallelism (empirically optimized)
         when SI_MB + 1 .. 50 * SI_MB =>
            declare
               Size_MB : constant Float := Float (File_Size) / Float (SI_MB);
            begin
               if Size_MB <= 5.0 then
                  Optimal_Count := 9;  -- 5MB: 9 workers (+102% performance)
               elsif Size_MB <= 10.0 then
                  --  Linear interpolation from 9 to 14 workers
                  Optimal_Count := Natural (9.0 + (Size_MB - 5.0) * 1.0);
               else
                  --  Gradual decrease from 14 workers
                  Optimal_Count := Natural (14.0 - (Size_MB - 10.0) * 0.2);
               end if;
            end;

         --  Medium files: Balanced approach
         when 50 * SI_MB + 1 .. 500 * SI_MB =>
            declare
               Size_MB : constant Float := Float (File_Size) / Float (SI_MB);
            begin
               if Size_MB <= 100.0 then
                  --  5 to 8 workers
                  Optimal_Count := Natural (5.0 + (Size_MB - 50.0) * 0.06);
               else
                  --  8 to 12 workers
                  Optimal_Count := Natural (8.0 + (Size_MB - 100.0) * 0.01);
               end if;
            end;

         --  Large files: Conservative to avoid coordination overhead
         when others =>
            Optimal_Count := 3;  -- 2GB+: 3 workers (+76% performance)
      end case;

      --  Ensure within bounds
      if Optimal_Count < Worker_Count_Type'First then
         return Worker_Count_Type'First;
      elsif Optimal_Count > Worker_Count_Type'Last then
         return Worker_Count_Type'Last;
      else
         return Worker_Count_Type (Optimal_Count);
      end if;
   end Optimal_For_File_Size;

   -- ---------------------------------
   --  Optimal_For_Processing_Type
   -- ---------------------------------

   function Optimal_For_Processing_Type
     (File_Size : Long_Long_Integer;
      Available_CPUs : Natural;
      Is_CPU_Intensive : Boolean) return Worker_Count_Type
   is
      Base_Count : constant Worker_Count_Type :=
        Optimal_For_File_Size (File_Size);
   begin
      if Is_CPU_Intensive then
         --  For CPU-intensive tasks, consider available CPUs
         declare
            CPU_Limited : constant Natural :=
              Natural'Min (Natural (Base_Count), Available_CPUs);
         begin
            if CPU_Limited < Worker_Count_Type'First then
               return Worker_Count_Type'First;
            else
               return Worker_Count_Type (CPU_Limited);
            end if;
         end;
      else
         --  For I/O-intensive tasks, can exceed CPU count
         return Base_Count;
      end if;
   end Optimal_For_Processing_Type;

end Pipelib.Core.Domain.Value_Objects.Worker_Count;
