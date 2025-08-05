--   =============================================================================
--   Pipelib.Core.Domain.Value_Objects.Worker_Count - Worker Count Value Object
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Value object representing the number of worker threads/tasks for
--   parallel pipeline processing.
--   =============================================================================

pragma Ada_2022;

with Pipelib.Core.Domain.Constants;

package Pipelib.Core.Domain.Value_Objects.Worker_Count is

   --  Type representing worker count with constraints
   subtype Worker_Count_Type is Positive range 1 .. Positive (Pipelib.Core.Domain.Constants.Max_Worker_Count);

   --  Default worker count (single-threaded)
   Default_Worker_Count : constant Worker_Count_Type := 1;

   --  Constructor with validation
   function Create (Value : Natural) return Worker_Count_Type
   with Pre => Value > 0 and then Value <= Natural (Pipelib.Core.Domain.Constants.Max_Worker_Count),
        Post => Create'Result = Value;

   --  Get optimal worker count based on system
   function Get_Optimal_Count return Worker_Count_Type;

   --  Get count based on CPU cores with multiplier
   function Get_CPU_Based_Count
     (Multiplier : Positive := 1) return Worker_Count_Type
   with
     Pre => Multiplier <= Natural (Pipelib.Core.Domain.Constants.Max_Worker_Multiplier),
     Post =>
       Get_CPU_Based_Count'Result >= 1
       and then Get_CPU_Based_Count'Result <= Natural (Pipelib.Core.Domain.Constants.Max_Worker_Count);

   --  Get optimal worker count based on file size (empirically optimized)
   function Optimal_For_File_Size
     (File_Size : Long_Long_Integer) return Worker_Count_Type
   with
     Post =>
       Optimal_For_File_Size'Result >= 1
       and then Optimal_For_File_Size'Result <= Natural (Pipelib.Core.Domain.Constants.Max_Worker_Count);

   --  Get optimal worker count based on processing type
   function Optimal_For_Processing_Type
     (File_Size        : Long_Long_Integer;
      Available_CPUs   : Natural;
      Is_CPU_Intensive : Boolean) return Worker_Count_Type
   with
     Pre => Available_CPUs > 0,
     Post =>
       Optimal_For_Processing_Type'Result >= 1
       and then Optimal_For_Processing_Type'Result <= Natural (Pipelib.Core.Domain.Constants.Max_Worker_Count);

   --  Image function for debugging
   function Image (Count : Worker_Count_Type) return String
   is (Count'Image);

end Pipelib.Core.Domain.Value_Objects.Worker_Count;
