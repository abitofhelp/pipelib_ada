--  =============================================================================
--  Pipelib.Core.Domain.Constants.Count_Arithmetic - Arithmetic Operations for Count Types
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  ## Arithmetic Operations for Pipeline Count Types
--
--  This package provides comprehensive arithmetic operations for pipeline count types,
--  enabling natural mathematical operations while maintaining type safety.
--  These operations eliminate the need for explicit type conversions in client code.
--
--  ### Design Principles
--
--  **Type Safety**: All operations preserve the specific count type, preventing
--  accidental mixing of different kinds of counts (e.g., read counts with error counts).
--
--  **Zero Runtime Overhead**: All operations are inline expression functions
--  that compile to the same machine code as direct arithmetic operations.
--
--  **Natural Usage**: Operations follow standard Ada operator precedence and
--  associativity rules, making expressions intuitive and readable.
--
--  ### Supported Operations
--
--  Each count type supports:
--  * Addition with same type and Natural values
--  * Subtraction with same type and Natural values
--  * Multiplication by Natural values
--  * Division by Natural values with truncation
--  * Modulo operations
--  * All comparison operators (<, <=, >, >=, =, /=)
--
--  ### Usage Examples
--
--  ```ada
--  -- Before (requires explicit conversions):
--  Chunks_Read := Read_Count_Type(Natural(Chunks_Read) + 1);
--  Total := Read_Count_Type(Natural(Count1) + Natural(Count2));
--
--  -- After (with arithmetic operations):
--  Chunks_Read := Chunks_Read + 1;
--  Total := Count1 + Count2;
--
--  -- Complex expressions work naturally:
--  Average := Total_Read / Batch_Count;
--  Remaining := Total_Count - Processed_Count;
--  Is_Half_Done := Processed_Count >= Total_Count / 2;
--  ```
--
--  =============================================================================

pragma Ada_2022;

package Pipelib.Core.Domain.Constants.Count_Arithmetic is
   pragma Preelaborate;

   --  =========================================================================
   --  Read_Count_Type Arithmetic Operations
   --  =========================================================================

   --  Addition operations
   function "+" (Left, Right : Read_Count_Type) return Read_Count_Type is
     (Read_Count_Type (Natural (Left) + Natural (Right)))
   with Inline;

   function "+" (Left : Read_Count_Type; Right : Natural) return Read_Count_Type is
     (Read_Count_Type (Natural (Left) + Right))
   with Inline;

   function "+" (Left : Natural; Right : Read_Count_Type) return Read_Count_Type is
     (Read_Count_Type (Left + Natural (Right)))
   with Inline;

   --  Subtraction operations
   function "-" (Left, Right : Read_Count_Type) return Read_Count_Type is
     (Read_Count_Type (Natural (Left) - Natural (Right)))
   with Inline, Pre => Left >= Right;

   function "-" (Left : Read_Count_Type; Right : Natural) return Read_Count_Type is
     (Read_Count_Type (Natural (Left) - Right))
   with Inline, Pre => Natural (Left) >= Right;

   --  Multiplication operations
   function "*" (Left, Right : Read_Count_Type) return Read_Count_Type is
     (Read_Count_Type (Natural (Left) * Natural (Right)))
   with Inline;

   function "*" (Left : Read_Count_Type; Right : Natural) return Read_Count_Type is
     (Read_Count_Type (Natural (Left) * Right))
   with Inline;

   function "*" (Left : Natural; Right : Read_Count_Type) return Read_Count_Type is
     (Read_Count_Type (Left * Natural (Right)))
   with Inline;

   --  Division operations
   function "/" (Left, Right : Read_Count_Type) return Read_Count_Type is
     (Read_Count_Type (Natural (Left) / Natural (Right)))
   with Inline, Pre => Right > 0;

   function "/" (Left : Read_Count_Type; Right : Natural) return Read_Count_Type is
     (Read_Count_Type (Natural (Left) / Right))
   with Inline, Pre => Right > 0;

   --  Modulo operations
   function "mod" (Left, Right : Read_Count_Type) return Read_Count_Type is
     (Read_Count_Type (Natural (Left) mod Natural (Right)))
   with Inline, Pre => Right > 0;

   function "mod" (Left : Read_Count_Type; Right : Natural) return Read_Count_Type is
     (Read_Count_Type (Natural (Left) mod Right))
   with Inline, Pre => Right > 0;

   --  Comparison operations
   function "<" (Left, Right : Read_Count_Type) return Boolean is
     (Natural (Left) < Natural (Right))
   with Inline;

   function "<=" (Left, Right : Read_Count_Type) return Boolean is
     (Natural (Left) <= Natural (Right))
   with Inline;

   function ">" (Left, Right : Read_Count_Type) return Boolean is
     (Natural (Left) > Natural (Right))
   with Inline;

   function ">=" (Left, Right : Read_Count_Type) return Boolean is
     (Natural (Left) >= Natural (Right))
   with Inline;

   --  =========================================================================
   --  Processed_Count_Type Arithmetic Operations
   --  =========================================================================

   --  Addition operations
   function "+" (Left, Right : Processed_Count_Type) return Processed_Count_Type is
     (Processed_Count_Type (Natural (Left) + Natural (Right)))
   with Inline;

   function "+" (Left : Processed_Count_Type; Right : Natural) return Processed_Count_Type is
     (Processed_Count_Type (Natural (Left) + Right))
   with Inline;

   function "+" (Left : Natural; Right : Processed_Count_Type) return Processed_Count_Type is
     (Processed_Count_Type (Left + Natural (Right)))
   with Inline;

   --  Subtraction operations
   function "-" (Left, Right : Processed_Count_Type) return Processed_Count_Type is
     (Processed_Count_Type (Natural (Left) - Natural (Right)))
   with Inline, Pre => Left >= Right;

   function "-" (Left : Processed_Count_Type; Right : Natural) return Processed_Count_Type is
     (Processed_Count_Type (Natural (Left) - Right))
   with Inline, Pre => Natural (Left) >= Right;

   --  Multiplication operations
   function "*" (Left, Right : Processed_Count_Type) return Processed_Count_Type is
     (Processed_Count_Type (Natural (Left) * Natural (Right)))
   with Inline;

   function "*" (Left : Processed_Count_Type; Right : Natural) return Processed_Count_Type is
     (Processed_Count_Type (Natural (Left) * Right))
   with Inline;

   function "*" (Left : Natural; Right : Processed_Count_Type) return Processed_Count_Type is
     (Processed_Count_Type (Left * Natural (Right)))
   with Inline;

   --  Division operations
   function "/" (Left, Right : Processed_Count_Type) return Processed_Count_Type is
     (Processed_Count_Type (Natural (Left) / Natural (Right)))
   with Inline, Pre => Right > 0;

   function "/" (Left : Processed_Count_Type; Right : Natural) return Processed_Count_Type is
     (Processed_Count_Type (Natural (Left) / Right))
   with Inline, Pre => Right > 0;

   --  Modulo operations
   function "mod" (Left, Right : Processed_Count_Type) return Processed_Count_Type is
     (Processed_Count_Type (Natural (Left) mod Natural (Right)))
   with Inline, Pre => Right > 0;

   function "mod" (Left : Processed_Count_Type; Right : Natural) return Processed_Count_Type is
     (Processed_Count_Type (Natural (Left) mod Right))
   with Inline, Pre => Right > 0;

   --  Comparison operations
   function "<" (Left, Right : Processed_Count_Type) return Boolean is
     (Natural (Left) < Natural (Right))
   with Inline;

   function "<=" (Left, Right : Processed_Count_Type) return Boolean is
     (Natural (Left) <= Natural (Right))
   with Inline;

   function ">" (Left, Right : Processed_Count_Type) return Boolean is
     (Natural (Left) > Natural (Right))
   with Inline;

   function ">=" (Left, Right : Processed_Count_Type) return Boolean is
     (Natural (Left) >= Natural (Right))
   with Inline;

   --  =========================================================================
   --  Written_Count_Type Arithmetic Operations
   --  =========================================================================

   --  Addition operations
   function "+" (Left, Right : Written_Count_Type) return Written_Count_Type is
     (Written_Count_Type (Natural (Left) + Natural (Right)))
   with Inline;

   function "+" (Left : Written_Count_Type; Right : Natural) return Written_Count_Type is
     (Written_Count_Type (Natural (Left) + Right))
   with Inline;

   function "+" (Left : Natural; Right : Written_Count_Type) return Written_Count_Type is
     (Written_Count_Type (Left + Natural (Right)))
   with Inline;

   --  Subtraction operations
   function "-" (Left, Right : Written_Count_Type) return Written_Count_Type is
     (Written_Count_Type (Natural (Left) - Natural (Right)))
   with Inline, Pre => Left >= Right;

   function "-" (Left : Written_Count_Type; Right : Natural) return Written_Count_Type is
     (Written_Count_Type (Natural (Left) - Right))
   with Inline, Pre => Natural (Left) >= Right;

   --  Multiplication operations
   function "*" (Left, Right : Written_Count_Type) return Written_Count_Type is
     (Written_Count_Type (Natural (Left) * Natural (Right)))
   with Inline;

   function "*" (Left : Written_Count_Type; Right : Natural) return Written_Count_Type is
     (Written_Count_Type (Natural (Left) * Right))
   with Inline;

   function "*" (Left : Natural; Right : Written_Count_Type) return Written_Count_Type is
     (Written_Count_Type (Left * Natural (Right)))
   with Inline;

   --  Division operations
   function "/" (Left, Right : Written_Count_Type) return Written_Count_Type is
     (Written_Count_Type (Natural (Left) / Natural (Right)))
   with Inline, Pre => Right > 0;

   function "/" (Left : Written_Count_Type; Right : Natural) return Written_Count_Type is
     (Written_Count_Type (Natural (Left) / Right))
   with Inline, Pre => Right > 0;

   --  Modulo operations
   function "mod" (Left, Right : Written_Count_Type) return Written_Count_Type is
     (Written_Count_Type (Natural (Left) mod Natural (Right)))
   with Inline, Pre => Right > 0;

   function "mod" (Left : Written_Count_Type; Right : Natural) return Written_Count_Type is
     (Written_Count_Type (Natural (Left) mod Right))
   with Inline, Pre => Right > 0;

   --  Comparison operations
   function "<" (Left, Right : Written_Count_Type) return Boolean is
     (Natural (Left) < Natural (Right))
   with Inline;

   function "<=" (Left, Right : Written_Count_Type) return Boolean is
     (Natural (Left) <= Natural (Right))
   with Inline;

   function ">" (Left, Right : Written_Count_Type) return Boolean is
     (Natural (Left) > Natural (Right))
   with Inline;

   function ">=" (Left, Right : Written_Count_Type) return Boolean is
     (Natural (Left) >= Natural (Right))
   with Inline;

   --  =========================================================================
   --  Error_Count_Type Arithmetic Operations
   --  =========================================================================

   --  Addition operations
   function "+" (Left, Right : Error_Count_Type) return Error_Count_Type is
     (Error_Count_Type (Natural (Left) + Natural (Right)))
   with Inline;

   function "+" (Left : Error_Count_Type; Right : Natural) return Error_Count_Type is
     (Error_Count_Type (Natural (Left) + Right))
   with Inline;

   function "+" (Left : Natural; Right : Error_Count_Type) return Error_Count_Type is
     (Error_Count_Type (Left + Natural (Right)))
   with Inline;

   --  Subtraction operations
   function "-" (Left, Right : Error_Count_Type) return Error_Count_Type is
     (Error_Count_Type (Natural (Left) - Natural (Right)))
   with Inline, Pre => Left >= Right;

   function "-" (Left : Error_Count_Type; Right : Natural) return Error_Count_Type is
     (Error_Count_Type (Natural (Left) - Right))
   with Inline, Pre => Natural (Left) >= Right;

   --  Multiplication operations
   function "*" (Left, Right : Error_Count_Type) return Error_Count_Type is
     (Error_Count_Type (Natural (Left) * Natural (Right)))
   with Inline;

   function "*" (Left : Error_Count_Type; Right : Natural) return Error_Count_Type is
     (Error_Count_Type (Natural (Left) * Right))
   with Inline;

   function "*" (Left : Natural; Right : Error_Count_Type) return Error_Count_Type is
     (Error_Count_Type (Left * Natural (Right)))
   with Inline;

   --  Division operations
   function "/" (Left, Right : Error_Count_Type) return Error_Count_Type is
     (Error_Count_Type (Natural (Left) / Natural (Right)))
   with Inline, Pre => Right > 0;

   function "/" (Left : Error_Count_Type; Right : Natural) return Error_Count_Type is
     (Error_Count_Type (Natural (Left) / Right))
   with Inline, Pre => Right > 0;

   --  Modulo operations
   function "mod" (Left, Right : Error_Count_Type) return Error_Count_Type is
     (Error_Count_Type (Natural (Left) mod Natural (Right)))
   with Inline, Pre => Right > 0;

   function "mod" (Left : Error_Count_Type; Right : Natural) return Error_Count_Type is
     (Error_Count_Type (Natural (Left) mod Right))
   with Inline, Pre => Right > 0;

   --  Comparison operations
   function "<" (Left, Right : Error_Count_Type) return Boolean is
     (Natural (Left) < Natural (Right))
   with Inline;

   function "<=" (Left, Right : Error_Count_Type) return Boolean is
     (Natural (Left) <= Natural (Right))
   with Inline;

   function ">" (Left, Right : Error_Count_Type) return Boolean is
     (Natural (Left) > Natural (Right))
   with Inline;

   function ">=" (Left, Right : Error_Count_Type) return Boolean is
     (Natural (Left) >= Natural (Right))
   with Inline;

   --  =========================================================================
   --  Chunk_Count_Type Arithmetic Operations
   --  =========================================================================

   --  Addition operations
   function "+" (Left, Right : Chunk_Count_Type) return Chunk_Count_Type is
     (Chunk_Count_Type (Natural (Left) + Natural (Right)))
   with Inline;

   function "+" (Left : Chunk_Count_Type; Right : Natural) return Chunk_Count_Type is
     (Chunk_Count_Type (Natural (Left) + Right))
   with Inline;

   function "+" (Left : Natural; Right : Chunk_Count_Type) return Chunk_Count_Type is
     (Chunk_Count_Type (Left + Natural (Right)))
   with Inline;

   --  Subtraction operations
   function "-" (Left, Right : Chunk_Count_Type) return Chunk_Count_Type is
     (Chunk_Count_Type (Natural (Left) - Natural (Right)))
   with Inline, Pre => Left >= Right;

   function "-" (Left : Chunk_Count_Type; Right : Natural) return Chunk_Count_Type is
     (Chunk_Count_Type (Natural (Left) - Right))
   with Inline, Pre => Natural (Left) >= Right;

   --  Multiplication operations
   function "*" (Left, Right : Chunk_Count_Type) return Chunk_Count_Type is
     (Chunk_Count_Type (Natural (Left) * Natural (Right)))
   with Inline;

   function "*" (Left : Chunk_Count_Type; Right : Natural) return Chunk_Count_Type is
     (Chunk_Count_Type (Natural (Left) * Right))
   with Inline;

   function "*" (Left : Natural; Right : Chunk_Count_Type) return Chunk_Count_Type is
     (Chunk_Count_Type (Left * Natural (Right)))
   with Inline;

   --  Division operations
   function "/" (Left, Right : Chunk_Count_Type) return Chunk_Count_Type is
     (Chunk_Count_Type (Natural (Left) / Natural (Right)))
   with Inline, Pre => Right > 0;

   function "/" (Left : Chunk_Count_Type; Right : Natural) return Chunk_Count_Type is
     (Chunk_Count_Type (Natural (Left) / Right))
   with Inline, Pre => Right > 0;

   --  Modulo operations
   function "mod" (Left, Right : Chunk_Count_Type) return Chunk_Count_Type is
     (Chunk_Count_Type (Natural (Left) mod Natural (Right)))
   with Inline, Pre => Right > 0;

   function "mod" (Left : Chunk_Count_Type; Right : Natural) return Chunk_Count_Type is
     (Chunk_Count_Type (Natural (Left) mod Right))
   with Inline, Pre => Right > 0;

   --  Comparison operations
   function "<" (Left, Right : Chunk_Count_Type) return Boolean is
     (Natural (Left) < Natural (Right))
   with Inline;

   function "<=" (Left, Right : Chunk_Count_Type) return Boolean is
     (Natural (Left) <= Natural (Right))
   with Inline;

   function ">" (Left, Right : Chunk_Count_Type) return Boolean is
     (Natural (Left) > Natural (Right))
   with Inline;

   function ">=" (Left, Right : Chunk_Count_Type) return Boolean is
     (Natural (Left) >= Natural (Right))
   with Inline;

   --  =========================================================================
   --  Convenience Functions
   --  =========================================================================

   --  Increment functions for common pattern: Count := Count + 1
   function Increment (Count : Read_Count_Type) return Read_Count_Type is
     (Read_Count_Type'(Count + Read_Count_Type'(1)))
   with Inline;

   function Increment (Count : Processed_Count_Type) return Processed_Count_Type is
     (Processed_Count_Type'(Count + Processed_Count_Type'(1)))
   with Inline;

   function Increment (Count : Written_Count_Type) return Written_Count_Type is
     (Written_Count_Type'(Count + Written_Count_Type'(1)))
   with Inline;

   function Increment (Count : Error_Count_Type) return Error_Count_Type is
     (Error_Count_Type'(Count + Error_Count_Type'(1)))
   with Inline;

   function Increment (Count : Chunk_Count_Type) return Chunk_Count_Type is
     (Chunk_Count_Type'(Count + Chunk_Count_Type'(1)))
   with Inline;

   --  Decrement functions for common pattern: Count := Count - 1
   function Decrement (Count : Read_Count_Type) return Read_Count_Type is
     (Read_Count_Type'(Count - Read_Count_Type'(1)))
   with Inline, Pre => Count > 0;

   function Decrement (Count : Processed_Count_Type) return Processed_Count_Type is
     (Processed_Count_Type'(Count - Processed_Count_Type'(1)))
   with Inline, Pre => Count > 0;

   function Decrement (Count : Written_Count_Type) return Written_Count_Type is
     (Written_Count_Type'(Count - Written_Count_Type'(1)))
   with Inline, Pre => Count > 0;

   function Decrement (Count : Error_Count_Type) return Error_Count_Type is
     (Error_Count_Type'(Count - Error_Count_Type'(1)))
   with Inline, Pre => Count > 0;

   function Decrement (Count : Chunk_Count_Type) return Chunk_Count_Type is
     (Chunk_Count_Type'(Count - Chunk_Count_Type'(1)))
   with Inline, Pre => Count > 0;

end Pipelib.Core.Domain.Constants.Count_Arithmetic;
