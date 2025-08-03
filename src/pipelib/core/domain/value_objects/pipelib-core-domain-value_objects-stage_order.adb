--   =============================================================================
--   Pipelib.Core.Domain.Value_Objects.Stage_Order - Implementation
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;

package body Pipelib.Core.Domain.Value_Objects.Stage_Order is

   -- ----------
   --  Create
   -- ----------

   function Create (Order : Positive) return Stage_Order_Type is
   begin
      return (Order_Value => Order);
   end Create;

   -- ---------
   --  Value
   -- ---------

   function Value (Self : Stage_Order_Type) return Positive is
   begin
      return Self.Order_Value;
   end Value;

   -- --------
   --  Next
   -- --------

   function Next (Self : Stage_Order_Type) return Stage_Order_Type is
   begin
      return Create (Self.Order_Value + 1);
   end Next;

   -- ------------
   --  Previous
   -- ------------

   function Previous (Self : Stage_Order_Type) return Stage_Order_Type is
   begin
      return Create (Self.Order_Value - 1);
   end Previous;

   -- ---------
   --  First
   -- ---------

   function First return Stage_Order_Type is
   begin
      return Create (1);
   end First;

   -- -----
   --  "="
   -- -----

   overriding
   function "=" (Left, Right : Stage_Order_Type) return Boolean is
   begin
      return Left.Order_Value = Right.Order_Value;
   end "=";

   -- --------------------
   --  Create_Sequence
   -- --------------------

   function Create_Sequence (Count : Positive) return Stage_Order_Array is
      Result : Stage_Order_Array (1 .. Count);
   begin
      for I in Result'Range loop
         Result (I) := Create (I);
      end loop;
      return Result;
   end Create_Sequence;

end Pipelib.Core.Domain.Value_Objects.Stage_Order;
