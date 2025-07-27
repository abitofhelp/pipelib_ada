--   =============================================================================
--   Pipelib.Core.Domain.Value_Objects.Stage_Order - Stage Order Value Object
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Type-safe stage ordering for pipeline stage sequencing and execution order
--   management. Ensures stages execute in the correct sequence.
--   =============================================================================

pragma Ada_2022;

package Pipelib.Core.Domain.Value_Objects.Stage_Order is

   --  Stage order type
   type Stage_Order_Type is tagged private;

   --  Forward declaration for validation
   function Is_Valid (Self : Stage_Order_Type) return Boolean;

   --  Constructor with validation
   function Create (Order : Positive) return Stage_Order_Type
     with Post => Value (Create'Result) = Order;

   --  Get order value
   function Value (Self : Stage_Order_Type) return Positive
     with Inline;

   --  Get next order
   function Next (Self : Stage_Order_Type) return Stage_Order_Type
     with Post => Value (Next'Result) = Value (Self) + 1;

   --  Get previous order (with validation)
   function Previous (Self : Stage_Order_Type) return Stage_Order_Type
     with Pre => Value (Self) > 1,
          Post => Value (Previous'Result) = Value (Self) - 1;

   --  Create first stage order
   function First return Stage_Order_Type
     with Post => Value (First'Result) = 1;

   --  Check if this is the first stage
   function Is_First (Self : Stage_Order_Type) return Boolean is
     (Value (Self) = 1);

   --  Calculate distance between two orders
   function Distance (From, To : Stage_Order_Type) return Natural is
     (if Value (To) >= Value (From) then
        Value (To) - Value (From)
      else
        Value (From) - Value (To));

   --  Comparison operators
   function "<" (Left, Right : Stage_Order_Type) return Boolean is
     (Value (Left) < Value (Right));

   function "<=" (Left, Right : Stage_Order_Type) return Boolean is
     (Value (Left) <= Value (Right));

   function ">" (Left, Right : Stage_Order_Type) return Boolean is
     (Value (Left) > Value (Right));

   function ">=" (Left, Right : Stage_Order_Type) return Boolean is
     (Value (Left) >= Value (Right));

   --  Equality operator
   overriding function "=" (Left, Right : Stage_Order_Type) return Boolean;

   --  String representation
   function Image (Self : Stage_Order_Type) return String is
     ("Stage Order: " & Value (Self)'Image);

   --  Create a sequence of stage orders
   type Stage_Order_Array is array (Positive range <>) of Stage_Order_Type;

   function Create_Sequence (Count : Positive) return Stage_Order_Array
     with Post => Create_Sequence'Result'Length = Count and then
                  (for all I in Create_Sequence'Result'Range =>
                     Value (Create_Sequence'Result (I)) = I);

private
   type Stage_Order_Type is tagged record
      Order_Value : Positive;
   end record
     with Type_Invariant => Is_Valid (Stage_Order_Type);

   --  Validation function for type invariant
   function Is_Valid (Self : Stage_Order_Type) return Boolean is
     (True);  -- Always valid since Order_Value is Positive

end Pipelib.Core.Domain.Value_Objects.Stage_Order;
