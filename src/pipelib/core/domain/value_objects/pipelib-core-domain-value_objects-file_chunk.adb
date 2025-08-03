--   =============================================================================
--   Pipelib.Core.Domain.Value_Objects.File_Chunk - Implementation
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;

with Ada.Numerics.Discrete_Random;
with Ada.Unchecked_Deallocation;
with Interfaces;
with SHA2;

package body Pipelib.Core.Domain.Value_Objects.File_Chunk is

   use type Chunk_Size.Chunk_Size_Type;

   --  Package for generating random bytes for UUID
   package Random_Byte is new
     Ada.Numerics.Discrete_Random (Interfaces.Unsigned_8);
   Random_Gen : Random_Byte.Generator;

   --  Procedure to free stream element arrays
   --  Note: With Ada 2022 ownership, explicit deallocation is still needed
   --  in Finalize to ensure deterministic cleanup
   procedure Free is new
     Ada.Unchecked_Deallocation
       (Stream_Element_Array,
        Stream_Element_Array_Access);

   --  Generate a simple UUID v4
   function Generate_UUID return String is
      Result    : String (1 .. 36) := "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx";
      Hex_Chars : constant String := "0123456789abcdef";
   begin
      for I in Result'Range loop
         case Result (I) is
            when 'x' =>
               Result (I) :=
                 Hex_Chars
                   (Natural (Random_Byte.Random (Random_Gen)) mod 16 + 1);

            when 'y' =>
               Result (I) :=
                 Hex_Chars
                   ((Natural (Random_Byte.Random (Random_Gen)) mod 4) + 8 + 1);

            when others =>
               null; -- Keep dashes and '4'
         end case;
      end loop;
      return Result;
   end Generate_UUID;

   -- ----------
   --  Create
   -- ----------

   function Create
     (Sequence_Number : Natural;
      Offset          : Long_Long_Integer;
      Data            : Stream_Element_Array;
      Is_Final        : Boolean) return File_Chunk_Type is
   begin
      if Data'Length = 0 then
         raise File_Chunk_Error with EMPTY_DATA_ERROR;
      end if;

      if Data'Length > MAX_CHUNK_DATA_SIZE then
         raise File_Chunk_Error with SIZE_EXCEEDED_ERROR;
      end if;

      declare
         --  Allocate new memory for chunk data
         --  Ada 2022: The access type has implicit ownership
         New_Data : constant Stream_Element_Array_Access :=
           new Stream_Element_Array'(Data);
         Size     : constant Chunk_Size.Chunk_Size_Type :=
           Chunk_Size.Create (Data'Length);
         Chunk    : constant File_Chunk_Type :=
           (Ada.Finalization.Controlled
            with
              Id              => To_Unbounded_String (Generate_UUID),
              Sequence_Number => Sequence_Number,
              Offset          => Offset,
              Size            => Size,
              Data            => New_Data,
              Checksum        => Null_Unbounded_String,
              Is_Final        => Is_Final,
              Created_At      => Ada.Calendar.Clock);
      begin
         return Chunk;
      end;
   end Create;

   -- ----------------------
   --  Create_From_Access
   -- ----------------------

   function Create_From_Access
     (Sequence_Number : Natural;
      Offset          : Long_Long_Integer;
      Data            : not null Stream_Element_Array_Access;
      Is_Final        : Boolean) return File_Chunk_Type is
   begin
      if Data.all'Length = 0 then
         raise File_Chunk_Error with EMPTY_DATA_ERROR;
      end if;

      if Data.all'Length > MAX_CHUNK_DATA_SIZE then
         raise File_Chunk_Error with SIZE_EXCEEDED_ERROR;
      end if;

      declare
         Size  : constant Chunk_Size.Chunk_Size_Type :=
           Chunk_Size.Create (Data.all'Length);
         Chunk : constant File_Chunk_Type :=
           (Ada.Finalization.Controlled
            with
              Id              => To_Unbounded_String (Generate_UUID),
              Sequence_Number => Sequence_Number,
              Offset          => Offset,
              Size            => Size,
              Data            => Data,  -- Take ownership directly
              Checksum        => Null_Unbounded_String,
              Is_Final        => Is_Final,
              Created_At      => Ada.Calendar.Clock);
      begin
         return Chunk;
      end;
   end Create_From_Access;

   -- ------------------------
   --  Create_With_Checksum
   -- ------------------------

   function Create_With_Checksum
     (Sequence_Number : Natural;
      Offset          : Long_Long_Integer;
      Data            : Stream_Element_Array;
      Checksum        : String;
      Is_Final        : Boolean) return File_Chunk_Type
   is
      Base_Chunk : constant File_Chunk_Type :=
        Create (Sequence_Number, Offset, Data, Is_Final);
   begin

      return With_Checksum (Base_Chunk, Checksum);
   end Create_With_Checksum;

   -- ----------------
   --  With_Checksum
   -- ----------------

   function With_Checksum
     (Chunk : File_Chunk_Type; Checksum : String) return File_Chunk_Type
   is
   begin
      if Checksum'Length /= 64 then
         raise File_Chunk_Error with INVALID_CHECKSUM_ERROR;
      end if;

      declare
         New_Data : constant Stream_Element_Array_Access :=
           new Stream_Element_Array'(Chunk.Data.all);
         New_Chunk : constant File_Chunk_Type :=
           (Ada.Finalization.Controlled
            with
              Id              => Chunk.Id,
              Sequence_Number => Chunk.Sequence_Number,
              Offset          => Chunk.Offset,
              Size            => Chunk.Size,
              Data            => New_Data,
              Checksum        => To_Unbounded_String (Checksum),
              Is_Final        => Chunk.Is_Final,
              Created_At      => Chunk.Created_At);
      begin
         return New_Chunk;
      end;
   end With_Checksum;

   -- ------------------------------
   --  Calculate_And_Set_Checksum
   -- ------------------------------

   function Calculate_And_Set_Checksum
     (Chunk : File_Chunk_Type) return File_Chunk_Type
   is
      Checksum_Calc : constant Checksum_Result.Result := Calculate_Checksum (Chunk.Data.all);
   begin
      if not Checksum_Result.Is_Ok (Checksum_Calc) then
         raise File_Chunk_Error with To_String (Checksum_Result.Get_Err (Checksum_Calc));
      end if;

      declare
         Checksum : constant String := To_String (Checksum_Result.Get_Ok (Checksum_Calc));
      begin
         return With_Checksum (Chunk, Checksum);
      end;
   end Calculate_And_Set_Checksum;

   -- -------------------
   --  Verify_Checksum
   -- -------------------

   function Verify_Checksum (Chunk : File_Chunk_Type) return Validation_Result.Result is
   begin
      if not Has_Checksum (Chunk) then
         return Validation_Result.Ok (True); -- No checksum to verify
      end if;

      declare
         Calculated_Result : constant Checksum_Result.Result := Calculate_Checksum (Chunk.Data.all);
      begin
         if not Checksum_Result.Is_Ok (Calculated_Result) then
            return Validation_Result.Err (Checksum_Result.Get_Err (Calculated_Result));
         end if;

         declare
            Calculated : constant String := To_String (Checksum_Result.Get_Ok (Calculated_Result));
            Is_Valid   : constant Boolean := Calculated = To_String (Chunk.Checksum);
         begin
            return Validation_Result.Ok (Is_Valid);
         end;
      end;
   end Verify_Checksum;

   -- --------------------
   --  Calculate_Checksum
   -- --------------------

   function Calculate_Checksum (Data : Stream_Element_Array) return Checksum_Result.Result is
   begin
      if Data'Length = 0 then
         return Checksum_Result.Err (To_Unbounded_String ("Cannot calculate checksum of empty data"));
      end if;

      declare
         Context : SHA2.SHA_256.Context := SHA2.SHA_256.Initialize;
      begin
         --  SHA2 works directly with Stream_Element_Array, no conversion needed!
         SHA2.SHA_256.Update (Context, Data);

         --  Get the digest
         declare
            Digest : constant SHA2.SHA_256.Digest := SHA2.SHA_256.Finalize (Context);
            Result : String (1 .. 64);  -- SHA256 produces 32 bytes = 64 hex chars
            Hex_Chars : constant String := "0123456789abcdef";
         begin
            --  Convert digest bytes to hex string
            for I in Digest'Range loop
               declare
                  Byte : constant Stream_Element := Digest (I);
                  High_Nibble : constant Natural := Natural (Byte / 16) + 1;
                  Low_Nibble : constant Natural := Natural (Byte mod 16) + 1;
                  Pos : constant Natural := 2 * Natural (I - Digest'First) + 1;
               begin
                  Result (Pos) := Hex_Chars (High_Nibble);
                  Result (Pos + 1) := Hex_Chars (Low_Nibble);
               end;
            end loop;
            return Checksum_Result.Ok (To_Unbounded_String (Result));
         end;
      end;
   end Calculate_Checksum;

   -- -------
   --  "="
   -- -------

   overriding
   function "=" (Left, Right : File_Chunk_Type) return Boolean is
   begin
      --  Compare all fields except Id and Created_At
      --  Id is unique per chunk, so we don't compare it for value equality
      return
        Left.Sequence_Number = Right.Sequence_Number
        and then Left.Offset = Right.Offset
        and then Left.Size = Right.Size
        and then Left.Data.all = Right.Data.all
        and then Left.Checksum = Right.Checksum
        and then Left.Is_Final = Right.Is_Final;
   end "=";

   -- ---------
   --  Image
   -- ---------

   function Image (Chunk : File_Chunk_Type) return String is
   begin
      return
        "FileChunk["
        & To_String (Chunk.Id)
        & ", seq="
        & Chunk.Sequence_Number'Image
        & ", offset="
        & Chunk.Offset'Image
        & ", size="
        & Pipelib.Core.Domain.Value_Objects.Chunk_Size.Value (Chunk.Size)'Image
        & ", final="
        & Chunk.Is_Final'Image
        & "]";
   end Image;

   -- ----------
   --  Adjust
   -- ----------

   overriding
   procedure Adjust (Object : in out File_Chunk_Type) is
   begin
      --  Deep copy the data array for value semantics
      --  Ada 2022: Each File_Chunk owns its data independently
      --  This ensures assignment creates a true copy, not shared data
      if Object.Data /= null then
         declare
            Old_Data : constant Stream_Element_Array_Access := Object.Data;
         begin
            Object.Data := new Stream_Element_Array'(Old_Data.all);
         end;
      end if;
   exception
      when others =>
         Object.Data := null;
         raise;
   end Adjust;

   -- ------------
   --  Finalize
   -- ------------

   overriding
   procedure Finalize (Object : in out File_Chunk_Type) is
   begin
      --  Explicit deallocation ensures deterministic cleanup
      --  Ada 2022: Even with ownership, we want immediate cleanup
      --  of large memory blocks (up to 512MB per chunk)
      if Object.Data /= null then
         Free (Object.Data);
      end if;
   exception
      when others =>
         --  Ensure we don't propagate exceptions from Finalize
         null;
   end Finalize;

   -- --------
   --  Move
   -- --------

   procedure Move (Target : out File_Chunk_Type; Source : in out File_Chunk_Type) is
   begin
      --  First finalize target to free any existing data
      if Target.Data /= null then
         Free (Target.Data);
      end if;

      --  Transfer all fields from Source to Target
      Target.Id := Source.Id;
      Target.Sequence_Number := Source.Sequence_Number;
      Target.Offset := Source.Offset;
      Target.Size := Source.Size;
      Target.Data := Source.Data;  -- Transfer ownership
      Target.Checksum := Source.Checksum;
      Target.Is_Final := Source.Is_Final;
      Target.Created_At := Source.Created_At;

      --  Clear source data pointer to prevent double-free
      Source.Data := null;
   end Move;

   --  =========================================================================
   --  Accessor Function Implementations
   --  =========================================================================

   function Id (Chunk : File_Chunk_Type) return String is
   begin
      return To_String (Chunk.Id);
   end Id;

   function Sequence_Number (Chunk : File_Chunk_Type) return Natural is
   begin
      return Chunk.Sequence_Number;
   end Sequence_Number;

   function Offset (Chunk : File_Chunk_Type) return Long_Long_Integer is
   begin
      return Chunk.Offset;
   end Offset;

   function Size (Chunk : File_Chunk_Type) return Chunk_Size.Chunk_Size_Type is
   begin
      return Chunk.Size;
   end Size;

   function Data (Chunk : File_Chunk_Type) return Stream_Element_Array is
   begin
      return Chunk.Data.all;
   end Data;

   function Data_Access (Chunk : File_Chunk_Type) return not null Stream_Element_Array_Access is
   begin
      return Chunk.Data;
   end Data_Access;

   function Checksum (Chunk : File_Chunk_Type) return String is
   begin
      return To_String (Chunk.Checksum);
   end Checksum;

   function Has_Checksum (Chunk : File_Chunk_Type) return Boolean is
   begin
      return Length (Chunk.Checksum) > 0;
   end Has_Checksum;

   function Is_Final (Chunk : File_Chunk_Type) return Boolean is
   begin
      return Chunk.Is_Final;
   end Is_Final;

   function Created_At (Chunk : File_Chunk_Type) return Ada.Calendar.Time is
   begin
      return Chunk.Created_At;
   end Created_At;

   function Data_Length (Chunk : File_Chunk_Type) return Natural is
   begin
      return Chunk.Data'Length;
   end Data_Length;

   function Is_Empty (Chunk : File_Chunk_Type) return Boolean is
   begin
      return Chunk.Data'Length = 0;
   end Is_Empty;

   function Is_Valid_Checksum (S : String) return Boolean is
   begin
      return S'Length = 64 and then
             (for all C of S => C in '0'..'9' | 'a'..'f' | 'A'..'F');
   end Is_Valid_Checksum;

begin
   --  Initialize random generator
   Random_Byte.Reset (Random_Gen);
end Pipelib.Core.Domain.Value_Objects.File_Chunk;
