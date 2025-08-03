--  =============================================================================
--  Pipelib.Core.Domain.Entities.Chunk - Chunk Entity Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Unchecked_Deallocation;

package body Pipelib.Core.Domain.Entities.Chunk is

   procedure Free is new Ada.Unchecked_Deallocation (
      Stream_Element_Array,
      Stream_Element_Array_Access);

   --  Create a new chunk
   function Create (
      Number : Natural;
      Size   : Pipelib.Core.Domain.Value_Objects.Chunk_Size.Chunk_Size_Type
   ) return Chunk_Type is
   begin
      return (
         Number         => Number,
         State          => Created,
         Data           => null,
         Data_Size      => 0,
         Allocated_Size => Size,
         Is_Compressed  => False,
         Original_Size  => 0,
         Retry_Count    => 0
      );
   end Create;

   --  Set state with validation
   procedure Set_State (Chunk : in out Chunk_Type; State : Chunk_State) is
   begin
      Chunk.State := State;
   end Set_State;

   --  Set data with ownership transfer
   procedure Set_Data (
      Chunk : in out Chunk_Type;
      Data  : in out Stream_Element_Array_Access) is
   begin
      --  Free existing data if any
      if Chunk.Data /= null then
         Free (Chunk.Data);
      end if;

      --  Transfer ownership
      Chunk.Data := Data;
      Data := null;
   end Set_Data;

   --  Set data size
   procedure Set_Data_Size (Chunk : in out Chunk_Type; Size : Natural) is
   begin
      Chunk.Data_Size := Size;
   end Set_Data_Size;

   --  Set compression flag
   procedure Set_Compressed (Chunk : in out Chunk_Type; Compressed : Boolean) is
   begin
      Chunk.Is_Compressed := Compressed;
   end Set_Compressed;

   --  Set original size
   procedure Set_Original_Size (Chunk : in out Chunk_Type; Size : Natural) is
   begin
      Chunk.Original_Size := Size;
   end Set_Original_Size;

   --  Increment retry count
   procedure Increment_Retry_Count (Chunk : in out Chunk_Type) is
   begin
      Chunk.Retry_Count := Chunk.Retry_Count + 1;
   end Increment_Retry_Count;

   --  Validation
   function Is_Valid (Chunk : Chunk_Type) return Boolean is
   begin
      return
         --  Valid state
         Chunk.State'Valid and

         --  Data consistency
         (if Chunk.Data /= null then Chunk.Data_Size > 0) and
         (if Chunk.Data = null then Chunk.Data_Size = 0) and

         --  Size constraints
         Long_Long_Integer (Chunk.Data_Size) <=
           Pipelib.Core.Domain.Value_Objects.Chunk_Size.Value (Chunk.Allocated_Size) and

         --  Compression consistency
         (if Chunk.Is_Compressed then Chunk.Original_Size > 0);
   end Is_Valid;

   --  State transition validation
   function Is_Valid_Transition (From, To : Chunk_State) return Boolean is
   begin
      return (case From is
         when Created    => To in Reading | Read,  -- Can skip reading if data pre-loaded
         when Reading    => To = Read,
         when Read       => To in Processing | Writing,  -- Can skip processing
         when Processing => To = Processed,
         when Processed  => To = Writing,
         when Writing    => To = Written,
         when Written    => To = Created);  -- Can be reset for reuse
   end Is_Valid_Transition;

   --  Reset chunk for reuse
   procedure Reset (Chunk : in out Chunk_Type) is
   begin
      --  Free data if allocated
      if Chunk.Data /= null then
         Free (Chunk.Data);
      end if;

      --  Reset all fields except Number and Allocated_Size
      Chunk.State := Created;
      Chunk.Data := null;
      Chunk.Data_Size := 0;
      Chunk.Is_Compressed := False;
      Chunk.Original_Size := 0;
      Chunk.Retry_Count := 0;
   end Reset;

end Pipelib.Core.Domain.Entities.Chunk;
