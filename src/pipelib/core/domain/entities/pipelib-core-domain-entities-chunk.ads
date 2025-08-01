--  =============================================================================
--  Pipelib.Core.Domain.Entities.Chunk - Chunk Entity
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Represents a chunk of data being processed through the pipeline.
--  This is an entity with identity (chunk number) and mutable state.
--  =============================================================================

pragma Ada_2022;

with Ada.Streams; use Ada.Streams;
with Pipelib.Core.Domain.Value_Objects.Chunk_Size;

package Pipelib.Core.Domain.Entities.Chunk is

   --  Chunk states through the pipeline
   type Chunk_State is (Created, Reading, Read, Processing, Processed, Writing, Written);

   --  Chunk entity type
   type Chunk_Type is tagged private;

   --  Access types for zero-copy operations
   type Chunk_Access is access all Chunk_Type;
   type Stream_Element_Array_Access is access all Stream_Element_Array;

   --  Create a new chunk
   function Create (
      Number : Natural;
      Size   : Pipelib.Core.Domain.Value_Objects.Chunk_Size.Chunk_Size_Type
   ) return Chunk_Type;

   --  Identity (chunk number is the unique identifier)
   function Number (Chunk : Chunk_Type) return Natural
   with Inline;

   --  State management
   function State (Chunk : Chunk_Type) return Chunk_State
   with Inline;

   procedure Set_State (Chunk : in out Chunk_Type; State : Chunk_State)
   with Pre => Is_Valid_Transition (Chunk.State, State),
        Post => Chunk.State = State;

   --  Data management (zero-copy operations)
   function Data (Chunk : Chunk_Type) return Stream_Element_Array_Access
   with Inline;

   procedure Set_Data (
      Chunk : in out Chunk_Type;
      Data  : in out Stream_Element_Array_Access)
   with Pre => Data /= null,
        Post => Data = null;  -- Transfer ownership

   function Data_Size (Chunk : Chunk_Type) return Natural
   with Inline;

   procedure Set_Data_Size (Chunk : in out Chunk_Type; Size : Natural)
   with Post => Data_Size (Chunk) = Size;

   --  Compression info
   function Is_Compressed (Chunk : Chunk_Type) return Boolean
   with Inline;

   procedure Set_Compressed (Chunk : in out Chunk_Type; Compressed : Boolean)
   with Post => Is_Compressed (Chunk) = Compressed;

   function Original_Size (Chunk : Chunk_Type) return Natural
   with Inline;

   procedure Set_Original_Size (Chunk : in out Chunk_Type; Size : Natural)
   with Post => Original_Size (Chunk) = Size;

   --  Processing metrics
   function Retry_Count (Chunk : Chunk_Type) return Natural
   with Inline;

   procedure Increment_Retry_Count (Chunk : in out Chunk_Type)
   with Post => Retry_Count (Chunk) = Retry_Count (Chunk)'Old + 1;

   --  Validation
   function Is_Valid (Chunk : Chunk_Type) return Boolean;

   --  State transition validation
   function Is_Valid_Transition (From, To : Chunk_State) return Boolean;

   --  Reset chunk for reuse (object pooling support)
   procedure Reset (Chunk : in out Chunk_Type)
   with Post => State (Chunk) = Created and
                Data (Chunk) = null and
                Data_Size (Chunk) = 0;

private

   type Chunk_Type is tagged record
      --  Identity
      Number         : Natural := 0;

      --  State
      State          : Chunk_State := Created;

      --  Data
      Data           : Stream_Element_Array_Access := null;
      Data_Size      : Natural := 0;
      Allocated_Size : Pipelib.Core.Domain.Value_Objects.Chunk_Size.Chunk_Size_Type;

      --  Compression info
      Is_Compressed  : Boolean := False;
      Original_Size  : Natural := 0;

      --  Metrics
      Retry_Count    : Natural := 0;
   end record;

   function Number (Chunk : Chunk_Type) return Natural is (Chunk.Number);
   function State (Chunk : Chunk_Type) return Chunk_State is (Chunk.State);
   function Data (Chunk : Chunk_Type) return Stream_Element_Array_Access is (Chunk.Data);
   function Data_Size (Chunk : Chunk_Type) return Natural is (Chunk.Data_Size);
   function Is_Compressed (Chunk : Chunk_Type) return Boolean is (Chunk.Is_Compressed);
   function Original_Size (Chunk : Chunk_Type) return Natural is (Chunk.Original_Size);
   function Retry_Count (Chunk : Chunk_Type) return Natural is (Chunk.Retry_Count);

end Pipelib.Core.Domain.Entities.Chunk;
