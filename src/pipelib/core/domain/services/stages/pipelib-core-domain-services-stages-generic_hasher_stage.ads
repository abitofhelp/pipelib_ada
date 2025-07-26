--   =============================================================================
--   Pipelib.Core.Domain.Services.Stages.Generic_Hasher_Stage - Generic Pipeline Hasher Stage
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Generic hasher stage for pipeline processing that maintains a running SHA256
--   hash of data chunks passing through. Can be instantiated for various purposes
--   within different pipeline implementations.
--   =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Finalization;
with Ada.Streams;           use Ada.Streams;
with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Services.Generic_SHA256_Hasher;
with Pipelib.Core.Domain.Value_Objects.File_Chunk;

generic
   Stage_Purpose : String;  -- e.g., "Input", "Output", "Verification"
package Pipelib.Core.Domain.Services.Stages.Generic_Hasher_Stage is

   use Pipelib.Core.Domain.Value_Objects.File_Chunk;

   --  Type for the hasher stage
   type Hasher_Stage_Type is new Ada.Finalization.Controlled with private;

   --  Result types for error handling
   package Chunk_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => File_Chunk_Type,
        Err_Type => Unbounded_String);

   package Hash_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Unbounded_String,  -- SHA256 hash as hex string
        Err_Type => Unbounded_String);

   --  Constructor
   function Create return Hasher_Stage_Type;
   --  Creates a new hasher stage

   --  Process a chunk and update the hash
   function Process_Chunk
     (Stage : in out Hasher_Stage_Type; Chunk : File_Chunk_Type)
      return Chunk_Result.Result
   with
     Pre => not Stage.Is_Finalized,
     Post => Stage.Chunks_Processed > Stage.Chunks_Processed'Old;
   --  Updates the running hash with the chunk data and passes the chunk through

   --  Get the final hash
   function Finalize_Hash
     (Stage : in out Hasher_Stage_Type) return Hash_Result.Result
   with Pre => not Stage.Is_Finalized, Post => Stage.Is_Finalized;
   --  Finalizes and returns the SHA256 hash of all processed chunks

   --  Query methods
   function Stage_Name return String
   is (Stage_Purpose & "Hasher");
   --  Returns the stage name

   function Chunks_Processed (Stage : Hasher_Stage_Type) return Natural
   with Inline;
   --  Returns the number of chunks processed

   function Bytes_Hashed (Stage : Hasher_Stage_Type) return Long_Long_Integer
   with Inline;
   --  Returns the total number of bytes hashed

   function Is_Finalized (Stage : Hasher_Stage_Type) return Boolean
   with Inline;
   --  Returns True if the hash has been finalized

   function Current_Hash (Stage : Hasher_Stage_Type) return String
   with
     Post =>
       (if not Stage.Is_Finalized then Current_Hash'Result = ""
        else Current_Hash'Result'Length = 64);
   --  Returns the current hash value (empty if not finalized)

   --  Reset the hasher for reuse
   procedure Reset (Stage : in out Hasher_Stage_Type)
   with
     Post =>
       not Stage.Is_Finalized
       and then Stage.Chunks_Processed = 0
       and then Stage.Bytes_Hashed = 0;
   --  Resets the hasher to initial state

   --  String representation
   function Image (Stage : Hasher_Stage_Type) return String;

private
   --  For simplicity, we'll use a dummy type and instantiate the generic
   --  We'll only use the Update_Stream procedure in the implementation
   type Dummy_Type is null record;

   pragma Warnings (Off, "formal parameter ""Data"" is not referenced");
   function Dummy_To_Stream (Data : Dummy_Type) return Stream_Element_Array
   is (Stream_Element_Array'(1 .. 0 => 0));
   pragma Warnings (On, "formal parameter ""Data"" is not referenced");

   package SHA256 is new
     Abohlib.Core.Domain.Services.Generic_SHA256_Hasher
       (Data_Type          => Dummy_Type,
        To_Stream_Elements => Dummy_To_Stream);

   type Hasher_Stage_Type is new Ada.Finalization.Controlled with record
      Hasher           : SHA256.SHA256_Hasher_Type;
      Chunks_Processed : Natural := 0;
      Bytes_Hashed     : Long_Long_Integer := 0;
      Is_Finalized     : Boolean := False;
      Final_Hash       : Unbounded_String;
   end record;

   overriding
   procedure Initialize (Stage : in out Hasher_Stage_Type);
   overriding
   procedure Finalize (Stage : in out Hasher_Stage_Type);
   overriding
   procedure Adjust (Stage : in out Hasher_Stage_Type);

end Pipelib.Core.Domain.Services.Stages.Generic_Hasher_Stage;
