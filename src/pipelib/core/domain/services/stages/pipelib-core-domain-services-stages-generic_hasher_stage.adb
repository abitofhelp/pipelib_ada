--   =============================================================================
--   Pipelib.Core.Domain.Services.Stages.Generic_Hasher_Stage - Implementation
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;

package body Pipelib.Core.Domain.Services.Stages.Generic_Hasher_Stage is

   use Abohlib.Core.Domain.Services.SHA256_Hasher;

   -- ----------
   --  Create
   -- ----------

   function Create return Hasher_Stage_Type is
   begin
      return
        (Ada.Finalization.Controlled
         with
           Hasher           => Create,
           Chunks_Processed => 0,
           Bytes_Hashed     => 0,
           Is_Finalized     => False,
           Final_Hash       => Null_Unbounded_String);
   end Create;

   -- -----------------
   --  Process_Chunk
   -- -----------------

   function Process_Chunk
     (Stage : in out Hasher_Stage_Type; Chunk : File_Chunk_Type)
      return Chunk_Result.Result is
   begin
      if Stage.Is_Finalized then
         return
           Chunk_Result.Err (To_Unbounded_String ("Hasher already finalized"));
      end if;

      --  Update the hash with chunk data using direct access to avoid stack copy
      declare
         Chunk_Data : constant Stream_Element_Array_Access :=
           Data_Access (Chunk);
      begin
         Update (Stage.Hasher, Chunk_Data.all);
      end;

      --  Update statistics
      Stage.Chunks_Processed := Stage.Chunks_Processed + 1;
      Stage.Bytes_Hashed :=
        Stage.Bytes_Hashed + Long_Long_Integer (Data_Length (Chunk));

      --  Pass the chunk through unchanged
      return Chunk_Result.Ok (Chunk);
   end Process_Chunk;

   -- -----------------
   --  Finalize_Hash
   -- -----------------

   function Finalize_Hash
     (Stage : in out Hasher_Stage_Type) return Hash_Result.Result is
   begin
      if Stage.Is_Finalized then
         return
           Hash_Result.Err (To_Unbounded_String ("Hasher already finalized"));
      end if;

      --  Get the final hash from the SHA256 hasher
      declare
         SHA_Result :
           constant Abohlib
                      .Core
                      .Domain
                      .Services
                      .SHA256_Hasher
                      .Hash_Result
                      .Result := Finalize_Hash (Stage.Hasher);
      begin
         if Abohlib.Core.Domain.Services.SHA256_Hasher.Hash_Result.Is_Ok
              (SHA_Result)
         then
            Stage.Final_Hash :=
              Abohlib.Core.Domain.Services.SHA256_Hasher.Hash_Result.Get_Ok
                (SHA_Result);
            Stage.Is_Finalized := True;
            return Hash_Result.Ok (Stage.Final_Hash);
         else
            return
              Hash_Result.Err
                (Abohlib.Core.Domain.Services.SHA256_Hasher.Hash_Result.Get_Err
                   (SHA_Result));
         end if;
      end;
   end Finalize_Hash;

   -- --------------------
   --  Chunks_Processed
   -- --------------------

   function Chunks_Processed (Stage : Hasher_Stage_Type) return Natural is
   begin
      return Stage.Chunks_Processed;
   end Chunks_Processed;

   -- ----------------
   --  Bytes_Hashed
   -- ----------------

   function Bytes_Hashed (Stage : Hasher_Stage_Type) return Long_Long_Integer
   is
   begin
      return Stage.Bytes_Hashed;
   end Bytes_Hashed;

   -- ----------------
   --  Is_Finalized
   -- ----------------

   function Is_Finalized (Stage : Hasher_Stage_Type) return Boolean is
   begin
      return Stage.Is_Finalized;
   end Is_Finalized;

   -- ----------------
   --  Current_Hash
   -- ----------------

   function Current_Hash (Stage : Hasher_Stage_Type) return String is
   begin
      if Stage.Is_Finalized then
         return To_String (Stage.Final_Hash);
      else
         return "";
      end if;
   end Current_Hash;

   -- ---------
   --  Reset
   -- ---------

   procedure Reset (Stage : in out Hasher_Stage_Type) is
   begin
      Stage.Hasher := Abohlib.Core.Domain.Services.SHA256_Hasher.Create;
      Stage.Chunks_Processed := 0;
      Stage.Bytes_Hashed := 0;
      Stage.Is_Finalized := False;
      Stage.Final_Hash := Null_Unbounded_String;
   end Reset;

   -- ---------
   --  Image
   -- ---------

   function Image (Stage : Hasher_Stage_Type) return String is
   begin
      return
        Stage_Name
        & "[chunks="
        & Stage.Chunks_Processed'Image
        & ", bytes="
        & Stage.Bytes_Hashed'Image
        & ", finalized="
        & Stage.Is_Finalized'Image
        & "]";
   end Image;

   -- --------------
   --  Initialize
   -- --------------

   overriding
   procedure Initialize (Stage : in out Hasher_Stage_Type) is
   begin
      Stage.Hasher := Abohlib.Core.Domain.Services.SHA256_Hasher.Create;
      Stage.Chunks_Processed := 0;
      Stage.Bytes_Hashed := 0;
      Stage.Is_Finalized := False;
      Stage.Final_Hash := Null_Unbounded_String;
   end Initialize;

   -- ------------
   --  Finalize
   -- ------------

   overriding
   procedure Finalize (Stage : in out Hasher_Stage_Type) is
   begin
      null;  -- SHA256_Hasher_Type handles its own cleanup
   end Finalize;

   -- ----------
   --  Adjust
   -- ----------

   overriding
   procedure Adjust (Stage : in out Hasher_Stage_Type) is
   begin
      --  SHA256_Hasher_Type handles its own copying
      --  Just ensure our state is consistent
      null;
   end Adjust;

end Pipelib.Core.Domain.Services.Stages.Generic_Hasher_Stage;
