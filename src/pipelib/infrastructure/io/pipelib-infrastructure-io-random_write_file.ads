--   =============================================================================
--   Pipelib.Infrastructure.IO.Random_Write_File - Random Access File Writer
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Provides random write access to files for parallel chunk processing.
--   Allows multiple threads to write chunks at specific positions concurrently.
--   =============================================================================

pragma Ada_2022;

with Ada.Streams.Stream_IO;
with Ada.Finalization;
with Abohlib.Core.Domain.Value_Objects.File_Path;
with Pipelib.Core.Domain.Value_Objects.File_Chunk;
with Abohlib.Core.Domain.Result;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Pipelib.Infrastructure.IO.Random_Write_File is

   --  Type representing a random access file writer
   type Random_Write_File_Type is new Ada.Finalization.Limited_Controlled with private;
   type Random_Write_File_Access is access all Random_Write_File_Type;

   --  Result types
   package Write_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => Boolean,  -- Success indicator
      Err_Type => Unbounded_String);

   --  Create and open a file for random write access
   function Create
     (Path : Abohlib.Core.Domain.Value_Objects.File_Path.File_Path;
      Expected_Size : Long_Long_Integer := 0;  -- Pre-allocate if known
      Use_Temp_File : Boolean := True)  -- Write to temp file for safety
      return Random_Write_File_Access
     with Post => Create'Result /= null;

   --  Write a chunk at a specific position
   procedure Write_Chunk_At_Position
     (File : in out Random_Write_File_Type;
      Chunk : Pipelib.Core.Domain.Value_Objects.File_Chunk.File_Chunk_Type;
      Position : Long_Long_Integer)
     with Pre => Is_Open (File) and then Position >= 0;

   --  Write a chunk using its sequence number
   procedure Write_Chunk
     (File : in out Random_Write_File_Type;
      Chunk : Pipelib.Core.Domain.Value_Objects.File_Chunk.File_Chunk_Type)
     with Pre => Is_Open (File);

   --  Check if file is open
   function Is_Open (File : Random_Write_File_Type) return Boolean;

   --  Get current file size
   function Size (File : Random_Write_File_Type) return Long_Long_Integer
     with Pre => Is_Open (File);

   --  Commit the file (rename temp to final if using temp file)
   function Commit (File : in out Random_Write_File_Type) return Write_Result.Result
     with Post => not Is_Open (File);

   --  Rollback (delete temp file if using temp file)
   procedure Rollback (File : in out Random_Write_File_Type)
     with Post => not Is_Open (File);

   --  Close without committing
   procedure Close (File : in out Random_Write_File_Type)
     with Post => not Is_Open (File);

   --  Flush buffers to disk
   procedure Flush (File : in out Random_Write_File_Type)
     with Pre => Is_Open (File);

   --  Pre-allocate file space for better performance
   procedure Preallocate
     (File : in out Random_Write_File_Type;
      Size : Long_Long_Integer)
     with Pre => Is_Open (File) and then Size > 0;

   --  Destructor
   procedure Destroy (File : in out Random_Write_File_Access);

   --  Thread-safe wrapper for concurrent access
   protected type Protected_Random_Write_File is
      --  Initialize with file access
      procedure Initialize (File : Random_Write_File_Access);

      --  Thread-safe write operations
      procedure Write_Chunk (Chunk : Pipelib.Core.Domain.Value_Objects.File_Chunk.File_Chunk_Type);
      procedure Write_At_Position
        (Chunk : Pipelib.Core.Domain.Value_Objects.File_Chunk.File_Chunk_Type;
         Position : Long_Long_Integer);

      --  Query operations
      function Get_Size return Long_Long_Integer;
      function Is_Ready return Boolean;

   private
      File_Access : Random_Write_File_Access := null;
   end Protected_Random_Write_File;

   type Protected_Random_Write_File_Access is access Protected_Random_Write_File;

private

   type Random_Write_File_Type is new Ada.Finalization.Limited_Controlled with record
      File_Handle : Ada.Streams.Stream_IO.File_Type;
      File_Path   : Abohlib.Core.Domain.Value_Objects.File_Path.File_Path;
      Temp_Path   : Abohlib.Core.Domain.Value_Objects.File_Path.File_Path;
      Is_Open_Flag : Boolean := False;
      Use_Temp     : Boolean := True;
      Chunk_Size   : Natural := 0;  -- For position calculation
   end record;

   overriding procedure Finalize (File : in out Random_Write_File_Type);

end Pipelib.Infrastructure.IO.Random_Write_File;
