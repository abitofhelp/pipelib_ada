--  =============================================================================
--  Pipelib.Infrastructure.IO.Memory_Mapped_Chunk_Adapter - Memory-Mapped File Chunk Adapter
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Adapter for creating File_Chunk objects from memory-mapped files with
--  zero-copy access for optimal performance with large files.
--  =============================================================================
pragma Ada_2022;

with System.Storage_Elements;
with Pipelib.Core.Domain.Ports.Memory_Mapped_File_Interface;
with Pipelib.Core.Domain.Value_Objects.File_Chunk;
with Pipelib.Core.Domain.Value_Objects.File_Chunk.Vectors;
with Abohlib.Core.Domain.Result;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Pipelib.Infrastructure.IO.Memory_Mapped_Chunk_Adapter is

   use System.Storage_Elements;
   use Pipelib.Core.Domain.Ports.Memory_Mapped_File_Interface;
   use Pipelib.Core.Domain.Value_Objects.File_Chunk;
   use Pipelib.Core.Domain.Value_Objects.File_Chunk.Vectors;

   --  Configuration for chunk creation from memory-mapped files
   type Chunk_Config is record
      Default_Chunk_Size : Positive := 64 * 1024; -- 64KB default
      Calculate_Checksums : Boolean := True;
      Use_Sequential_Access : Boolean := True; -- Hint for OS optimization
   end record;

   --  Result type for chunk creation operations
   package Chunk_Vector_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type  => File_Chunk_Vector,
      Err_Type => Unbounded_String);

   --  Create chunks from memory-mapped file with zero-copy access
   function Create_Chunks_From_Memory_Map
     (Map : Memory_Mapped_File_Interface'Class;
      Config : Chunk_Config := (others => <>);
      Start_Offset : Storage_Count := 0;
      Max_Bytes : Storage_Count := 0) -- 0 means entire file
     return Chunk_Vector_Result.Result
   with Pre => Map.Is_Mapped;
   --  Creates File_Chunk objects from memory-mapped data without copying
   --  Uses zero-copy access by creating Stream_Element_Array_Access that
   --  points directly to the memory-mapped region

   --  Create a single chunk from memory-mapped file region
   function Create_Single_Chunk_From_Memory_Map
     (Map : Memory_Mapped_File_Interface'Class;
      Sequence_Number : Natural;
      Offset : Storage_Count;
      Length : Storage_Count;
      Is_Final : Boolean;
      Calculate_Checksum : Boolean := True)
     return File_Chunk_Type
   with Pre => Map.Is_Mapped and then
               Offset + Length <= Map.Get_Size and then
               Length > 0;
   --  Creates a single File_Chunk from a specific region of memory-mapped data

   --  Utility function to determine optimal chunk size based on file size
   function Calculate_Optimal_Chunk_Size
     (File_Size : Storage_Count;
      Available_Memory : Storage_Count := 0) -- 0 means auto-detect
     return Positive;
   --  Calculates optimal chunk size based on file size and available memory
   --  Uses adaptive algorithms similar to the Rust implementation

   --  Check if memory mapping should be used for a file
   function Should_Use_Memory_Mapping_For_File
     (File_Size : Storage_Count;
      Available_Memory : Storage_Count := 0) -- 0 means auto-detect
     return Boolean;
   --  Determines if memory mapping is beneficial for the given file size

private

   --  Internal function to create Stream_Element_Array_Access from memory view
   function Create_Stream_Array_Access_From_Memory
     (View : Memory_View;
      Offset : Storage_Count;
      Length : Storage_Count)
     return Pipelib.Core.Domain.Value_Objects.File_Chunk.Stream_Element_Array_Access
   with Pre => Offset + Length <= View.Size;
   --  Creates a Stream_Element_Array_Access that points to memory-mapped data
   --  This enables zero-copy access to the file data

   --  Internal function to convert memory address to stream elements
   function Memory_To_Stream_Elements
     (Address : System.Address;
      Length : Storage_Count)
     return Pipelib.Core.Domain.Value_Objects.File_Chunk.Stream_Element_Array_Access;
   --  Converts memory address and length to Stream_Element_Array_Access

end Pipelib.Infrastructure.IO.Memory_Mapped_Chunk_Adapter;
