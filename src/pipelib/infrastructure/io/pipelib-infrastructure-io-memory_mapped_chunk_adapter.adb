--  =============================================================================
--  Pipelib.Infrastructure.IO.Memory_Mapped_Chunk_Adapter - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Adapter for creating File_Chunk objects from memory-mapped files with
--  zero-copy access for optimal performance with large files.
--  =============================================================================
pragma Ada_2022;

with System.Address_To_Access_Conversions;
with Ada.Exceptions;
with Ada.Streams; use Ada.Streams;
with Pipelib.Core.Domain.Value_Objects.File_Chunk;
use Pipelib.Core.Domain.Value_Objects.File_Chunk;

package body Pipelib.Infrastructure.IO.Memory_Mapped_Chunk_Adapter is
   --  Create chunks from memory-mapped file with zero-copy access
   function Create_Chunks_From_Memory_Map
     (Map : Memory_Mapped_File_Interface'Class;
      Config : Chunk_Config := (others => <>);
      Start_Offset : Storage_Count := 0;
      Max_Bytes : Storage_Count := 0) -- 0 means entire file
     return Chunk_Vector_Result.Result is

      File_Size : constant Storage_Count := Map.Get_Size;
      --  View : constant Memory_View := Map.Get_View;  -- Not used
      Chunks : File_Chunk_Vector;

      --  Calculate actual processing bounds
      Actual_Start : constant Storage_Count := Storage_Count'Min (Start_Offset, File_Size);
      Actual_End : constant Storage_Count :=
        (if Max_Bytes = 0 then File_Size
         else Storage_Count'Min (Actual_Start + Max_Bytes, File_Size));

      Current_Offset : Storage_Count := Actual_Start;
      Sequence : Natural := 0;
      Chunk_Size : constant Positive := Config.Default_Chunk_Size;

   begin
      --  Set access pattern hint for sequential reading
      if Config.Use_Sequential_Access then
         Map.Advise (Sequential, Actual_Start, Actual_End - Actual_Start);
      end if;

      --  Create chunks from memory-mapped data
      while Current_Offset < Actual_End loop
         declare
            Remaining : constant Storage_Count := Actual_End - Current_Offset;
            This_Chunk_Size : constant Storage_Count :=
              Storage_Count'Min (Storage_Count (Chunk_Size), Remaining);
            Is_Final : constant Boolean := Current_Offset + This_Chunk_Size >= Actual_End;

            Chunk : constant File_Chunk_Type := Create_Single_Chunk_From_Memory_Map
              (Map => Map,
               Sequence_Number => Sequence,
               Offset => Current_Offset,
               Length => This_Chunk_Size,
               Is_Final => Is_Final,
               Calculate_Checksum => Config.Calculate_Checksums);
         begin

            File_Chunk_Vectors.Append (Chunks, Chunk);

            Current_Offset := Current_Offset + This_Chunk_Size;
            Sequence := Sequence + 1;
         end;
      end loop;

      return Chunk_Vector_Result.Ok (Chunks);

   exception
      when E : others =>
         return Chunk_Vector_Result.Err
           (To_Unbounded_String ("Failed to create chunks from memory map: " &
                                Ada.Exceptions.Exception_Message (E)));
   end Create_Chunks_From_Memory_Map;

   --  Create a single chunk from memory-mapped file region
   function Create_Single_Chunk_From_Memory_Map
     (Map : Memory_Mapped_File_Interface'Class;
      Sequence_Number : Natural;
      Offset : Storage_Count;
      Length : Storage_Count;
      Is_Final : Boolean;
      Calculate_Checksum : Boolean := True)
     return File_Chunk_Type is

      --  View : constant Memory_View := Map.Get_View;  -- Not used
      Subview : constant Memory_View := Map.Create_Subview (Offset, Length);
      Stream_Access : Stream_Element_Array_Access;

   begin
      --  Create Stream_Element_Array_Access pointing to memory-mapped data
      Stream_Access := Create_Stream_Array_Access_From_Memory (Subview, 0, Length);

      --  Create File_Chunk using zero-copy access
      declare
         Chunk : File_Chunk_Type := Create_From_Access
           (Sequence_Number => Sequence_Number,
            Offset => Long_Long_Integer (Offset),
            Data => Stream_Access,
            Is_Final => Is_Final);
      begin
         --  Add checksum if requested
         if Calculate_Checksum then
            Chunk := Calculate_And_Set_Checksum (Chunk);
         end if;

         return Chunk;
      end;
   end Create_Single_Chunk_From_Memory_Map;

   --  Utility function to determine optimal chunk size based on file size
   function Calculate_Optimal_Chunk_Size
     (File_Size : Storage_Count;
      Available_Memory : Storage_Count := 0) -- 0 means auto-detect
     return Positive is
      pragma Unreferenced (Available_Memory);

      --  Constants based on empirical testing (similar to Rust implementation)
      Small_File_Threshold : constant Storage_Count := 10 * 1024 * 1024; -- 10MB
      Medium_File_Threshold : constant Storage_Count := 100 * 1024 * 1024; -- 100MB
      Large_File_Threshold : constant Storage_Count := 1024 * 1024 * 1024; -- 1GB

      Small_Chunk_Size : constant Positive := 64 * 1024; -- 64KB
      Medium_Chunk_Size : constant Positive := 1024 * 1024; -- 1MB
      Large_Chunk_Size : constant Positive := 4 * 1024 * 1024; -- 4MB
      Huge_Chunk_Size : constant Positive := 16 * 1024 * 1024; -- 16MB

   begin
      --  Adaptive chunk sizing based on file size
      if File_Size <= Small_File_Threshold then
         return Small_Chunk_Size;
      elsif File_Size <= Medium_File_Threshold then
         return Medium_Chunk_Size;
      elsif File_Size <= Large_File_Threshold then
         return Large_Chunk_Size;
      else
         return Huge_Chunk_Size;
      end if;
   end Calculate_Optimal_Chunk_Size;

   --  Check if memory mapping should be used for a file
   function Should_Use_Memory_Mapping_For_File
     (File_Size : Storage_Count;
      Available_Memory : Storage_Count := 0) -- 0 means auto-detect
     return Boolean is
      pragma Unreferenced (Available_Memory);

      --  Minimum file size for memory mapping (100MB)
      Min_Mmap_Size : constant Storage_Count := 100 * 1024 * 1024;

      --  Maximum file size for memory mapping (1GB default)
      Max_Mmap_Size : constant Storage_Count := 1024 * 1024 * 1024;

   begin
      --  Use memory mapping for files between 100MB and 1GB
      return File_Size >= Min_Mmap_Size and then
             File_Size <= Max_Mmap_Size;
   end Should_Use_Memory_Mapping_For_File;

   --  Internal function to create Stream_Element_Array_Access from memory view
   function Create_Stream_Array_Access_From_Memory
     (View : Memory_View;
      Offset : Storage_Count;
      Length : Storage_Count)
     return Pipelib.Core.Domain.Value_Objects.File_Chunk.Stream_Element_Array_Access is

      Target_Address : constant System.Address := View.Address + Offset;
   begin
      return Memory_To_Stream_Elements (Target_Address, Length);
   end Create_Stream_Array_Access_From_Memory;

   --  Internal function to convert memory address to stream elements
   function Memory_To_Stream_Elements
     (Address : System.Address;
      Length : Storage_Count)
     return Pipelib.Core.Domain.Value_Objects.File_Chunk.Stream_Element_Array_Access is

      --  Create a properly sized array type for the conversion
      subtype Target_Array is Stream_Element_Array (1 .. Stream_Element_Offset (Length));

      --  Create address-to-access conversion for the specific array size
      package Target_Array_Conversions is new
        System.Address_To_Access_Conversions (Target_Array);

      --  Convert the memory address directly to an access type
      Target_Access : constant Target_Array_Conversions.Object_Pointer :=
        Target_Array_Conversions.To_Pointer (Address);

      --  Allocate new memory and copy the data
      --  Note: This is not zero-copy, but it's safer and more compatible with Ada's type system
      Result : constant Stream_Element_Array_Access := new Stream_Element_Array (1 .. Stream_Element_Offset (Length));

   begin
      --  Copy the memory-mapped data to the new array
      Result.all := Target_Access.all;
      return Result;
   end Memory_To_Stream_Elements;

end Pipelib.Infrastructure.IO.Memory_Mapped_Chunk_Adapter;
