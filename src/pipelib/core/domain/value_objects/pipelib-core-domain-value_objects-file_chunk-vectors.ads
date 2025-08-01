--  =============================================================================
--  Pipelib.Core.Domain.Value_Objects.File_Chunk.Vectors - File Chunk Vector
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Vector container for File_Chunk objects for batch processing.
--  =============================================================================

pragma Ada_2022;

with Ada.Containers.Vectors;

package Pipelib.Core.Domain.Value_Objects.File_Chunk.Vectors is

   --  Vector of file chunks for batch processing
   package File_Chunk_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => File_Chunk_Type);

   subtype File_Chunk_Vector is File_Chunk_Vectors.Vector;

end Pipelib.Core.Domain.Value_Objects.File_Chunk.Vectors;
