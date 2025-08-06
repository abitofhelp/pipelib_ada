--  =============================================================================
--  Pipelib.Core.Domain.Value_Objects.Chunk_Size - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

package body Pipelib.Core.Domain.Value_Objects.Chunk_Size is

   --  Constructor with validation
   function Create (Bytes : SI_Bytes_Type) return Chunk_Size_Type is
   begin
      return (Bytes => Bytes);
   end Create;

   --  Factory methods (expression functions for performance)
   function Default return Chunk_Size_Type
   is ((Bytes => DEFAULT_CHUNK_SIZE));

   function Min return Chunk_Size_Type
   is ((Bytes => MIN_CHUNK_SIZE));

   function Max return Chunk_Size_Type
   is ((Bytes => MAX_CHUNK_SIZE));

   --  Convenience constructors (expression functions for performance)
   function From_KB (KB : Natural) return Chunk_Size_Type
   is ((Bytes => SI_Bytes_Type (KB) * SI_KB));

   function From_MB (MB : Natural) return Chunk_Size_Type
   is ((Bytes => SI_Bytes_Type (MB) * SI_MB));

   --  Named size constructors (expression functions for performance)
   function Small return Chunk_Size_Type
   is ((Bytes => SIZE_1MB));

   function Medium return Chunk_Size_Type
   is ((Bytes => SIZE_16MB));

   function Large return Chunk_Size_Type
   is ((Bytes => SIZE_64MB));

   --  Adaptive chunk size based on total size
   function Adaptive_For_Size
     (Total_Size : SI_Bytes_Type) return Chunk_Size_Type
   is
      Chunk_Bytes : SI_Bytes_Type;
   begin
      if Total_Size < SI_Bytes_Type (10) * SI_MB then
         --  Small files: 256KB chunks
         Chunk_Bytes := SIZE_256KB;
      elsif Total_Size < SI_Bytes_Type (100) * SI_MB then
         --  Medium files: 4MB chunks
         Chunk_Bytes := SIZE_4MB;
      elsif Total_Size < SI_GB then
         --  Large files: 16MB chunks
         Chunk_Bytes := SIZE_16MB;
      elsif Total_Size < SI_Bytes_Type (10) * SI_GB then
         --  Very large files: 64MB chunks
         Chunk_Bytes := SIZE_64MB;
      else
         --  Huge files: 128MB chunks
         Chunk_Bytes := SIZE_128MB;
      end if;

      return (Bytes => Chunk_Bytes);
   end Adaptive_For_Size;

end Pipelib.Core.Domain.Value_Objects.Chunk_Size;
